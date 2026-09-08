## This script links outbreaks to alert groups and produces an intermediate exploratory dataset

## libraries
library(tidyverse)

## settings
timeseries_plot <- FALSE ## whether to create a time series of alert groups and outbreaks
filtered_linkages <- FALSE ## whether to filter linkages using to keep only the overlapping alerts and the manual inspection suggestions (for 'closest before' alerts)
if (filtered_linkages){
  removed_overlapping_alert_ids <- c() ##fill in overlapping alert ids to filter linkages, manually identified by exploring time series
}


## load data
alert_groups <- readRDS(here::here("data", "alert_groups_nweeks8.rds"))
#time_series_outbreak_extraction2 <- readRDS(here::here("data", "time_series_outbreak_extraction.rds"))
time_series_outbreak_extraction <- arrow::read_parquet(here::here("data", "Public_outbreak_dataset.parquet")) %>%
  rename(location = location_name) ##temp testing w public dataset

####################  Processing #################### 

# outbreaks <- time_series_outbreak_extraction %>%
  # group_by(outbreak_UID) %>%
  # summarize(
  #   location = first(location),
  #   total_sCh = sum(sCh),
  #   population = mean(pop), ## in case the outbreak spans multiple years, take the average population
  #   outbreak_duration = as.numeric(sum(date_range) / 7), 
  #   outbreak_start = min(TL),
  #   outbreak_end = max(TR),
  #   .groups = "drop"
  # )

##testing
outbreaks <- time_series_outbreak_extraction %>%
  group_by(outbreak_UID) %>%
  summarize(
    location = first(location),
    total_sCh = sum(sCh),
    population = mean(pop), ## in case the outbreak spans multiple years, take the average population
    outbreak_start = min(TL),
    outbreak_end = max(TR),
    outbreak_duration = 7, 
    .groups = "drop"
  )

alert_numbers <- tibble(alert_number = 1:24)

## cross join to get one row per alert number per outbreak_UID
outbreaks <- outbreaks %>%
  tidyr::crossing(alert_numbers)

## link outbreaks to alerts and determine linkage type (overlap, closest before, closest after)
outbreaks_alerts_join <- outbreaks %>%
  left_join(alert_groups, by = c("location", "alert_number"), relationship = "many-to-many") %>%
  mutate(
    is_triggered = if_else(is.na(TL_first_alert), FALSE, TRUE),
    linkage = case_when(
      is.na(TL_first_alert) ~ NA_character_,
      TL_first_alert <= outbreak_end & TL_last_alert >= outbreak_start ~ "overlap",
      TL_last_alert < outbreak_start ~ "closest before",
      TL_first_alert > outbreak_end ~ "closest after",
      TRUE ~ NA_character_  
    )
  )

## separate by linkage type and select earliest/latest accordingly
## for "closest before" we select the alert_id with the latest TL_first_alert
closest_before <- outbreaks_alerts_join %>%
  filter(linkage == "closest before") %>%
  group_by(outbreak_UID, alert_number, linkage) %>%
  slice_max(order_by = TL_first_alert, with_ties = FALSE) %>%
  ungroup()

## for "overlap" and closest after" we select the alert_id with the earliest TL_first_alert
overlap_closest_after <- outbreaks_alerts_join %>%
  filter(linkage %in% c("overlap", "closest after")) %>%
  group_by(outbreak_UID, alert_number, linkage) %>%
  slice_min(order_by = TL_first_alert, with_ties = FALSE) %>%
  ungroup()

## add the untriggered alert numbers back to the main dataset
untriggered <- outbreaks_alerts_join %>%
  filter(is_triggered == FALSE)

## combining filtered cases and untriggered alert numbers
outbreaks_alerts_join <- bind_rows(closest_before, overlap_closest_after, untriggered) %>%
  rename(
    alert_group_start = TL_first_alert,
    alert_group_end = TL_last_alert
  ) %>%
  select(-c(alert_type, alert_count))

## save intermediate exploratory dataset
saveRDS(outbreaks_alerts_join, here::here("data", "linked_outbreaks_alert_groups_nweeks8.rds"))

#################### Plotting time series ####################

if (timeseries_plot){
  ## directories
  plot_dir <- here::here("notebooks", "manuscript_figures")  
  
  ## load time series data
  time_series <- readRDS(here::here("data", "time_series_preoutbreak_extraction_public_apr_2025.rds"))  
  
  ## add country to outbreaks alerts join
  outbreak_data <- outbreaks_alerts_join %>%
    mutate(country = str_split_fixed(location, "::", 3)[, 2])
  
  ## function to generate alert plots
  alert_plot_function <- function(time_data, outbreak_data, loc) {
    single_loc_ts <- filter(time_data, location == loc)  ## filter time series data for the given location
    single_loc_groups <- filter(outbreak_data, location == loc)  ## filter intermediate dataset for the given location
    
    
    ## ensure we only plot locations with cases
    if (nrow(single_loc_ts) == 0 || sum(single_loc_ts$sCh, na.rm = TRUE) == 0) {
      return(NULL)  ## return NULL if there are no cases
    }
    
    p <- ggplot(single_loc_ts) +
      geom_bar(aes(x = TL, y = sCh), stat = 'identity', color = "black") +  ## bar plot for time series data
      
      ## shaded outbreak duration
      geom_rect(
        data = single_loc_groups,
        aes(xmin = outbreak_start, xmax = outbreak_end, ymin = -Inf, ymax = Inf),
        fill = "grey50", alpha = 0.2, inherit.aes = FALSE
      ) +
      
      ## vertical solid line for alert group start
      geom_vline(data = single_loc_groups, aes(xintercept = alert_group_start, color = "Start of a new alert group"),
                 linetype = "solid", alpha = 0.7) +
      
      ## vertical dashed line for alert group end
      geom_vline(data = single_loc_groups, aes(xintercept = alert_group_end, color = "End of alert group"),
                 linetype = "dashed", alpha = 0.7) +
      
      ## show all alert numbers in the same page
      facet_wrap(vars(alert_number), nrow = 24) +
      
      ## legend for alert group line
      scale_color_manual(name = " ", values = c("Start of a new alert group" = "blue", "End of alert group" = "blue")) +
      
      
      ggtitle(paste("Location:", loc)) +  
      labs(x = 'Week', y = "Suspected Cholera cases") +  
      theme_bw() +  
      theme(axis.title = element_text(size = 20),  
            axis.text = element_text(size = 15),
            legend.title = element_text(size = 15),  
            legend.text = element_text(size = 12))  
    
    return(p)  
  }
  
  ## save plots in separate PDFs for each country
  save_alert_pdfs <- function(time_series, outbreak_data, plot_dir) {
    countries <- unique(outbreak_data$country)  
    
    for (country in countries) {  
      pdf_file <- file.path(plot_dir, paste0(country,"_linked_outbreaks_alert_groups.pdf"))  
      pdf(pdf_file, width = 8.5, height = 24)  
      
      locations <- unique(filter(outbreak_data, country == !!country)$location)
      for (loc in locations) {  
        p <- alert_plot_function(time_series, outbreak_data, loc)  
        if (!is.null(p)) {  ## only print plots that are not NULL (i.e., locations with cases)
          print(p)  
        }
      }
      
      dev.off()  
      print(paste("Saved:", pdf_file))  
      
    }
  }
  
  ## generate and save plots
  save_alert_pdfs(time_series, outbreak_data, plot_dir) 
}

#################### Filter outbreak alert linkages ####################

if (filtered_linkages){
  
  ## get the manually selected "closest before" alerts that we want to keep
  selected_closest_before <- readxl::read_excel(here::here("data", "keep_closest_before.xlsx")) %>%
    mutate(alert_id = str_trim(iconv(as.character(alert_id), to = "ASCII//TRANSLIT")),
           outbreak_UID = str_trim(iconv(as.character(outbreak_UID), to = "ASCII//TRANSLIT")),
           location = str_trim(iconv(as.character(location), to = "ASCII//TRANSLIT"))) %>%
    mutate(linkage = "closest before")
  
  filtered_closest_before <- outbreaks_alerts_join %>%
    right_join(selected_closest_before, by = c("alert_id", "outbreak_UID")) %>%
    select(-location.y, -linkage.y) %>%
    rename(location = location.x, 
           linkage = linkage.x)
  
  ## get all the overlaps (if there are multiple overlapping alerts for the same outbreak_UID and alert number combination keep the earliest)
  only_overlaps <- outbreaks_alerts_join %>%
    filter(linkage == "overlap") %>%
    group_by(alert_number, outbreak_UID) %>%
    slice_min(order_by = alert_group_start, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  ## get all the cases where an alert number was not triggered for each outbreak
  not_triggered <- outbreaks_alerts_join %>%
    filter(is_triggered == FALSE)
  
  ## combine into a single object
  selected_linkages <- dplyr::bind_rows(filtered_closest_before, only_overlaps, not_triggered)
  
  ## remove the overlapping alerts where based on manual inspection we decided to keep the closest before instead of the overlap alerts (outbreak_UID: AFR::COD::Haut-Katanga::Kilwa Health District-2016-11-07-2017-02-05)
  selected_linkages <- selected_linkages %>%
    filter(!alert_id %in% removed_overlapping_alert_ids)
  
  ## add in rows for each alert number that was filtered out per outbreak
  
  ## columns to be filled in from same outbreak_UID
  context_cols <- c("location", "total_sCh", "population", "outbreak_duration", "outbreak_start", "outbreak_end")
  
  ## all outbreak_UID x alert_number combinations
  all_combinations <- selected_linkages %>%
    distinct(outbreak_UID) %>%
    expand(outbreak_UID, alert_number = 1:24)
  
  ## join with the selected linkages dataset
  full_data <- all_combinations %>%
    left_join(selected_linkages, by = c("outbreak_UID", "alert_number"))
  
  # identify which combinations are missing (these will be new rows that are added in)
  new_rows <- full_data %>%
    anti_join(selected_linkages, by = c("outbreak_UID", "alert_number")) %>%
    mutate(is_triggered = FALSE) %>%
    select(-any_of(context_cols))
  
  # add outbreak info to new rows
  outbreak_info <- selected_linkages %>%
    select(outbreak_UID, all_of(context_cols)) %>%
    distinct() %>%
    group_by(outbreak_UID) %>%
    slice(1) %>%
    ungroup()
  
  new_rows_filled <- new_rows %>%
    left_join(outbreak_info, by = "outbreak_UID")
  
  ## add new rows to the full dataset
  selected_linkages <- bind_rows(selected_linkages, new_rows_filled) %>%
    arrange(outbreak_UID, alert_number) %>%
    distinct(outbreak_UID, alert_number, .keep_all = TRUE)
  
  #### check for duplicates or missing alert numbers ####
  
  ## check if each outbreak_UID has all 24 unique alert_numbers (1–24)
  check_alerts <- selected_linkages %>%
    group_by(outbreak_UID) %>%
    summarise(
      n_alerts = n_distinct(alert_number),
      has_all_alerts = all(1:24 %in% alert_number)
    )
  
  ## outbreaks with missing alert_numbers
  problem_outbreaks_missing <- check_alerts %>%
    filter(n_alerts != 24 | !has_all_alerts)
  
  ## check for duplicates per outbreak_UID + alert_number combo
  duplicates <- selected_linkages %>%
    group_by(outbreak_UID, alert_number) %>%
    filter(n() > 1)
  
  print("Outbreaks missing alert numbers:")
  print(problem_outbreaks_missing)
  
  print("Duplicate rows by outbreak_UID and alert_number:")
  print(duplicates)
  
  ## final check: outbreak info columns have no missing values
  
  missing_info <- selected_linkages %>%
    filter(if_any(all_of(context_cols), is.na))
  
  if (nrow(missing_info) > 0) {
    warning("There are rows with missing outbreak information columns!")
    print(missing_info %>% select(outbreak_UID, alert_number, all_of(context_cols)))
  } else {
    message("All outbreak information columns are complete with no missing values.")
  }
  
  ## save file
  saveRDS(selected_linkages, here::here("data", "filtered_linked_outbreaks_alert_groups_nweeks8.rds"))
  
}