## The purpose of this script is to trigger alerts and write alert outcomes
## run from repository root
library(OutbreakExtractR)
library(tidyverse)
library(sf)

## how are alerts defined? 
## alert1: alert week always exceeds mean of last 4 weeks of cases, thus indicating an increase in cases relative to the past month
## alert2: exceeds mean of last 4 weeks of cases in alert week and at least one other time within the past month
## alert3: exceeds mean of last 4 weeks of cases in alert week and at least two other times within the past month
## alert4-10: 3 consecutive weeks with at least 2, 5, 10, 25, 50, 100, 250 sCh each
## alert11-17: at least 5, 10, 25, 50, 100, 500, 1000 sCh cumulatively in past 3 weeks

testing <- FALSE ## process the testing dataset or full evaluation of outbreaks
test_cntry <- "COD"
write_alerts_only <- FALSE

## what is the outcome evaluation period
delays <- seq(from = 0, to = 24, by = 1) ## evaluate starting x weeks after alert; assumption on how long it takes to deliver and administer vaccine
evalperiod <- 52 ## evaluate for 52 weeks (1 year) after alert date + delay; period across which the alert outcome will be evaluated
nweeks_consec <- 8 ## number of consecutive weeks with no alert to refresh grouping of alerts

alert_columns <- paste0("alert", 1:24) ## unique alert columns

## change orig_dpath to the location of the preoutbreak extractions
## change new_dpath to the location where alert and outcome summary data are stored
orig_dpath <- "C:/Users/chalam/Documents/outbreaks_project/main_ecl_dec2024/inputs"
new_dpath <- "C:/Users/chalam/Documents/outbreaks_project/main_ecl_dec2024/outputs"

########## Write Alerts ########## 

if(testing & !file.exists(file.path(new_dpath, paste0("alerts_", test_cntry, ".rds")))){
  print("Processing alerts for testing data")
  
  ct_clean_export <- readRDS(file.path(orig_dpath, "time_series_preoutbreak_extraction.rds"))  ## includes obs without location periods
  ct_clean_export <- dplyr::filter(ct_clean_export, country == test_cntry)
  
  ## averaging duplicates with existing OutbreakExtractR function
  if(any(duplicated(ct_clean_export))){
    ct_clean_export <- OutbreakExtractR::average_duplicate_observations(ct_clean_export)
  }
  
  ## trigger alerts
  ct <- OutbreakExtractR::trigger_alert(ct_clean_export)
  
  ## add unique alert ids
  ct <- ct %>% OutbreakExtractR::add_unique_alert_ids()
  
  ## make alerts file more parsimonious and 'long'
  ct <- ct %>% OutbreakExtractR::format_alerts() 
  
  ## trigger rate alerts, add unique alert ids, and make them more parsimonious and 'long'
  ct_rate <- OutbreakExtractR::trigger_alert_rate(ct_clean_export) %>%
    OutbreakExtractR::add_unique_alert_ids()%>%
    OutbreakExtractR::format_alerts() 
  
  ## merge alerts and rate alerts
  ct <- rbind(ct, ct_rate)
  
  print("Completed processing alerts for testing data")
  readr::write_rds(ct, file.path(new_dpath, paste0("alerts_", test_cntry, ".rds"))) ## write alerts to file 
  #rm(ct_clean_export)

} else if(testing & file.exists(file.path(new_dpath, paste0("alerts_", test_cntry, ".rds")))){
  ct <- readr::read_rds(file.path(new_dpath, paste0("alerts_", test_cntry, ".rds")))
  ct_clean_export <- readRDS(file.path(orig_dpath, "time_series_preoutbreak_extraction.rds"))  ## needed to calculate cases in evaluation period
} else if(!testing & !file.exists(file.path(new_dpath, "alerts.rds"))){
  print("Processing alerts for all data")
  
  ct_clean_export <- readRDS(file.path(orig_dpath, "time_series_preoutbreak_extraction.rds"))  ## includes obs without location periods
  
  ## averaging duplicates with existing OutbreakExtractR function
  if(any(duplicated(ct_clean_export))){
    ct_clean_export <- OutbreakExtractR::average_duplicate_observations(ct_clean_export)
  }
  
  ## trigger alerts
  ct <- OutbreakExtractR::trigger_alert(ct_clean_export)

  ## add unique alert ids
  ct <- ct %>% OutbreakExtractR::add_unique_alert_ids()
  
  ## make alerts file more parsimonious and 'long'
  ct <- ct %>% OutbreakExtractR::format_alerts()
  
  ## trigger rate alerts, add unique alert ids, and make them more parsimonious and 'long'
  ct_rate <- OutbreakExtractR::trigger_alert_rate(ct_clean_export) %>%
    OutbreakExtractR::add_unique_alert_ids()%>%
    OutbreakExtractR::format_alerts() 
  
  ## merge alerts and rate alerts
  ct <- rbind(ct, ct_rate)
  
  print("Completed processing alerts for all data")
  readr::write_rds(ct, file.path(new_dpath, "alerts.rds")) ## write alerts to file
  #rm(ct_clean_export)

} else{
  ct <- readr::read_rds(file.path(new_dpath, "alerts.rds"))
  ct_clean_export <- readRDS(file.path(orig_dpath, "time_series_preoutbreak_extraction.rds"))  ## needed to calculate cases in evaluation period
}

########## Write Alert Groups ########## 

if(testing & !file.exists(file.path(new_dpath, paste0("alert_groups_nweeks", nweeks_consec,"_", test_cntry, ".rds")))){
  print("Processing alert groups for testing data")
  print("--- Creating alert groups for testing data")
  ct2 <- OutbreakExtractR::create_alert_groups2(ct, nweeks = nweeks_consec) ## Jan 2025 new create alert groups function to work with new alerts file structure
  rm(ct)
  readr::write_rds(ct2, file.path(new_dpath, paste0("alert_groups_nweeks", nweeks_consec,"_", test_cntry, ".rds")))
  print("Completed processing alert groups for testing data")

  
} else if(testing & file.exists(file.path(new_dpath, paste0("alert_groups_nweeks", nweeks_consec,"_", test_cntry, ".rds")))){
  ct2 <- readr::read_rds(file.path(new_dpath, paste0("alert_groups_nweeks", nweeks_consec,"_", test_cntry, ".rds")))
} else if(!testing & !file.exists(file.path(new_dpath, paste0("alert_groups_nweeks", nweeks_consec, ".rds")))){
  
  print("Processing alert groups for all data")
  print("--- Creating alert groups for all data")
  ct2 <- OutbreakExtractR::create_alert_groups2(ct, nweeks = nweeks_consec) ## Jan 2025 new create alert groups function to work with new alerts file structure
  rm(ct)
  readr::write_rds(ct2, file.path(new_dpath, paste0("alert_groups_nweeks", nweeks_consec, ".rds")))
  print("Completed processing alert groups for all data")
  
} else{
  ct2 <- readr::read_rds(file.path(new_dpath, paste0("alert_groups_nweeks", nweeks_consec, ".rds")))
}

if(write_alerts_only){
  stop("You selected only to write alerts only. Change setting for write_alerts_only if you wish to write outcomes as well.")
}

###### Outcome processing for simplified pipeline ######

## loop through each delay value
for (delay in delays) {
  
  print(paste("Processing delay = ", delay))
  start_time <- Sys.time() ## start timer
  ## count number of cases in the evaluation period for every alert group
  evaluation_cases <- OutbreakExtractR::calculate_cases(ct2, preoutbreak_ts = ct_clean_export, 
                                                        delay_period = delay, 
                                                        evaluation_duration = evalperiod)
  ## write evaluation period cases per alert group to file
  print(paste("Writing evaluation for delay = ", delay))
  output_filename <- if (testing) {
    file.path(new_dpath, paste0("evaluation_cases_dw", delay, "_ew", evalperiod, "_", test_cntry, ".rds"))
  } else {
    file.path(new_dpath, paste0("evaluation_cases_dw", delay, "_ew", evalperiod, ".rds"))
  }
  
  readr::write_rds(evaluation_cases, output_filename)
  
  end_time <- Sys.time()  ## end timer
  print(paste("Processing delay =", delay, "took", round(difftime(end_time, start_time, units = "secs"), 2), "seconds"))
}