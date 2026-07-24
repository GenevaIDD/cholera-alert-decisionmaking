## This is a master script that runs the analyses of alert outcomes to produce the utility score
## Modify the parameters and filenames to produce custom analyses

library(OutbreakExtractR)

## directories
new_dpath <- "../../main_ecl_dec2024/outputs"
plot_path <- paste0(new_dpath, "/exploratory")
pre_path <- "../../data/"
qmd_path <- "../../notebooks"
fig_path <- "../../notebooks/manuscript_figures"

## in case directory for figure creation does not exist yet
if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}

## settings/parameters
transmission_setting <- "outbreak-prone" ## choose one of: "outbreak-prone" or "endemic"
testing <- FALSE
if(testing){
  test_cntry <- "COD" ## ISO3 code for testing country
}
ag_nweeks <- 8    ## modify to set number of weeks for grouping temporally proximate alerts, default is 8 weeks
evalperiod <- 52  ## modify to set evaluation period length in weeks, default is 52
delayperiod <- 4  ## modify to set delay period (weeks) after alert for starting evaluation, default is 4
ag_nweeks_code <- paste0("nweeks", ag_nweeks)
evalperiod_code <- paste0("ew", evalperiod)
delayperiod_code <- paste0("dw", delayperiod)
remove_censored <- FALSE ## whether to remove censored observations
retrigger_alerts <- FALSE
incl_trend_alerts <- TRUE
impact_thresh <- 300 
use_filtered_linked <- TRUE 
## map to alert utility terminology
which_setting_alert <- dplyr::case_when(
  transmission_setting == "outbreak-prone" ~ "epidemic",
  transmission_setting == "endemic" ~ "endemic",
  TRUE ~ transmission_setting
)
use_iqr <- FALSE ## whether to use IQR instead of SD for some alert utility metric tables
ocv_status <- "all" ## choose one of: "all", "vaccinated", "unvaccinated"

## building filename from parameter values
suffix <- paste(
  transmission_setting,
  ag_nweeks_code,
  evalperiod_code,
  delayperiod_code,
  ocv_status,
  if (remove_censored) "rmCens" else "keepCens",
  if (testing) paste0("TEST_", test_cntry) else NULL,
  if (use_iqr) "iqr" else "sd",
  sep = "_"
)

suffix <- gsub("__+", "_", suffix)

alert_utility_file <- paste0("alert_utility_", suffix, ".html")

## starting pipeline

## optional: re-trigger alerts, alert groups, and get evaluation files
if (retrigger_alerts){
  source("write_alert_outcomes2.R")
}

## render time censoring qmd to get evaluation period censoring estimate per alert
if(remove_censored){
   quarto::quarto_render(
     input = file.path(qmd_path, "check_output_time_censoring.qmd")
   )
}

## rendering alert utility score
quarto::quarto_render(
  input = file.path(qmd_path, "alert_utility_score.qmd"),
  output_file = alert_utility_file,
  execute_params = list(
    which_setting = which_setting_alert,
    ag_nweeks_code = ag_nweeks_code,
    evalperiod_code = evalperiod_code,
    delayperiod_code = delayperiod_code,
    remove_censored = remove_censored,
    testing = testing,
    test_cntry = if (testing) test_cntry else NULL,
    new_dpath = new_dpath,
    plot_path = plot_path,
    pre_path = pre_path,
    incl_trend_alerts = incl_trend_alerts,
    impact_thresh = impact_thresh, 
    use_filtered_linked = use_filtered_linked,
    use_iqr = use_iqr,
    ocv_status = ocv_status
  )
)

message("Alert utility report rendered: ", alert_utility_file)
