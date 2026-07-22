## This is master script that runs the analyses of alert outcomes to run the sensitivity to delay models

## directories
new_dpath <- "../../main_ecl_dec2024/outputs"
plot_path <- paste0(new_dpath, "/exploratory")
pre_path <- "../../main_ecl_dec2024/inputs/"
qmd_path <- "../../notebooks"
fig_path <- "../../notebooks/manuscript_figures"

## in case directory for figure creation does not exist yet
if (!dir.exists(fig_path)) {
  dir.create(fig_path, recursive = TRUE)
}
## settings/parameters

## general, for both utility calculation and BHM
transmission_setting <- "outbreak-prone" ## choose one of: "outbreak-prone" or "endemic"
pop_group <- "50Kto500K" ## the population group you want to run BHM for, options: "under_50K", "50Kto500K", "over_500K"
testing <- FALSE
if(testing){
  test_cntry <- "COD" ## ISO3 code for testing country
}
remove_censored <- FALSE ## whether to remove censored observations
retrigger_alerts <- FALSE ##whether to re-run the script that produces alerts and outcomes
rerun_time_censoring_analysis <- FALSE

## BHM specific
alert_no <- 8 ## choose the alert number you want to run the BHM for the effect of delay
model_choice <- "modelB2" ## the BHM to use to estimate the effect of delay
rerun_model <- FALSE
do_prior_check <- TRUE ## whether to run prior predictive check 
compare_epidemic_endemic_posteriors <- FALSE ## whether to compare alpha0 posteriors between transmission settings 
transform_posterior <- TRUE ## whether to convert posteriors to observation scale: exp(posterior) - 1 (weekly delay effect) 
run_model_selection <- FALSE ## whether to perform LOO + WAIC comparison 
constant <- 1 ## log ratio stabilizer when cases are 0
location_filter <- transmission_setting
rate_ratio_model <- FALSE

## building filename from parameter values
suffix <- paste(
  transmission_setting,
  paste0("pg", pop_group),
  paste0("alert", alert_no),
  model_choice,
  if (remove_censored) "rmCens" else "keepCens",
  if (constant) "const" else "noConst",
  if (testing) paste0("TEST_", test_cntry) else NULL,
  sep = "_"
)

suffix <- gsub("__+", "_", suffix)
log_ratio_models_file <- paste0("log_ratio_models_", suffix, ".html")

## optional: re-trigger alerts, alert groups, and get evaluation files
if (retrigger_alerts){
  message("re-triggering alert and outcomes")
  source("write_alert_outcomes2.R")
}

## render time censoring qmd to get evaluation period censoring estimate per alert/delay value
if(rerun_time_censoring_analysis){
  message("re-running alert time censoring analysis")
  quarto::quarto_render(
    input = file.path(qmd_path, "check_output_time_censoring.qmd")
  )
}

quarto::quarto_render(
  input = file.path(qmd_path, "log_ratio_models.qmd"),
  output_file = log_ratio_models_file,
  
  execute_params = list(
    model_choice = model_choice,
    rerun_model = rerun_model,
    alert_no = alert_no,
    pop_group = pop_group,
    location_filter = location_filter,
    run_model_selection = run_model_selection,
    constant = constant,
    do_prior_check = do_prior_check,
    compare_epidemic_endemic_posteriors = compare_epidemic_endemic_posteriors,
    transform_posterior = transform_posterior,
    rate_ratio_model = rate_ratio_model,
    remove_censored = remove_censored
  )
)

message("BHM model report rendered: ", log_ratio_models_file)
