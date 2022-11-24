



read_retro_forecasts <- function() {
  
  source("../clinical_forecasting/R/make_result_quants.R")
  
  
  forecast_meta <- expand_grid(
    run_date = c(  "2022-02-05",
                   "2022-02-11",
                   "2022-02-18",
                   "2022-02-25",
                   "2022-03-04",
                   "2022-03-11",
                   "2022-03-18",
                   "2022-03-24",
                   "2022-04-01",
                   "2022-04-08",
                   "2022-04-14",
                   "2022-04-22",
                   "2022-04-29",
                   "2022-05-04",
                   "2022-05-12",
                   "2022-05-19",
                   "2022-05-27",
                   "2022-06-03",
                   "2022-06-10",
                   "2022-06-17",
                   "2022-06-24",
                   "2022-07-01",
                   "2022-07-08",
                   "2022-07-15",
                   "2022-07-22",
                   "2022-07-28",
                   "2022-08-03"),
    suffix = c("retro3_neither", "retro3_knowntv", "retro3_knowncases", "retro3_knownboth")
  ) %>%
    mutate(#input_dir = str_c("../clinical_forecasting/historical_inputs/", run_date),
      input_dir = str_c("../clinical_forecasting/results/fc_", run_date, "_final/archive"),
      
      id = str_c("fc_", run_date, "_", suffix),
      fc_dir = str_c("../clinical_forecasting/results/", id),
      
      trajectory = str_c(fc_dir, "/trajectories.fst"),
      
      local_cases = str_c(input_dir, "/local_cases.csv")) %>%
    
    filter(file.exists(local_cases), dir.exists(fc_dir), file.exists(trajectory))
  
  
  
  read_forecast_data(forecast_meta)
}


