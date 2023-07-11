



read_retro_forecasts <- function() {
  
  source("../clinical_forecasting/R/make_result_quants.R")
  
  
  forecast_meta <- expand_grid(
    run_date = c("2022-03-18", "2022-03-11", "2022-02-25", "2022-02-22", "2022-02-18",
                 "2022-02-11", "2022-02-05",
                 "2022-03-24", "2022-04-01", "2022-04-08", "2022-04-14", "2022-04-22", 
                 "2022-04-29", "2022-05-04", "2022-05-12", "2022-05-19", "2022-05-27",
                 "2022-06-03", "2022-06-10", "2022-06-17", "2022-06-24", "2022-07-01",
                 "2022-07-08", "2022-07-15", "2022-07-22", "2022-07-28", "2022-08-03",
                 "2022-08-11", "2022-08-17", "2022-08-25", "2022-09-01", "2022-09-09",
                 "2022-09-16", "2022-09-29", "2022-10-06", "2022-10-14", "2022-10-21",
                 "2022-10-28", "2022-11-04", "2022-11-11", "2022-11-18", "2022-11-25",
                 "2022-12-02", "2022-12-09", "2022-12-15", "2023-01-06", "2023-01-12",
                 "2023-01-19", "2023-01-27", "2023-02-02", "2023-02-09", "2023-02-16",
                 "2023-02-23", "2023-03-03", "2023-03-09", "2023-03-17", "2023-03-23",
                 "2023-03-31", "2023-04-06", "2023-04-14", "2023-04-20", "2023-04-27", 
                 "2023-05-05", "2023-05-12", "2023-05-19"),
    suffix = c("test_pf_b_2", "test_pf_b_baseline")
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


