

read_past_forecasts <- function() {
  
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
                 "2023-01-19", "2023-01-27"),
    suffix = "final"
  ) %>%
    mutate(
      id = str_c("fc_", run_date, "_", suffix),
      fc_dir = str_c("../clinical_forecasting/results/", id),
      
      input_dir = str_c(fc_dir, "/archive/"),
      trajectory = str_c(fc_dir, "/trajectories.fst")
    ) %>%
    
    filter(dir.exists(input_dir), dir.exists(fc_dir), file.exists(trajectory),
           
           file.exists(str_c(input_dir, "/local_cases.csv")))
  
  read_forecast_data(forecast_meta)
}
