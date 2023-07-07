library(targets)
library(tarchetypes)

options(tidyverse.quiet = TRUE)
tar_option_set(packages = c("tidyverse", "lubridate"))


source("R/read_forecast_data.R")
source("R/read_past_forecasts.R")
source("R/read_retro_forecasts.R")

source("R/get_occupancy_data.R")

source("R/get_performance_data.R")

source("R/plot_forecasts.R")
source("R/plot_supplementary_all_forecasts.R")

list(
  tar_target(
    past_forecasts_data,
    read_past_forecasts(),
    format = "qs"
  ),
  
  tar_target(
    retro_forecasts_data,
    read_retro_forecasts(),
    format = "qs"
  ),
  
  tar_target(
    all_forecasts_data,
    list(
      "quants" = bind_rows(past_forecasts_data$quants, retro_forecasts_data$quants),
      "trajs" = bind_rows(past_forecasts_data$trajs, retro_forecasts_data$trajs)
    ),
    format = "qs"
  ),
  
  tar_target(
    paper_forecasts_data,
    list(
      "quants" = past_forecasts_data$quants %>% 
        filter(run_date < ymd("2022-08-10"),
               run_date >= ymd("2022-03-15"),
               suffix == "final"),
      "trajs" = past_forecasts_data$trajs %>% 
        filter(run_date < ymd("2022-08-10"),
               run_date >= ymd("2022-03-15"),
               suffix == "final")
    ),
    format = "qs"
  ),
  
  tar_target(
    occupancy_data,
    read_csv("data/occupancy_compiled_Sep2022.csv", show_col_types = FALSE)
  ),
  
  tar_target(
    occupancy_data_total,
    read_csv("../clinical_forecasting/data/occupancy/compiled/occupancy_compiled_2023-05-26.csv")
  ),

  
  tar_target(
    performance_data,
    
    get_performance_data(
      all_forecasts_data$trajs,
      occupancy_data_total
    ),
    
    format = "qs"
  ),
  
  
  tar_target(
    paper_performance_data,
    
    get_performance_data(
      paper_forecasts_data$trajs,
      occupancy_data
    ),
    
    format = "qs"
  ),
  
  
  tar_target(
    retro_performance_data,
    
    get_performance_data(
      retro_forecasts_data$trajs,
      occupancy_data_total
    ),
    
    format = "qs"
  )
  
  
)