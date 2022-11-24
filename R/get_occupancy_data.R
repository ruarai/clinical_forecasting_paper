

get_occupancy_data <- function() {
  source("../clinical_forecasting/R/get_public_occupancy.R")
  
  
  occupancy_data <- get_public_occupancy(today()) %>%
    filter(date <= ymd("2022-09-09"))
}