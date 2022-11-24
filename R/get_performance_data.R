


get_performance_data <- function(
  trajs,
  occupancy_data
) {
  require(tsibble)
  require(fabletools)
  require(distributional)
  
  trajs <- tar_read(all_forecasts_data)$trajs
  occupancy_data <- tar_read(occupancy_data)
  
  
  occupancy_timeseries <- occupancy_data %>%
    filter(date >= ymd("2021-06-01"),
           #date <= ymd("2022-09-09")
           ) %>%
    as_tsibble(key = c("state", "group"), index = "date") 
  
  
  
  forecast_data_dist <- trajs %>%
    filter(date >= case_forecast_start + ddays(7),
           date < case_forecast_start + ddays(28)) %>%
    arrange(date) %>%
    pivot_longer(cols = starts_with("sim"),
                 names_prefix = "sim_",
                 names_to = "sample",
                 values_to = "count") %>%
    
    group_by(state, suffix, group, run_date, case_forecast_start, date) %>%
    summarise(forecast_count = distributional::dist_sample(list(count[!is.na(count)])),
              .groups = "drop")
  
  
  naive_forecasts <- trajs %>%
    distinct(run_date, state, group, case_forecast_start) %>%
    as_tibble() %>%
    pmap_dfr(
      function(state, group, run_date, case_forecast_start) {
        training_data <- occupancy_timeseries %>%
          filter(state == !!state, group == !!group) %>%
          filter(date < case_forecast_start + ddays(7)) %>%
          select(date, count) %>%
          as_tsibble()
        
        
        naive_forecast <- training_data %>%
          model(fable::RW(count)) %>%
          forecast(h = 21) %>%
          select(-c(.model)) %>%
          
          rename(naive_count = count,
                 naive_mean = .mean) %>%
          mutate(state = !!state, group = !!group, run_date = !!run_date, case_forecast_start = !!case_forecast_start,
                 model = "naive",
                 
                 .before = 1) %>%
          as_tibble()
        
        naive_forecast
      }
    )
  
  
  # via https://github.com/tidyverts/fabletools, Mitchell O'Hara Wild
  get_CRPS <- function(forecast_samples, true_count) {
    map2_dbl(
      forecast_samples, true_count, 
      function(d, y){
        x <- sort(unlist(d))
        m <- length(x)
        (2/m) * mean((x-y)*(m*(y<x) - seq_len(m) + 0.5))
      }
    )
  }
  
  
  performance_data <- naive_forecasts %>%
    left_join(
      forecast_data_dist,
      by = c("state", "group", "run_date", "case_forecast_start", "date")
    ) %>%
    left_join(
      occupancy_timeseries %>%
        rename(true_count = count),
      by = c("state", "group", "date")
    ) %>%
    
    mutate(
      days_ahead = as.numeric((date - case_forecast_start - ddays(7)) / ddays(1))
    ) %>%
    
    mutate(
      naive_count = dist_sample(generate(naive_count, 2000)),
      
      naive_median = median(naive_count),
      forecast_median = median(forecast_count)
    ) %>%
    
    mutate(
      z_CRPS_forecast = get_CRPS(forecast_count, true_count),
      z_CRPS_naive = get_CRPS(naive_count, true_count),
      
      AE_forecast = abs(forecast_median - true_count),
      AE_naive = abs(naive_median - true_count),
    ) %>%
    drop_na(forecast_count)
  
  
  performance_data
}


