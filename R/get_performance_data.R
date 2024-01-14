


get_performance_data <- function(
  trajs,
  occupancy_data
) {
  
  occupancy_timeseries <- occupancy_data %>%
    filter(date >= ymd("2022-01-01"),
           #date <= ymd("2022-09-09")
           ) %>%
    as_tsibble(key = c("state", "group"), index = "date") 
  
  
  forecast_data_dist <- trajs %>% 
    filter(date > case_forecast_start + ddays(7),
           date <= case_forecast_start + ddays(28)) %>%
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
        
        # 
        # naive_forecast <- training_data %>%
        #   model(fable::RW(count)) %>%
        #   forecast(h = 21) %>%
        #   select(-c(.model)) %>%
        #   
        #   rename(naive_count = count,
        #          naive_mean = .mean) %>%
        #   mutate(state = !!state, group = !!group, run_date = !!run_date, case_forecast_start = !!case_forecast_start,
        #          model = "naive",
        #          
        #          .before = 1) %>%
        #   as_tibble()
        
        obs_counts_dist <- distributional::dist_sample(list(training_data$count))
        
        
        naive_forecast <- expand_grid(
          date = seq(case_forecast_start + days(8), case_forecast_start + days(28), "days")
        ) %>%
          mutate(naive_count = obs_counts_dist,
                 naive_mean = mean(obs_counts_dist)) %>%
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
  
  # Custom function for distributional compatibility
  log_plus_one <- function(x) log(x + 1)
  
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
      days_ahead = as.numeric((date - case_forecast_start - 7))
    ) %>%
    
    mutate(
      #naive_count = dist_sample(generate(naive_count, 2000)),
      
      naive_median = median(naive_count),
      forecast_median = median(forecast_count)
    ) %>%
    
    mutate(
      z_CRPS_forecast = get_CRPS(forecast_count, true_count),
      #z_CRPS_naive = get_CRPS(naive_count, true_count),
      
      z_log_CRPS_forecast = get_CRPS(log_plus_one(forecast_count), log_plus_one(true_count)),
      #z_log_CRPS_naive = get_CRPS(log_plus_one(naive_count), log_plus_one(true_count)),
      
      AE_forecast = abs(forecast_median - true_count),
      #AE_naive = abs(naive_median - true_count),
      
      AE_log_forecast = abs(log_plus_one(forecast_median) - log_plus_one(true_count) ),
      #AE_log_naive = abs(log_plus_one(naive_median) - log_plus_one(true_count) ),
    ) %>%
    drop_na(forecast_count)
  
  # https://stackoverflow.com/a/70394780
  print_filtered_rows <- function(dataframe, ...) {
    df <- dataframe
    vars = as.list(substitute(list(...)))[-1L]
    for(arg in vars) {
      dataframe <- df
      dataframe_new <- dataframe %>% filter(!!arg)
      rows_filtered <- nrow(df) - nrow(dataframe_new)
      cat(sprintf('Filtered out %s rows using: %s\n', rows_filtered, deparse(arg)))
      df = dataframe_new
    }
    return(dataframe_new)
  }
  
  performance_data_filt <- performance_data %>%
    
    # Fix for ACT data missing on 15th and 17th April 2022
    print_filtered_rows(!is.na(true_count))
  
  performance_data_filt
}


