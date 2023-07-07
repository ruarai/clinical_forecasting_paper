

read_forecast_data <- function(forecast_meta) {
  
  local_cases <- forecast_meta %>%
    pull(input_dir) %>%
    str_c("/local_cases.csv") %>%
    map(read_csv, show_col_types = FALSE) %>%
    map(function(y) {
      y %>% rename_with(function(x) if_else(x == "completion_probability", "detection_probability", x))
    }) %>%
    `names<-`(forecast_meta$run_date) %>%
    bind_rows(.id = "run_date")
  
  forecast_start_dates <- local_cases %>%
    group_by(run_date, state) %>%
    filter(detection_probability > 0.95) %>%
    filter(date_onset == max(date_onset)) %>%
    select(run_date, state, case_forecast_start = date_onset) %>%
    
    distinct(run_date, state, case_forecast_start)
  
  
  
  
  trajectory_tbls <- forecast_meta$trajectory %>%
    map(fst::read_fst) %>%
    `names<-`(forecast_meta$id)
  
  require(progress)
  pb <- progress_bar$new(total = nrow(forecast_meta))
  
  all_data <- map(1:nrow(forecast_meta), function(i) {
    print(str_c("Loading forecast ", i, " out of ", nrow(forecast_meta)))
    
    traj <- forecast_meta$trajectory[[i]]
    name <- forecast_meta$id[[i]]
    
    traj_tbl <- fst::read_fst(traj)
    
    traj_wide <- traj_tbl %>%
      mutate(id = name) %>%
      select(c(id, state, group, sample, count, date)) %>%
      filter(sample < 2000) %>% 
      pivot_wider(names_from = "sample",
                  names_prefix = "sim_",
                  values_from = "count") %>%
      
      left_join(forecast_meta, by = "id") %>%
      
      left_join(forecast_start_dates, by = c("run_date", "state")) %>%
      
      filter(date >= case_forecast_start + ddays(7), date <= case_forecast_start + ddays(28))
    
    quants <- traj_wide %>%
      make_results_quants(probs = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))
    
    
    list("quants" = quants, "traj_wide" = traj_wide)
    
  })
  
  
  
  all_quants <- map_dfr(all_data, function(x) x$quants)
  all_trajs_wide <- map_dfr(all_data, function(x) x$traj_wide)
  
  list(
    "quants" = all_quants,
    "trajs" = all_trajs_wide
  )
}