
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
  suffix = c("test_abc_1")
) %>%
  mutate(#input_dir = str_c("../clinical_forecasting/historical_inputs/", run_date),
    input_dir = str_c("../clinical_forecasting/results/fc_", run_date, "_final/archive"),
    
    id = str_c("fc_", run_date, "_", suffix),
    fc_dir = str_c("../clinical_forecasting/results/", id),
    
    local_cases = str_c(input_dir, "/local_cases.csv")) %>%
  
  filter(file.exists(local_cases), dir.exists(fc_dir))


forecast_archives <- forecast_meta %>%
  expand_grid(state = c("NT", "ACT", "TAS")) %>% 
  mutate(archive_file = str_c(fc_dir, "/archive/", state, "_archive.qs")) %>%
  rowwise() %>% 
  mutate(archive = list(qs::qread(archive_file)))



source("../clinical_forecasting/R/make_result_quants.R")

forecast_trajs <- forecast_archives %>%
  
  mutate(results = list(map_dfr(
    archive$sim_results,
    function(x) {
      x$simulations %>%
        select(day, sample, count_ward = sim_ward, count_ICU = sim_ICU) %>%
        
        mutate(run_date = ymd(run_date),
               state = state,
               simulation_start = run_date - days(120)) %>%
        mutate(date = simulation_start + day) %>%
        
        pivot_longer(c("count_ward", "count_ICU"), names_prefix = "count_", names_to = "group", values_to = "count") %>%
        group_by(run_date, state) %>%
        filter(date >= max(date) - days(21))
  }, .id = "id_param"))) %>%
  select(id, results) %>% 
  ungroup() %>%
  unnest(results) %>% 
  pivot_wider(names_from = "sample",
              names_prefix = "sim_",
              values_from = "count") %>%
  
  select(c(id, suffix = id_param, run_date, state, group, date, starts_with("sim_"))) %>%
  
  group_by(state, id) %>% 
  mutate(case_forecast_start = min(date) - days(7)) %>%
  ungroup()


# forecast_quants <- forecast_trajs %>%
#   make_results_quants(probs = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))

source("R/get_performance_data.R")

performance_data <- get_performance_data(forecast_trajs, tar_read(occupancy_data_total))

retro_perf <- tar_read(retro_performance_data) %>%
  filter(suffix == "test_pf_b_baseline", state %in% c("TAS", "ACT", "NT")) %>%
  mutate(run_date = ymd(run_date))

performance_data_with_old <- bind_rows(performance_data, retro_perf) %>% 
  mutate(rate = seq(-10, -6, by = 0.5)[as.numeric(suffix)]) %>%
  mutate(rate = round(exp(rate) * 10000, 2),
         rate = replace_na(rate, 0))

performance_data_with_old %>%
  filter(group == "ward") %>% 
  group_by(state, group, rate) %>%
  summarise(error = mean(z_CRPS_forecast)) %>% 
  ggplot() +
  geom_line(aes(x = rate, y = error)) +
  geom_point(aes(x = rate, y = error)) +
  
  facet_wrap(~state, scales = "free_y") +
  
  xlab("Importation risk") +
  ylab("CRPS") +
  
  plot_theme






performance_data_with_old %>%
  filter(group == "ward") %>% 
  group_by(state, group, days_ahead, suffix) %>%
  get_performance() %>%
  
  ggplot() +
  geom_line(aes(x = days_ahead, y = CRPS_forecast, colour = suffix),
            position = position_dodge()) +
  
  plot_theme +
  
  facet_wrap(~state)









