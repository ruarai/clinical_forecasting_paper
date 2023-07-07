
library(tidyverse)
library(targets)
library(lubridate)
paper_forecasts_data <- tar_read(paper_forecasts_data)
occupancy_data <- tar_read(occupancy_data)

forecast_quants <- paper_forecasts_data$quants
performance_data <- tar_read(paper_performance_data)



source("R/plots_common.R")



forecast_meta <- expand_grid(
  run_date = c("2022-03-18", "2022-03-24", "2022-04-01", "2022-04-08", "2022-04-14", "2022-04-22", 
               "2022-04-29", "2022-05-04", "2022-05-12", "2022-05-19", "2022-05-27",
               "2022-06-03", "2022-06-10", "2022-06-17", "2022-06-24", "2022-07-01",
               "2022-07-08", "2022-07-15", "2022-07-22", "2022-07-28", "2022-08-03"),
  suffix = "final"
) %>%
  mutate(
    id = str_c("fc_", run_date, "_", suffix),
    fc_dir = str_c("../clinical_forecasting/results/", id),
    
    input_dir = str_c(fc_dir, "/archive/"),
    trajectory = str_c(fc_dir, "/trajectories.fst"),
    ensemble = str_c(input_dir, "/ensemble.csv")
  ) %>%
  
  filter(dir.exists(input_dir), dir.exists(fc_dir), file.exists(trajectory),
         
         file.exists(str_c(input_dir, "/local_cases.csv")))


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

ensemble_spec <- cols(
  state = col_character(), .model = col_character(),
  date = col_date(), forecast_origin = col_date(),
  .default = col_double()
)



performance_data <- tar_read(paper_performance_data)

case_forecasts <- forecast_meta %>% 
  ungroup() %>%
  pmap(function(ensemble, run_date, ...) {
    print(ensemble)
    vroom::vroom(ensemble,
                 col_types = ensemble_spec) %>%
      mutate(run_date = !!run_date)
  }) %>%
  bind_rows()





case_forecasts_dist <- case_forecasts %>%
  left_join(forecast_start_dates, by = c("state", "run_date")) %>%
  arrange(date) %>%
  pivot_longer(cols = starts_with("sim"),
               names_prefix = "sim",
               names_to = "sample",
               values_to = "count") %>%
  
  group_by(state, date, case_forecast_start) %>%
  summarise(forecast_count = distributional::dist_sample(list(count[!is.na(count)])),
            .groups = "drop")


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


local_cases <- read_csv("../clinical_forecasting/data/local_cases_input_2023-03-02.csv")


# Custom function for distributional compatibility
log_plus_one <- function(x) log(x + 1)

case_performance <- case_forecasts_dist %>%
  mutate(forecast_median = median(forecast_count)) %>% 
  left_join(local_cases %>% select(state, date_onset, true_count = count), by = c("state", "date" = "date_onset")) %>%
  
  mutate(z_log_CRPS_forecast = get_CRPS(log_plus_one(forecast_count), log_plus_one(true_count)),
         AE_forecast = abs(forecast_median - true_count))



perf_bias <- performance_data %>%
  rowwise() %>% 
  mutate(p1 = sum(unlist(forecast_count) <= true_count) / length(unlist(forecast_count)),
         p2 = sum(unlist(forecast_count) <= true_count - 1) / length(unlist(forecast_count)),
         bias = 1 - p1 - p2,
         
         sharpness = median(abs(forecast_count - forecast_median)) / 0.675,
         log_sharpness = median(abs(log(forecast_count + 1) - log(forecast_median + 1))) / 0.675)



plot_data <- case_performance %>%
  select(state, date, case_forecast_start, log_CRPS_case = z_log_CRPS_forecast) %>% 
  
  #filter(state == "VIC") %>% 
  
  left_join(
    perf_bias %>%
      filter(group == "ward") %>%
      select(state, group, case_forecast_start, date, true_count, days_ahead, log_CRPS_ward = z_log_CRPS_forecast),
    by = c("state", "date", "case_forecast_start")
  ) %>%
  
  drop_na(log_CRPS_ward) %>%
  
  mutate(x = log_CRPS_ward - log_CRPS_case) %>%
  
  group_by(state, true_count, date) %>%
  summarise(x = mean(x),
            x = pmin(x, 0.4) %>% pmax(-0.4))

x_limit <- max(abs(plot_data$x))

p_timeseries <- plot_data %>% 
  
  ggplot() +
  
  geom_linerange(aes(x = date, ymin = 0, ymax = true_count, colour = x),
                 size = 0.8) +
  
  geom_step(aes(x = date, y = true_count)) +
  
  geom_hline(yintercept = 0, size = 0.8) +
  
  scale_colour_distiller(type = "div", palette = "RdBu",
                         name = "Difference in CRPS",
                         limits = c(-x_limit, x_limit)) +
  
  #scale_colour_continuous(trans = "log") +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                     labels = scales::label_comma(),
                     breaks = scales::breaks_extended(n = 4)) +
  
  facet_wrap(~state, scales = "free_y",
             ncol = 2) +
  
  ylab("Occupancy count") +
  xlab(NULL) +
  
  plot_theme +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank()) +
  
  ggtitle("B")





case_performance %>%
  select(state, date, case_forecast_start, log_CRPS_case = z_log_CRPS_forecast) %>% 
  
  left_join(
    performance_data %>%
      filter(group == "ward") %>%
      select(state, group, case_forecast_start, date, days_ahead, log_CRPS_ward = z_log_CRPS_forecast),
    by = c("state", "date", "case_forecast_start")
  ) %>%
  
  drop_na(log_CRPS_ward) %>%
  
  group_by(state, case_forecast_start, date, days_ahead) %>% 
  summarise(log_CRPS_ward = mean(log_CRPS_ward),
            log_CRPS_case = mean(log_CRPS_case),
            x = log_CRPS_ward - log_CRPS_case,
            x = pmin(x, x_limit),
            x = pmax(x, -x_limit),
            .groups = "drop") %>% 
  ungroup() %>%
  mutate(label = match(case_forecast_start, unique(case_forecast_start))) %>% 
  
  ggplot() +
  
  # geom_abline(aes(slope = 1, intercept = int, colour = x),
  #             tibble(int = seq(-1, 1, by = 0.05)) %>% mutate(x = pmin(int, x_limit), x = pmax(x, -x_limit)),
  #             size = 3) +
  
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  
  geom_hline(yintercept = 0, size = 0.8) +
  geom_vline(xintercept = 0, size = 0.8) +
  
  geom_point(aes(x = log_CRPS_case, y = log_CRPS_ward, group = case_forecast_start, colour = days_ahead),
             size = 0.4, stroke = 0.3) +
  
  #geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  
  scale_colour_distiller(type = "div", palette = "RdBu") +
  
  facet_wrap(~state, ncol = 2) +
  
  coord_fixed(xlim = c(0, 1.0), ylim = c(0, 1.0), expand = FALSE) +
  
  scale_x_continuous(breaks = c(0, 0.5, 1.0)) +
  scale_y_continuous(breaks = c(0, 0.5, 1.0)) +
  
  xlab("Case forecast CRPS") +
  ylab("Ward forecast CRPS") +
  
  plot_theme +
  
  theme(panel.grid.major = element_blank(),
        panel.spacing.x = unit(1, "cm"),
        legend.position = "none") +
  
  ggtitle("A")





fit_data <- case_performance %>%
  select(state, date, case_forecast_start, log_CRPS_case = z_log_CRPS_forecast) %>% 
  
  left_join(
    performance_data %>%
      filter(group == "ward") %>%
      select(state, group, case_forecast_start, date, days_ahead, log_CRPS_ward = z_log_CRPS_forecast),
    by = c("state", "date", "case_forecast_start")
  ) %>%
  mutate(state = factor(state, levels = unique(state))) %>%
  
  drop_na(log_CRPS_ward) %>%
  
  mutate(log_CRPS_ward = log(log_CRPS_ward),
         log_CRPS_case = log(log_CRPS_case))
