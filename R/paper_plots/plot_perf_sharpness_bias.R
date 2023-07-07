


library(targets)
library(tidyverse)
library(lubridate)
library(distributional)

performance_data <- tar_read(paper_performance_data)

source("R/plots_common.R")


perf_bias <- performance_data %>%
  rowwise() %>% 
  mutate(p1 = sum(unlist(forecast_count) <= true_count) / length(unlist(forecast_count)),
         p2 = sum(unlist(forecast_count) <= true_count - 1) / length(unlist(forecast_count)),
         bias = 1 - p1 - p2,
         
         sharpness = median(abs(forecast_count - forecast_median)) / 0.675,
         log_sharpness = median(abs(log(forecast_count + 1) - log(forecast_median + 1))) / 0.675)



perf_bias %>%
  filter(group == "ward", state == "NSW") %>% 
  group_by(run_date) %>% 
  summarise(bias = mean(bias)) %>% 
  
  ggplot() +
  
  geom_point(aes(x = ymd(run_date), y = bias)) +
  
  
  scale_fill_distiller(type = "div", limits = c(-1, 1)) +
  
  coord_cartesian(ylim = c(-1, 1)) +
  
  
  plot_theme


perf_bias %>%
  
  group_by(group, days_ahead, state) %>%
  summarise(sharpness = mean(sharpness)) %>% 
  mutate(group = factor(group, c("ward", "ICU"), labels = c("ward" = "Ward", "ICU" = "ICU"))) %>% 
  
  ggplot() +
  geom_line(aes(x = days_ahead, y = sharpness, colour = state)) +
  
  ggokabeito::scale_colour_okabe_ito(name = "State/Territory") +
  
  xlab("Forecast horizon (days)") +
  ylab("Sharpness") +
  
  facet_wrap(~group, scales = "free_y") +
  coord_cartesian(ylim = c(0, NA), xlim = c(0, 21), expand = FALSE) +
  scale_x_continuous(breaks = c(0, 7, 14, 21)) +
  
  plot_theme +
  theme(panel.spacing.x = unit(1, "cm"))


ggsave(
  "results_paper/perf_sharpness.png",
  bg = "white",
  
  scale = 10 / 16,
  width = 12, height = 5
)

peak_tbl <- bind_rows(
  occupancy_data %>% 
    filter(date >= ymd("2022-06-01"), date <= ymd("2022-12-01")) %>%
    group_by(state, group) %>% 
    filter(count == max(count, na.rm = TRUE)) %>%
    slice(1),
  
  occupancy_data %>% 
    filter(date >= ymd("2022-03-22"), date < ymd("2022-06-01")) %>%
    group_by(state, group) %>% 
    filter(count == max(count, na.rm = TRUE)) %>%
    slice(1)
) 


p_bias_ward <- perf_bias %>%
  ungroup() %>% 
  filter(group == "ward") %>% 
  ggplot() +
  geom_line(aes(x = date, y = bias, group = run_date, colour = run_date)) +
  
  scale_colour_manual(values = rep(ggokabeito::palette_okabe_ito(c(1, 3, 5, 8)), times = 10)) +
  
  geom_hline(yintercept = -1, alpha = 0.3, linetype = "dashed") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = "dashed") +
  
  geom_vline(aes(xintercept = date),
             size = 0.6, alpha = 0.7,
             peak_tbl %>% filter(group == "ward")) +
  
  facet_wrap(~state, ncol = 2) +
  
  coord_cartesian(expand = FALSE) +
  
  plot_theme +
  
  ylab("Bias") + xlab("Date") +
  
  theme(legend.position = "none") +
  
  ggtitle("Forecast bias \u2012 ward forecasts") +
  theme(panel.grid.major = element_blank())

p_bias_ward

p_bias_ICU <- perf_bias %>%
  ungroup() %>% 
  filter(group == "ICU") %>% 
  ggplot() +
  geom_line(aes(x = date, y = bias, group = run_date, colour = run_date)) +
  
  scale_colour_manual(values = rep(ggokabeito::palette_okabe_ito(c(1, 3, 5, 8)), times = 10)) +
  
  geom_hline(yintercept = -1, alpha = 0.3, linetype = "dashed") +
  geom_hline(yintercept = 0, alpha = 0.5) +
  geom_hline(yintercept = 1, alpha = 0.3, linetype = "dashed") +
  
  geom_vline(aes(xintercept = date),
             size = 0.6, alpha = 0.7,
             peak_tbl %>% filter(group == "ICU")) +
  
  facet_wrap(~state, ncol = 2) +
  
  coord_cartesian(expand = FALSE) +
  
  plot_theme +
  
  ylab("Bias") + xlab("Date") +
  
  theme(legend.position = "none") +
  
  ggtitle("Forecast bias \u2012 ICU forecasts") +
  theme(panel.grid.major = element_blank())



cowplot::plot_grid(
  p_bias_ward, p_bias_ICU, ncol = 1, align = "v"
)


ggsave(
  "results_paper/perf_bias.png",
  
  dpi = get_dpi(width = 8),
  width = 8, height = 9,
  bg = "white"
)
