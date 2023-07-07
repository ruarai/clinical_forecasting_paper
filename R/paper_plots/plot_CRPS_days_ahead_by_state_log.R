

library(targets)
library(tidyverse)
library(lubridate)
library(distributional)

performance_data <- tar_read(paper_performance_data)

source("R/plots_common.R")



get_performance <- . %>%
  
  summarise(
    CRPS_naive = mean(z_log_CRPS_naive),
    CRPS_forecast = mean(z_log_CRPS_forecast),
    
    MAE_naive = mean(AE_naive),
    MAE_forecast = mean(AE_forecast),
    
    .groups = "drop"
  ) %>%
  mutate(CRPS_skill_score = (CRPS_naive - CRPS_forecast) / CRPS_naive,
         MAE_skill_score = (MAE_naive - MAE_forecast) / MAE_naive)

perf_days_ahead <- performance_data %>%
  
  group_by(group, state, date, run_date, days_ahead) %>% 
  
  get_performance()

perf_days_ahead_summ <- perf_days_ahead %>%
  
  group_by(group, state, days_ahead) %>%
  summarise(median_CRPS = median(CRPS_forecast))


perf_days_ahead_summ_total <- perf_days_ahead %>%
  
  group_by(group, state) %>%
  summarise(median_CRPS = median(CRPS_forecast)) 


p_ward_daysahead <- ggplot() +
  
  geom_hline(yintercept = 0, size = 1) +
  geom_vline(xintercept = 0, size = 1) +
  
  geom_line(aes(x = days_ahead + 1, y = CRPS_forecast, group = run_date),
            perf_days_ahead %>% filter(group == "ward"),
            size = 0.3, alpha = 0.2) +
  
  geom_point(aes(x = days_ahead + 1, y = median_CRPS),
            perf_days_ahead_summ %>% filter(group == "ward"),
            
            size = 8, pch = "-", stroke = 0.1,
            colour = ggokabeito::palette_okabe_ito(5)) +
  
  
  facet_wrap(~state, ncol = 4) +
  
  xlab(NULL) +
  
  ylab("CRPS") +
  
  scale_y_log10(expand = expansion(mult = c(0, 0.05))) +
  
  scale_x_continuous(breaks = c(0, 7, 14, 21),
                     expand = expansion(c(0, 0.05))) +
  
  
  coord_cartesian(ylim = c(0.01, 3.0)) +
  
  plot_theme +
  
  theme(panel.grid.major = element_blank())

p_ward_daysahead



p_ICU_daysahead <- ggplot() +
  
  geom_hline(yintercept = 0, size = 1) +
  geom_vline(xintercept = 0, size = 1) +
  
  geom_line(aes(x = days_ahead + 1, y = CRPS_forecast, group = run_date),
            perf_days_ahead %>% filter(group == "ICU"),
            size = 0.3) +
  
  geom_line(aes(x = days_ahead + 1, y = median_CRPS),
            perf_days_ahead_summ %>% filter(group == "ICU"),
            
            size = 1.2,
            colour = ggokabeito::palette_okabe_ito(2)) +
  
  
  facet_wrap(~state, ncol = 4) +
  
  xlab("Horizon (days ahead)") +
  
  ylab("CRPS") +
  
  scale_y_log10(expand = expansion(mult = c(0, 0.05))) +
  
  scale_x_continuous(breaks = c(0, 7, 14, 21),
                     expand = expansion(c(0, 0.05))) +
  
  
  coord_cartesian(ylim = c(0.01, 3.0)) +
  
  plot_theme +
  
  theme(panel.grid.major = element_blank())



p_ward_summ <- perf_days_ahead %>%
  filter(group == "ward") %>% 
  group_by(state) %>%
  mutate(mean_state = median(CRPS_forecast)) %>% 
  ungroup() %>%
  arrange(mean_state) %>% 
  mutate(state = fct_inorder(state)) %>% 
  ggplot() +
  ggdist::stat_histinterval(aes(x = CRPS_forecast, y = state),
                            show_interval = FALSE,
                            breaks = seq(-10, 10, by = 0.02),
                            fill = "grey30",
                            side = "top") +
  
  stat_summary(aes(x = CRPS_forecast, y = state), fun = median,
               size = 0.3,
               colour = ggokabeito::palette_okabe_ito(2)) +
  
  
  scale_x_log10() +
  
  coord_cartesian(xlim = c(0.01, 3.0)) +
  
  xlab("CRPS") +
  ylab(NULL) +
  
  plot_theme +
  theme(panel.grid.major.x = element_blank())

p_ward_summ

p_ICU_summ <- perf_days_ahead %>%
  filter(group == "ICU") %>% 
  group_by(state) %>%
  mutate(mean_state = median(CRPS_forecast)) %>% 
  ungroup() %>%
  arrange(mean_state) %>% 
  mutate(state = fct_inorder(state)) %>% 
  ggplot() +
  ggdist::stat_histinterval(aes(x = CRPS_forecast, y = state),
                            show_interval = FALSE,
                            breaks = seq(-10, 10, by = 0.02),
                            fill = "grey30",
                            side = "top") +
  
  stat_summary(aes(x = CRPS_forecast, y = state), fun = median,
               size = 0.3,
               colour = ggokabeito::palette_okabe_ito(2)) +
  
  
  scale_x_log10() +
  
  coord_cartesian(xlim = c(0.01, 3.0)) +
  
  xlab("CRPS") +
  ylab(NULL) +
  
  plot_theme +
  theme(panel.grid.major.x = element_blank())

p_ICU_summ


cowplot::plot_grid(
  p_ward_daysahead, p_ward_summ,
  p_ICU_daysahead, p_ICU_summ,
  
  ncol = 2, align = "hv", axis = "lrtb",
  rel_widths = c(3, 1)
)


ggsave(
  "results_paper/results_CRPS_ward_and_ICU_log.pdf",
  width = 8, height = 6
)



