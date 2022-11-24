

library(targets)
library(tidyverse)
library(lubridate)
library(distributional)

performance_data <- tar_read(performance_data)


source("R/plots_common.R")

get_performance <- . %>%
  
  summarise(
    CRPS_naive = mean(z_CRPS_naive),
    CRPS_ours = mean(z_CRPS_forecast),
    
    MAE_naive = mean(AE_naive),
    MAE_forecast = mean(AE_forecast),
    
    .groups = "drop"
  ) %>%
  mutate(CRPS_skill_score = (CRPS_naive - CRPS_ours) / CRPS_naive,
         MAE_skill_score = (MAE_naive - MAE_forecast) / MAE_naive,
         
         relative_CRPS = CRPS_ours / CRPS_naive) %>%
  
  mutate(group = if_else(group == "ward", "Ward", group),
         group = factor(group, c("Ward", "ICU")))

plot_data_days_ahead <- performance_data %>%
  filter(suffix == "final") %>%
  drop_na(true_count) %>% 
  
  filter(#run_date < ymd("2022-09-01"),
         run_date >= ymd("2022-03-15")) %>%
  
  group_by(state, group, days_ahead) %>%
  get_performance()


ggplot(plot_data_days_ahead) +
  geom_line(aes(x = days_ahead + 1, y = CRPS_skill_score, colour = state)) +
  
  coord_cartesian(ylim = c(-1, 1)) +
  
  facet_wrap(~group) +
  
  plot_theme +
  
  xlab("Days ahead") + ylab("Skill-score (CRPS)") +
  
  geom_hline(yintercept = 0, colour = 'grey20', linetype = 'dashed', size = 0.5) +
  
  geom_hline(yintercept = 1, colour = 'grey20', linetype = 'solid', size = 0.5) +
  geom_vline(xintercept = 0, colour = 'grey40', size = 0.9) +
  
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  ggokabeito::scale_colour_okabe_ito(name = "Jurisdiction") +
  
  theme(legend.position = "bottom") + 
  
  guides(colour = guide_legend(nrow = 1, byrow = TRUE))

ggsave(
  str_c("results/past_forecast_performance_days_ahead.png"),

  units = "cm",
  width = 18,
  height = 9,
  bg = "white"
)



trans_y <- function(y) - log(1 - y)
break_points <- c(-20, -3, 0, 0.75, 0.95)
minor_break_points <- c(-10, -1, 0.5, 0.9)

performance_data %>% 
  filter(group == "ward",
         suffix == "final",
         days_ahead >= 0) %>%
  
  mutate(
    y = ((z_CRPS_naive - z_CRPS_forecast) / z_CRPS_naive),
    y = trans_y(y),
    is_pos = if_else(y > 0, "pos", "neg"),
    
    run_date = as_date(run_date)
  ) %>%
  drop_na(y) %>%
  
  ggplot() +
  geom_linerange(aes(x = date, ymin = 0, ymax = y, colour = is_pos),
                 alpha = 0.4,
                 size = 0.4) +
  geom_point(aes(x = date, y = y, colour = is_pos),
             alpha = 0.4,
             size = 0.2) +
  
  facet_wrap(~state, ncol = 2) +
  
  plot_theme +
  
  scale_x_date(
    date_breaks = "months",
    labels = scales::label_date_short(format = c("%Y", "%b"))
  ) +
  
  xlab(NULL) + ylab("Skill score")+
  
  scale_colour_manual(values = paletteer::paletteer_d("LaCroixColoR::paired")[c(1, 3)]) +
  
  scale_y_continuous(
    breaks = trans_y(break_points),
    minor_breaks = trans_y(minor_break_points),
    labels = break_points,
    limits = trans_y(range(break_points))
  ) +
  
  
  geom_hline(yintercept = trans_y(0), colour = 'grey20', linetype = 'solid', size = 0.7) +
  
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 9),
    #panel.grid.minor.y = element_line(colour = "grey80", linetype = "dotted", size = 0.3)
  )


ggsave(
  str_c("results/past_forecast_performance_daily.png"),
  
  units = "cm",
  width = 18,
  height = 12,
  bg = "white"
)







coverage <- performance_data %>%
  filter(suffix == "final") %>%
  
  filter(run_date < ymd("2022-09-01"),
         run_date >= ymd("2022-03-15")) %>%
  
  expand_grid(
    quant = c(0, 0.01, seq(0.05, 0.95, by = 0.05), 0.99, 1)
  ) %>%
  
  rowwise() %>%
  
  mutate(upper = quantile(forecast_count, 0.5 + quant / 2),
         lower = quantile(forecast_count, 0.5 - quant / 2)) %>%
  ungroup() %>%
  mutate(within = true_count >= lower & true_count <= upper) %>% 
  
  group_by(state, group, quant) %>%
  summarise(within = sum(within) / n())


coverage %>%
  
  ggplot() +
  
  geom_line(aes(x = quant, y = within, colour = group, group = group)) +
  
  facet_wrap(~state, ncol = 4) +
  
  annotate(geom = "segment", x = 0, xend = 1, y = 0, yend = 1,
           linetype = 'dashed') +
  
  geom_hline(yintercept = 0, colour = 'grey20', size = 0.6) +
  geom_vline(xintercept = 0, colour = 'grey20', size = 0.6) +
  
  coord_fixed(ylim = c(0,1), xlim = c(0,1)) +
  
  xlab("Quantile") + ylab("Empirical coverage") +
  
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  
  scale_colour_manual(values = c("ward" = ward_base_colour, "ICU" = ICU_base_colour),
                      labels = c("ward" = "Ward", "ICU" = "ICU"),
                      name = NULL) +
  
  plot_theme +
  
  theme(panel.spacing.x = unit(1, "cm"),
        legend.position = "bottom")



quant_mapping <- coverage %>%
  mutate(within = if_else(quant == 0, 0, within)) %>%
  
  group_by(
    state, group
  ) %>%
  
  mutate(quant_actual = approxfun(within, quant)(quant)) %>%
  
  select(state, group, quant, quant_actual) %>%
  filter(as.character(quant) %in% c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9))


forecast_quants_actual <- performance_data %>%
  left_join(quant_mapping) %>%
  
  rowwise() %>%
  
  mutate(upper = quantile(forecast_count, 0.5 + quant / 2),
         lower = quantile(forecast_count, 0.5 - quant / 2),
         upper_actual = quantile(forecast_count, 0.5 + quant_actual / 2),
         lower_actual = quantile(forecast_count, 0.5 - quant_actual / 2)) %>%
  ungroup()



forecast_quants_actual %>%
  filter(state == "QLD", group == "ward",
         as.character(quant) == "0.6") %>% 
  ggplot() +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(case_forecast_start, quant)),
              alpha = 0.4, fill = "blue") +
  
  geom_ribbon(aes(x = date, ymin = lower_actual, ymax = upper_actual, group = interaction(case_forecast_start, quant)),
              linetype = "dotted",
              colour = "red", alpha = 0.4, fill = NA) +
  
  plot_theme


coverage_by_day <- performance_data %>%
  filter(suffix == "final") %>%
  
  filter(run_date < ymd("2022-09-01"),
         run_date >= ymd("2022-03-15")) %>%
  
  expand_grid(
    quant = c(0.5, 0.9)
  ) %>%
  
  rowwise() %>%
  
  mutate(upper = quantile(forecast_count, 0.5 + quant / 2),
         lower = quantile(forecast_count, 0.5 - quant / 2)) %>%
  ungroup() %>%
  mutate(within = true_count >= lower & true_count <= upper) %>% 
  
  mutate(days_ahead = floor(days_ahead / 7)) %>% 
  
  group_by(days_ahead, state, group, quant) %>%
  summarise(within = sum(within) / n())



coverage_by_day %>%
  
  ggplot() +
  
  geom_line(aes(x = days_ahead * 7, y = within, group = interaction(quant, group), colour = group,
                linetype = factor(quant))) +
  
  facet_wrap(~state, ncol = 4) +
  
  geom_hline(yintercept = 0.5) +
  geom_hline(yintercept = 0.9,
             linetype = 2) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  scale_colour_manual(values = c("ward" = ward_base_colour, "ICU" = ICU_base_colour),
                      name = c("ward" = "Ward", "ICU" = "ICU")) +
  
  
  
  plot_theme +
  
  theme(panel.spacing.x = unit(1, "cm"),
        legend.position = "bottom")


trans_y <- function(y) - log(1 - y)
break_points <- c(-20, -3, 0, 0.75, 0.95)
minor_break_points <- c(-10, -1, 0.5, 0.9)


make_perf_plot <- function(i_group, i_legend = "none") {
  performance_data %>% 
    filter(run_date < ymd("2022-08-25"),
           run_date >= ymd("2022-03-15"),
           suffix == "final",
           group == i_group) %>%
    
    mutate(
      y = ((z_CRPS_naive - z_CRPS_forecast) / z_CRPS_naive),
      y = trans_y(y),
      is_pos = if_else(y > 0, "pos", "neg"),
      
      run_date = as_date(run_date),
      
      days_ahead = factor(floor(days_ahead / 7))
    ) %>%
    
    group_by(state, group, suffix, days_ahead, case_forecast_start) %>%
    summarise(y = mean(y, na.rm = TRUE)) %>% 
    
    ggplot()+
    
    geom_line(aes(x = case_forecast_start + days(7), y = y, colour = factor(days_ahead)),
              linetype = "solid") +
    geom_point(aes(x = case_forecast_start + days(7), y = y, colour = factor(days_ahead)),
               size = 1.3, pch = 15) +
    
    geom_hline(yintercept = trans_y(0), colour = 'grey20', linetype = 'solid', size = 0.7)  +
    
    facet_wrap(~state, ncol = 2) +
    
    xlab("Forecast start date") + ylab("Skill score (CRPS)") +
    
    plot_theme +
    
    scale_y_continuous(
      breaks = trans_y(break_points),
      minor_breaks = trans_y(minor_break_points),
      labels = break_points
    ) +
    
    scale_colour_manual(values = paletteer::paletteer_d("LaCroixColoR::PeachPear")[c(1, 4, 5)],
                        name = "Horizon",
                        labels = c("0" = "1-7 days",
                                   "1" = "8-14 days",
                                   "2" = "15-21 days")) +
    
    coord_cartesian(ylim = trans_y(range(break_points))) +
    
    theme(
      legend.position = i_legend,
      axis.text.y = element_text(size = 9)
    ) +
    
    scale_x_date(
      date_breaks = "months",
      labels = scales::label_date_short(format = c("%Y", "%b"))
    )
}

make_perf_plot("ward")


ggsave(
  str_c("results/performance_weeks_ahead_ward.pdf"),
  
  units = "cm",
  width = 18,
  height = 18,
  bg = "white"
)


make_perf_plot("ICU")


ggsave(
  str_c("results/performance_weeks_ahead_ICU.pdf"),
  
  units = "cm",
  width = 22,
  height = 18,
  bg = "white"
)





