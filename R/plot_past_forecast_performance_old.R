
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
  
  #mutate(days_ahead = floor(days_ahead / 7)) %>% 
  
  group_by(days_ahead, state, group, quant) %>%
  summarise(p_within = sum(within) / n(),
            
            within_lower = binom.test(sum(within), n())$conf.int[1],
            within_upper = binom.test(sum(within), n())$conf.int[2])



coverage_by_day %>%
  filter(quant == 0.5) %>% 
  
  ggplot() +
  
  geom_line(aes(x = days_ahead, y = p_within, colour = group, group = interaction(group, quant))) +
  geom_ribbon(aes(x = days_ahead, ymin = within_lower, ymax = within_upper, fill = group, group = interaction(group, quant)),
              colour = "white",
              alpha = 0.4) +
  
  facet_wrap(~state, ncol = 4) +
  
  geom_hline(yintercept = 0.5) +
  # geom_hline(yintercept = 0.9,
  #            linetype = 2) +
  
  coord_cartesian(ylim = c(0, 1)) +
  
  scale_colour_manual(values = c("ward" = ward_base_colour, "ICU" = ICU_base_colour),
                      labels = c("ward" = "Ward", "ICU" = "ICU")) +
  
  scale_fill_manual(values = c("ward" = ward_base_colour, "ICU" = ICU_base_colour),
                    labels = c("ward" = "Ward", "ICU" = "ICU")) +
  
  
  
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
