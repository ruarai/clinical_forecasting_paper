

performance_data_retro3 <- performance_data %>%
  filter(str_starts(suffix, "retro3"),
         days_ahead >= 13,
         run_date <= ymd("2022-08-10"))

suffix_labels <- c(
  "retro3_neither" = "Future case incidence and time-varying probabilities predicted",
  "retro3_knowntv" = "Future time-varying probabilities known exactly",
  "retro3_knowncases" = "Future case incidence known exactly",
  "retro3_knownboth" = "Future case incidence and time-varying probabilities known exactly"
)

perf_scores_retro3 <- performance_data_retro3 %>%
  
  group_by(suffix) %>%
  filter(group == "ward") %>% 
  
  summarise(
    mean_error = mean(AE_forecast)
  ) %>%
  
  mutate(relative_error = mean_error / mean_error[suffix == "retro3_neither"]) %>%
  
  select(suffix, mean_error, relative_error) %>%
  
  mutate(
    label = str_c("Mean absolute error: ", round(mean_error, 0), " (", round(relative_error, 2), "x)"),
    suffix_label = suffix_labels[suffix],
    suffix_label = factor(suffix_label, suffix_labels)
  )

  
  
  
forecast_quants <- tar_read(retro_forecasts_data)$quants

occupancy_data <- tar_read(occupancy_data)



forecast_quants %>%
  mutate(days_ahead = as.numeric(date - case_forecast_start) - 7,
         suffix_label = suffix_labels[suffix],
         suffix_label = factor(suffix_label, suffix_labels)) %>% 
  filter(group == "ward",
         quant == 50 | quant == 90,
         days_ahead >= 13,
         case_forecast_start >= ymd("2022-02-15"),
         run_date <= ymd("2022-08-10")) %>% 
  ggplot() +
  
  geom_point(aes(x = date, y = count),
             occupancy_data %>% filter(group == "ward", state == "NSW",
                                       date >= ymd("2022-03-01"),
                                       date <= max(forecast_quants$date)),
             size = 0.6, pch = 1) +

  geom_ribbon(aes(x = date, ymin = lower, ymax = upper,
                  group = interaction(case_forecast_start, suffix, quant)),
              alpha = 0.2, fill = ward_base_colour) +
  
  geom_line(aes(x = date, y = median, 
                group = interaction(case_forecast_start, suffix)),
            colour = ward_base_colour) +
  
  geom_label(
    aes(x = ymd("2022-03-01") - days(1), y = 3010, label = label),
    
    hjust = 0, vjust = 1, label.r = unit(0, "cm"),
    label.size = 0.1, size = 3.4,
    perf_scores_retro3
  ) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short()) +
  
  scale_y_continuous(
    breaks = scales::breaks_extended(4),
    labels = scales::label_comma(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  
  xlab(NULL) + ylab("Count") +
  
  coord_cartesian(ylim = c(0, 3400)) +
  
  facet_wrap(~suffix_label, ncol = 2, scales = "free_y") +
  
  plot_theme +
  
  theme(
    strip.text = element_text(size = 10.7, vjust = 1)
  )
