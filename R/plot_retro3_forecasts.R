

library(targets)
library(tidyverse)
library(lubridate)
library(distributional)

performance_data_retro3 <- tar_read(performance_data) %>%
  filter(str_starts(suffix, "retro3"),
         days_ahead >= 13,
         run_date <= ymd("2022-08-10"))

suffix_labels <- c(
  "retro3_neither" = "Baseline",
  "retro3_knowntv" = "Future time-varying probabilities\nknown exactly",
  "retro3_knowncases" = "Future case incidence\nknown exactly",
  "retro3_knownboth" = "Future case incidence and time-varying\nprobabilities known exactly"
)

perf_scores_retro3 <- performance_data_retro3 %>%
  
  group_by(suffix) %>%
  filter(group == "ward") %>% 
  
  summarise(
    mean_error = mean(AE_forecast)
  ) %>%
  
  mutate(
    suffix_label = suffix_labels[suffix],
    suffix_label = factor(suffix_label, suffix_labels)
  )


# Add uncertainty

perf_scores_retro3 %>%
  mutate(suffix_label = fct_rev(suffix_label)) %>% 
  ggplot() +
  
  geom_vline(xintercept = perf_scores_retro3 %>% filter(suffix == "retro3_neither") %>% pull(mean_error),
             linetype = "dashed", alpha = 0.5) +
  
  geom_linerange(aes(xmin = 0, xmax = mean_error, y = suffix_label),
                 size = 4) +
  
  coord_cartesian(xlim = c(0, NA)) +
  
  xlab("Mean absolute error (MAE)") +
  ylab(NULL) +
  
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  
  plot_theme +
  
  ggtitle("Performance \u2012 known and unknown future components")
  
  
  
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
