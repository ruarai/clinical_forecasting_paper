

library(targets)
library(tidyverse)
library(lubridate)
library(distributional)


source("R/plots_common.R")



performance_data <- tar_read(paper_performance_data)


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

plot_data_days_ahead <- performance_data %>% 
  
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
  summarise(p_within = sum(within) / n(),
            
            within_lower = binom.test(sum(within), n())$conf.int[1],
            within_upper = binom.test(sum(within), n())$conf.int[2])



coverage %>%
  mutate(state = state_nice_names[state]) %>% 

  ggplot() +
  
  geom_line(aes(x = quant, y = p_within, colour = group, group = group)) +
  # geom_ribbon(aes(x = quant, ymin = within_lower, ymax = within_upper, fill = group),
  #             colour = "white",
  #             alpha = 0.4) +
  
  facet_wrap(~state, ncol = 4) +
  
  annotate(geom = "segment", x = 0, xend = 1, y = 0, yend = 1,
           linetype = 'dashed') +
  
  geom_hline(yintercept = 0, colour = 'grey20', size = 0.6) +
  geom_vline(xintercept = 0, colour = 'grey20', size = 0.6) +
  
  coord_fixed(ylim = c(0,1), xlim = c(0,1)) +
  
  xlab("Prediction quantile") + ylab("Empirical coverage") +
  
  scale_x_continuous(expand = expansion(mult = c(0, 0.01))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  
  scale_fill_manual(values = c("ward" = ward_base_colour, "ICU" = ICU_base_colour),
                    labels = c("ward" = "Ward forecasts", "ICU" = "ICU forecasts"),
                    name = NULL) +
  
  scale_colour_manual(values = c("ward" = ward_base_colour, "ICU" = ICU_base_colour),
                      labels = c("ward" = "Ward forecasts", "ICU" = "ICU forecasts"),
                      name = NULL) +
  
  plot_theme +
  
  theme(panel.spacing.x = unit(1, "cm"),
        legend.position = "bottom",
        plot.margin = margin(l = 0.2, r = 0.7, unit = "cm"),
        strip.text = element_text(vjust = 1, size = 10),
        axis.text = element_text(size = 9))


ggsave(
  str_c("results_paper/quantile_coverage_states.pdf"),
  
  units = "cm",
  width = 18,
  height = 11,
  bg = "white"
)