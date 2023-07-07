
library(targets)
library(tidyverse)
library(lubridate)

time_varying_estimates <- qs::qload("~/source/clinical_forecasting/results/fc_2022-08-11_final/archive/NSW_archive.qs") %>%
  pluck("morbidity_trajectories_state")

forecast_dates <- read_csv("~/source/clinical_forecasting/results/fc_2022-08-11_final/forecast_dates.csv")

source("../clinical_forecasting/R/make_result_quants.R")
source("R/plots_common.R")



delay_parameters <- tar_read(clinical_parameters, store = "../clinical_forecasting/_targets/")




delay_data <- tibble(
  age_group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80+"),
  scale_onset_to_ward = c(3.41, 3.41, 3.41, 3.41, 3.41, 3.35, 3.35, 3.24, 3.24) * 0.7,
  shape_onset_to_ward = c(1.7, 1.7, 1.7, 1.7, 1.7, 1.7, 1.9, 1.9, 1.3) * 0.7,
  
  scale_ward_to_ICU = c(1.47, 1.47, 1.47, 1.47, 2.74, 2.74, 2.74, 3.53, 3.53),
  shape_ward_to_ICU = c(0.53, 0.53, 0.53, 0.53, 0.44, 0.44, 0.44, 0.52, 0.52)
) %>%
  mutate(
    days_prior_hosp = qgamma(0.99, shape = shape_onset_to_ward, scale = scale_onset_to_ward),
  ) %>%
  rowwise() %>%
  mutate(
    days_prior_ICU = quantile(
      rgamma(100000, shape = shape_onset_to_ward, scale = scale_onset_to_ward) +
        rgamma(100000, shape = shape_ward_to_ICU, scale = scale_ward_to_ICU),
      0.99
    )
  ) %>%
  ungroup()




estimate_quants <- time_varying_estimates %>% 
  pivot_longer(starts_with("pr_")) %>% 
  pivot_wider(names_from = bootstrap,
              names_prefix = "sim_",
              values_from = value) %>%
  make_results_quants(probs = c(0.5, 0.95)) %>%
  
  filter(date <= forecast_dates$NNDSS - ddays(7),
         date >= ymd("2022-04-01"), date <= ymd("2022-09-15"))










alpha_vals <- c(0.3, 1)
lr_size <- 0.6
zero_inflate_hosp <- 0

col_nowcast <- "grey70"



plots_pr_common <- list(
  scale_alpha_manual(values = alpha_vals),
  
  
  xlab(NULL), ylab(NULL),
  
  plot_theme,
  geom_blank(aes(y = 0)),
  
  geom_hline(yintercept = 0, color = "grey20",
             size = 0.6),
  
  theme(legend.position = "none",
        plot.title = element_text(size = 11)),
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))),
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(sep = " "))
)

age_quants_other <- estimate_quants %>% 
  filter(name == "pr_age_given_case",
         !(age_group %in% plot_age_groups))

p_age <- ggplot(estimate_quants %>% 
         filter(name == "pr_age_given_case",
                age_group %in% plot_age_groups) %>%
           rename_ages()) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper,
                  group = interaction(quant, age_group), fill = "Other"),
              age_quants_other,
              alpha = 0.2) +
  
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper,
                  group = interaction(quant, age_group), fill = age_group),
              alpha = 0.5) +
  
  geom_line(aes(x = date, y = median, color = "Other", group = age_group),
            alpha = 0.2,
            age_quants_other) +
  
  geom_line(aes(x = date, y = median, color = age_group)) +
  ggokabeito::scale_fill_okabe_ito(name = NULL, order = c(1, 2, 3, 8)) +
  ggokabeito::scale_color_okabe_ito(name = NULL, order = c(1, 2, 3, 8)) +
  
  plots_pr_common +
  
  theme(legend.position = c(0.8, 0.9)) +
  
  coord_cartesian(ylim = c(0, 0.25)) +
  
  theme(legend.background = element_rect(fill = "white", size = 0, colour = "white"),
        plot.title = element_text(vjust = 1))

p_age

p_hosp <- ggplot(estimate_quants %>% 
                   filter(name == "pr_hosp",
                          age_group %in% plot_age_groups) %>%
                   rename_ages()) +
  
  geom_rect(
    aes(ymin = -Inf, ymax = Inf,
        xmin = as_date(forecast_dates$NNDSS - ddays(days_prior_hosp)),
        xmax = forecast_dates$NNDSS - ddays(6)),
    delay_data %>%
      filter(age_group %in% plot_age_groups) %>%
      rename_ages(),
    fill = col_nowcast, alpha = 0.2
  ) +
  
  geom_linerange(aes(x = date, ymin = lower, ymax = upper,
                     group = quant, alpha = quant),
                 color = "#b53aa0",
                 size = lr_size) +
  
  facet_wrap(~age_group, ncol = 1, scales = "free_y") +
  
  plots_pr_common

p_hosp

#IC

p_ICU <- ggplot(estimate_quants %>%
                  filter(name == "pr_ICU", age_group %in% plot_age_groups) %>%
                  rename_ages()) +
  
  geom_rect(
    aes(ymin = -Inf, ymax = Inf,
        xmin = as_date(forecast_dates$NNDSS - ddays(days_prior_ICU)),
        xmax = forecast_dates$NNDSS - ddays(6)),
    delay_data %>%
      filter(age_group %in% plot_age_groups) %>%
      rename_ages(),
    fill = col_nowcast, alpha = 0.2
  ) +
  
  geom_linerange(aes(x = date, ymin = lower, ymax = upper,
                     group = quant, alpha = quant),
                 color = "#008200",
                 size =lr_size) +
  
  geom_point(aes(x = date, y = median ),
             color = "#008200",
             pch = '-',
             size = 4,
             
             data = estimate_quants %>% 
               filter(name == "pr_ICU",
                      median == 0,
                      age_group %in% plot_age_groups) %>%
               rename_ages()) +
  
  facet_wrap(~age_group, ncol = 1) +
  
  plots_pr_common

p_ICU



plot_null <- ggplot() + theme_void()

cowplot::plot_grid(
  plot_null, plot_null, plot_null,
  p_age,
  p_hosp,
  p_ICU,
  rel_heights = c(1, 8),
  ncol = 3,
  align = 'h',
  axis = 'tb'
) +
  cowplot::draw_label("Case age distribution", size = 12, hjust = 0, vjust = 1,
                      x = 0.02, y = 0.97) +
  cowplot::draw_label(expression(p["age|case"]*"(t)"), size = 12, hjust = 0, vjust = 1,
                      x = 0.02, y = 0.92) +
  cowplot::draw_label("Probability of case hospitalisation", size = 12, hjust = 0, vjust = 1,
                      x = 0.35, y = 0.97) +
  cowplot::draw_label(expression(p["hosp|age"]*"(t)"), size = 12, hjust = 0, vjust = 1,
                      x = 0.35, y = 0.92)  +
  cowplot::draw_label("Probability of ICU admission", size = 12, vjust = 1, hjust = 0,
                      x = 0.68, y = 0.97) +
  cowplot::draw_label(expression(p["ICU|hosp, age"]*"(t)"), size = 12, hjust = 0, vjust = 1,
                      x = 0.68, y = 0.92)



ggsave(
  paste0("results_paper/time_varying_estimates.pdf"),
  bg = "white",
  width = 28, height = 10,
  unit = "cm"
)







