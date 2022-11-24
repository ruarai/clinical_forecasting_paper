



library(tidyverse)
library(lubridate)
library(targets)

occupancy_data <- tar_read(occupancy_data)


case_ensemble <- read_csv("~/source/clinical_forecasting/results/fc_2022-07-08_final/archive/ensemble.csv")
local_cases <- read_csv("~/source/clinical_forecasting/results/fc_2022-09-29_final/archive/local_cases.csv")

forecast_trajs <- fst::read_fst("~/source/clinical_forecasting/results/fc_2022-07-08_final/trajectories.fst")


source("~/source/clinical_forecasting/R/make_result_quants.R")

source("R/plots_common.R")


alpha_vals <- scales::rescale(rev(1/1.7^(1:8)), to = c(0.05, 0.99))

case_cols <- shades::opacity("#006699", alpha_vals)
ward_cols <- shades::opacity("#b53aa0", alpha_vals)
ICU_cols <- shades::opacity("#008200", alpha_vals)

case_forecast_start <- ymd("2022-06-28")

case_quants <- case_ensemble %>%
  filter(state == "VIC", date >= ymd("2022-06-28"), date <= ymd("2022-06-28") + days(28)) %>%
  select(-forecast_origin) %>% 
  pivot_wider(names_from = .model, values_from = starts_with("sim")) %>%
  rename_with(~ if_else(str_starts(., "sim"), str_c("sim_", .), .)) %>% 
  make_results_quants()

forecast_quants <- forecast_trajs %>%
  filter(state == "VIC", date >= case_forecast_start) %>%
  select(sample, date, group, count) %>% 
  pivot_wider(names_from = sample, values_from = count,
              names_prefix = "sim_") %>%
  make_results_quants()


date_range <- c(ymd("2022-05-01"), ymd("2022-07-26"))

known_cases <- local_cases %>%
  filter(state == "VIC", date_onset <= date_range[2])


known_occupancy <- occupancy_data %>%
  filter(state == "VIC", date <= date_range[2])

forecast_text_size <- 4

occ_plots_common <- list(
  geom_vline(xintercept = case_forecast_start + days(1), linetype = "dotdash", alpha = 0.8),
  xlab(NULL), ylab(NULL),
  scale_y_continuous(breaks = scales::breaks_extended(3),
                     labels = scales::label_comma(),
                     expand = expansion(mult = c(0, 0))),
  scale_x_date(date_breaks = "months", labels = scales::label_date_short(c("%Y", "%B"))),
  plot_theme, theme(legend.position = "none")
)

p_cases <- ggplot() +
  
  geom_ribbon(
    aes(x = date, ymin = lower, ymax = upper, fill = quant),
    case_quants
  ) +
  
  geom_point(
    aes(x = date_onset, y = count),
    known_cases,
    size = 0.6, stroke = 0, colour = "white"
  ) +
  
  geom_point(
    aes(x = date_onset, y = count),
    known_cases,
    pch = 1, size = 0.6, stroke = 0.6
  ) +
  
  geom_point(
    aes(x = date_onset, y = count),
    known_cases %>% filter(date_onset < ymd("2022-06-30")),
    size = 0.6, stroke = 0, colour = "black"
  ) +
  
  # annotate(
  #   geom = "label", x = ymd("2022-05-01"), y = 23000, label = "Input case forecast",
  #   hjust = 0, label.size = 0, size = forecast_text_size, vjust = 1
  # ) +
  
  scale_fill_manual(values = case_cols) +
  
  occ_plots_common + 
  
  coord_cartesian(xlim = date_range, ylim = c(0, 23000)) +
  
  ylab("Incidence")

p_cases

p_ward <- ggplot() +
  
  geom_ribbon(
    aes(x = date, ymin = lower, ymax = upper, fill = quant),
    forecast_quants %>%
      filter(group == "ward")
  ) +
  
  geom_point(
    aes(x = date, y = count),
    known_occupancy %>% filter(group == "ward"),
    size = 0.6, stroke = 0, colour = "white"
  ) +
  
  geom_point(
    aes(x = date, y = count),
    known_occupancy %>% filter(group == "ward"),
    pch = 1, size = 0.6, stroke = 0.6
  ) +
  
  geom_point(
    aes(x = date, y = count),
    known_occupancy %>% filter(group == "ward", date < ymd("2022-07-08")),
    size = 0.6, stroke = 0, colour = "black"
  )  +
  
  # annotate(
  #   geom = "label", x = ymd("2022-05-01"), y = 1350, label = "Ward occupancy forecast",
  #   hjust = 0, label.size = 0, size = forecast_text_size, vjust = 1
  # ) +
  
  scale_fill_manual(values = ward_cols) +
  
  occ_plots_common + 
  geom_vline(xintercept = case_forecast_start + days(7), linetype = "dashed", alpha = 0.8) +
  
  coord_cartesian(xlim = date_range, ylim = c(0, 1350)) +
  
  ylab("Ward beds")

p_ICU <- ggplot() +
  
  geom_ribbon(
    aes(x = date, ymin = lower, ymax = upper, fill = quant),
    forecast_quants %>%
      filter(group == "ICU")
  ) +
  
  geom_point(
    aes(x = date, y = count),
    known_occupancy %>% filter(group == "ICU"),
    size = 0.6, stroke = 0, colour = "white"
  ) +
  
  geom_point(
    aes(x = date, y = count),
    known_occupancy %>% filter(group == "ICU"),
    pch = 1, size = 0.6, stroke = 0.6
  ) +
  
  geom_point(
    aes(x = date, y = count),
    known_occupancy %>% filter(group == "ICU", date < ymd("2022-07-08")),
    size = 0.6, stroke = 0, colour = "black"
  ) +
  
  # annotate(
  #   geom = "label", x = ymd("2022-05-01"), y = 75, label = "ICU occupancy forecast",
  #   hjust = 0, label.size = 0, size = forecast_text_size, vjust = 1
  # ) +
  
  scale_fill_manual(values = ICU_cols) +
  
  occ_plots_common +
  geom_vline(xintercept = case_forecast_start + days(7), linetype = "dashed", alpha = 0.8) +
  
  coord_cartesian(xlim = date_range, ylim = c(0, 75)) +
  
  ylab("ICU beds")



cowplot::plot_grid(
  p_cases, p_ward, p_ICU,
  ncol = 1,
  align = "v", axis = "lr"
)


time_varying_estimates <- read_rds("data/VIC_estimates_2022-07-05.rds") %>%
  filter(age_group %in% c("20-29", "40-49", "60-69")) %>%
  rename_ages()

quants_pr_age <- time_varying_estimates %>%
  select(bootstrap, age_group, date, pr_age_given_case) %>% 
  pivot_wider(names_from = bootstrap, values_from = pr_age_given_case,
              names_prefix = "sim_") %>%
  make_results_quants(c(0.5, 0.9))

quants_pr_hosp <- time_varying_estimates %>%
  select(bootstrap, age_group, date, pr_hosp) %>% 
  pivot_wider(names_from = bootstrap, values_from = pr_hosp,
              names_prefix = "sim_") %>%
  make_results_quants(c(0.5, 0.9))

quants_pr_ICU <- time_varying_estimates %>%
  select(bootstrap, age_group, date, pr_ICU) %>% 
  pivot_wider(names_from = bootstrap, values_from = pr_ICU,
              names_prefix = "sim_") %>%
  make_results_quants(c(0.5, 0.9))

est_plots_common <- list(
  xlab(NULL),
  scale_x_date(date_breaks = "months", labels = scales::label_date_short(c("%Y", "%B"))),
  plot_theme, theme(legend.position = "none", strip.text = element_blank()),
  scale_alpha_manual(values = c(0.3, 1)),
  geom_vline(xintercept = case_forecast_start, linetype = "dotdash", alpha = 0.8)
)

p_age_label <- ggplot(tibble(age_group = unique(quants_pr_age$age_group))) +
  geom_label(aes(x = 0, y = 0, label = age_group),
             label.size = 0, size = 3.5) +
  
  facet_wrap(~age_group, nrow = 1) +
  
  theme_void() +
  theme(strip.text = element_blank())


colour_age <- paletteer::paletteer_d("LaCroixColoR::PeachPear")[4]

p_age <- ggplot() +
  
  geom_line(aes(x = date, y = median),
            quants_pr_age %>% filter(date <= ymd("2022-06-28")),
            size = 0.8,
            colour = colour_age) +
  
  geom_line(aes(x = date, y = median), 
            quants_pr_age %>% filter(date >= ymd("2022-06-28")),
            size = 0.8,
            alpha = 0.4,
            colour = colour_age) +
  
  facet_wrap(~age_group, nrow = 1) +
  
  ylab(expression(p[age])) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  
  coord_cartesian(xlim = c(date_range[1], date_range[2]), ylim = c(0, 0.2)) +
  
  est_plots_common

p_age

linerange_size <- 0.4



p_pr_hosp <- ggplot() +
  geom_linerange(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
                 quants_pr_hosp %>% filter(date < ymd("2022-06-28")),
                 colour = ward_base_colour, size = linerange_size) +
  geom_linerange(aes(x = date, ymin = lower, ymax = upper, alpha = quant, group = date),
                 quants_pr_hosp %>% filter(date >= ymd("2022-06-28")),
                 alpha = 0.15,
                 colour = ward_base_colour, size = linerange_size) +
  
  facet_wrap(~age_group, nrow = 1) +
  
  ylab(expression(p[hosp])) +
  
  scale_y_continuous(trans = "log10",
                     breaks = c(0.001, 0.005, 0.01)) +
  
  coord_cartesian(xlim = c(date_range[1], date_range[2])) +
  
  est_plots_common



p_pr_ICU <- ggplot() +
  geom_linerange(aes(x = date, ymin = lower, ymax = upper, alpha = quant),
                 quants_pr_ICU %>% filter(date < ymd("2022-06-28")),
                 colour = ICU_base_colour, size = linerange_size) +
  geom_linerange(aes(x = date, ymin = lower, ymax = upper, alpha = quant, group = date),
                 quants_pr_ICU %>% filter(date >= ymd("2022-06-28")),
                 alpha = 0.15,
                 colour = ICU_base_colour, size = linerange_size) +
  
  facet_wrap(~age_group, nrow = 1) +
  
  ylab(expression(p[ICU])) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  
  coord_cartesian(xlim = c(date_range[1], date_range[2])) +
  
  est_plots_common



blank_plot <- ggplot() + theme_void()

p_forecast_title <- blank_plot +
  ggtitle("Output forecasts") +
  theme(plot.title = element_text(hjust = -0.1))

p_input_title <- blank_plot +
  ggtitle("Input estimates") +
  theme(plot.title = element_text(hjust = -0.1))

p_together <- cowplot::plot_grid(
  cowplot::plot_grid(
    p_input_title,
    p_age_label,
    p_pr_hosp,
    p_age,
    p_forecast_title,
    p_ward,
    rel_heights = c(1.8, 1, 10, 10, 1.8, 10),
    ncol = 1,
    align = "v", axis = "lr"
  ),
  cowplot::plot_grid(
    blank_plot,
    p_age_label,
    p_pr_ICU,
    p_cases,
    blank_plot,
    p_ICU,
    rel_heights = c(1.8, 1, 10, 10, 1.8, 10),
    ncol = 1,
    align = "v", axis = "lr"
  ),
  nrow = 1,
  align = "h",
  axis = "tb"
)

p_together

p_together_labelled <- p_together + 
  cowplot::draw_label("A", hjust = 0, vjust = 0, x = 0.01, y = 0.92, fontface = "bold") + 
  cowplot::draw_label("C", hjust = 0, vjust = 0, x = 0.01, y = 0.63, fontface = "bold") + 
  cowplot::draw_label("E", hjust = 0, vjust = 0, x = 0.01, y = 0.28, fontface = "bold") + 
  
  cowplot::draw_label("B", hjust = 0, vjust = 0, x = 0.51, y = 0.92, fontface = "bold") + 
  cowplot::draw_label("D", hjust = 0, vjust = 0, x = 0.51, y = 0.63, fontface = "bold") + 
  cowplot::draw_label("F", hjust = 0, vjust = 0, x = 0.51, y = 0.28, fontface = "bold")

p_together_labelled

ggsave(
  "results/example_forecast.pdf",
  plot = p_together_labelled,
  width = 12,
  height = 6,
  bg = "white"
)



