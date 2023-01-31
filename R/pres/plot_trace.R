

library(tidyverse)
library(lubridate)
library(targets)

source("R/plots_common.R")

sim_archive <- qs::qread("~/source/clinical_forecasting/results/fc_2022-08-17_final/archive/NSW_archive.qs")

sim_results <- sim_archive$sim_results

# [1] "symptomatic_clinical" "ward"                 "discharged_ward"      "died_ward"            "ICU"                
# "discharged_ICU"       "died_ICU"             "postICU_to_discharge" "postICU_to_death"    
# [10] "discharged_postICU"   "died_postICU"     

traj_plot_data <- sim_results$trajectories_ungrouped %>%
  
  filter(sample == 1,
         date >= ymd("2022-04-15"),
         compartment %in% c("symptomatic_clinical", "ward", "ICU")) %>% 
  
  pivot_longer(c(count, transitions)) %>% 
  
  mutate(
    compartment = if_else(compartment == "symptomatic_clinical", "symptomatic", compartment),
    compartment = factor(compartment, c("symptomatic", "ward", "ICU")),
    name = factor(name, c("transitions", "count")),
    
    value = if_else(compartment == "symptomatic", value * 0.75, value),
    value = if_else(compartment == "ward", value / 2, value),
    value = if_else(compartment == "ward" & name == "count", value * 1.33, value),
    value = if_else(compartment == "ICU", value * 4, value)
  )



ggsave(
  "results/pres/trace.png",
  width = 6,
  height = 4,
  bg = "white"
)




case_forecast <- read_csv("~/source/clinical_forecasting/results/fc_2022-08-17_final/archive/ensemble.csv")

local_cases <- read_csv("~/source/clinical_forecasting/results/fc_2022-08-17_final/archive/local_cases.csv")

known_cases <- local_cases %>%
  filter(state == "NSW", date_onset <= ymd("2022-08-10"))


source("~/source/clinical_forecasting/R/make_result_quants.R")

case_quants <- case_forecast %>%
  filter(state == "NSW",
         date <= ymd("2022-09-06")) %>%
  select(-forecast_origin) %>% 
  pivot_wider(names_from = .model, values_from = starts_with("sim")) %>%
  rename_with(~ if_else(str_starts(., "sim"), str_c("sim_", .), .)) %>% 
  make_results_quants()

occ_plots_common <- list(
  geom_vline(xintercept = min(case_forecast$date) + days(1), linetype = "dotdash", alpha = 0.8),
  xlab(NULL), ylab(NULL),
  scale_y_continuous(breaks = scales::breaks_extended(3),
                     labels = scales::label_comma(),
                     expand = expansion(mult = c(0, 0))),
    
    scale_x_date(breaks = seq(ymd("2022-06-01"), ymd("2022-09-01"), "months"),
                 labels = str_c("Month ", 1:4)),
  plot_theme, theme(legend.position = "none")
)

alpha_vals <- scales::rescale(rev(1/1.7^(1:8)), to = c(0.05, 0.99))

case_cols <- shades::opacity("#006699", alpha_vals)
ward_cols <- shades::opacity("#b53aa0", alpha_vals)
ICU_cols <- shades::opacity("#008200", alpha_vals)

p_case_incidence <- ggplot() +
  
  geom_ribbon(
    aes(x = date, ymin = lower * 0.6, ymax = upper * 0.6, fill = quant),
    case_quants
  ) +
  
  geom_point(
    aes(x = date_onset, y = count * 0.6),
    known_cases,
    size = 0.6, stroke = 0, colour = "white"
  ) +

  geom_point(
    aes(x = date_onset, y = count * 0.6),
    known_cases,
    pch = 1, size = 0.6, stroke = 0.6
  ) +
  
  scale_fill_manual(values = case_cols) +
  
  occ_plots_common + 
  
  coord_cartesian(ylim = c(0, 12000),
                  xlim = c(ymd("2022-06-01"), ymd("2022-09-04"))) +
  
  ylab(NULL)


p_case_incidence_trace <- case_forecast %>%
  filter(state == "NSW", .model == "moss",
         date <= ymd("2022-09-06")) %>%
  
  ggplot() +
  
  geom_point(
    aes(x = date_onset, y = count * 0.6),
    known_cases,
    size = 0.6, stroke = 0, colour = "white"
  ) +
  
  geom_point(
    aes(x = date_onset, y = count * 0.6),
    known_cases,
    pch = 1, size = 0.6, stroke = 0.6
  ) +
  
  geom_ribbon(
    aes(x = date, ymin = lower * 0.6, ymax = upper * 0.6, fill = quant),
    case_quants
  ) +
  geom_step(aes(x = date, y = sim5 * 0.6)) +
  
  scale_fill_manual(values = case_cols %>% shades::opacity(0.08)) +
  
  occ_plots_common + 
  
  coord_cartesian(ylim = c(0, 12000),
                  xlim = c(ymd("2022-06-01"), ymd("2022-09-04"))) +
  
  ylab(NULL)



p_hosp_incidence_trace <- ggplot(traj_plot_data %>% filter(compartment == "symptomatic", name == "transitions")) +
  
  geom_step(aes(x = date, y = value)) +
  
  #facet_wrap(~compartment * name, scales = "free_y", ncol = 2, labeller = label_wrap_gen(multi_line=FALSE)) +
  
  geom_hline(yintercept = 0) +
  
  xlab(NULL) + ylab(NULL) +
  
  scale_x_date(breaks = seq(ymd("2022-06-01"), ymd("2022-09-01"), "months"),
               labels = str_c("Month ", 1:4)) +
  coord_cartesian(xlim = c(ymd("2022-06-01"), ymd("2022-09-04")),
                  ylim = c(0, NA)) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  occ_plots_common +
  plot_theme +
    
  theme(legend.position = "none")


cowplot::plot_grid(
  p_case_incidence,
  ggplot() + theme_void(),
  p_hosp_incidence_trace,
  
  ncol = 1,
  rel_heights = c(1, 0.1, 1),
  
  align = "v"
)

ggsave(
  "results/pres/trace_stack_1.png",
  width = 5,
  height = 4,
  dpi = 400,
  bg = "white"
)

cowplot::plot_grid(
  p_case_incidence_trace,
  ggplot() + theme_void(),
  p_hosp_incidence_trace,
  
  ncol = 1,
  rel_heights = c(1, 0.1, 1),
  
  align = "v"
)

ggsave(
  "results/pres/trace_stack_2.png",
  width = 5,
  height = 4,
  dpi = 400,
  bg = "white"
)



traj_plot_data %>%
  filter(compartment == "ward" | compartment == "ICU" | compartment == "symptomatic") %>% 
  
  ggplot() +
  
  geom_step(aes(x = date, y = value)) +
  
  facet_wrap(~compartment * name, scales = "free", ncol = 2, labeller = label_wrap_gen(multi_line=FALSE)) +
  
  geom_hline(yintercept = 0) +
  
  xlab(NULL) + ylab(NULL) +
  
  ggokabeito::scale_colour_okabe_ito(order = c(3, 5)) +
  occ_plots_common +
  
  scale_x_date(breaks = seq(ymd("2022-06-01"), ymd("2022-09-01"), "months"),
               labels = str_c("Month ", 1:4)) +
  coord_cartesian(xlim = c(ymd("2022-06-01"), ymd("2022-09-16")),
                  ylim = c(0, NA)) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  
  plot_theme +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, colour = "white"))



ggsave(
  "results/pres/trace_occ_1.png",
  width = 9,
  height = 6,
  dpi = 400,
  bg = "white"
)

source("R/pres/gaussprocess.R")

# ward_plot_data <- traj_plot_data %>%
#   filter(compartment == "ward" | compartment == "ICU",
#          name == "count",
#          date >= ymd("2022-05-25")) %>%
#   
#   group_by(compartment) %>% 
#   
#   mutate(false_true_occ = value + 
#            (gaussprocess(m = n(), from = 0, to = 2, K = function(s, t) exp(-32 * (s - t) ^ 2))$xt * 4 + rnorm(n(), 0, 1)) * 
#            if_else(compartment == "ward", 20, 10))

ward_plot_data <- read_csv("data/pres_trace_ward_plot_data.csv")  %>% 
  
  mutate(
    compartment = factor(compartment, c("symptomatic", "ward", "ICU")),
    name = factor(name, c("transitions", "count"))
  )
  

traj_plot_data %>%
  filter(compartment == "ward" | compartment == "ICU" | compartment == "symptomatic") %>% 
  
  ggplot() +
  
  geom_step(aes(x = date, y = value)) +
  
  geom_point(aes(x = date, y = false_true_occ),
             ward_plot_data %>% filter(date <= ymd("2022-08-15")),
             colour = "#E5F5F0" %>% shades::saturation(10),
             size = 0.8) +
  
  geom_point(aes(x = date, y = false_true_occ),
             ward_plot_data %>% filter(date <= ymd("2022-08-15")),
             pch = 1, stroke = 0.5, size = 0.8) +
  
  facet_wrap(~compartment * name, scales = "free", ncol = 2, labeller = label_wrap_gen(multi_line=FALSE)) +
  
  geom_hline(yintercept = 0) +
  
  xlab(NULL) + ylab(NULL) +
  
  ggokabeito::scale_colour_okabe_ito(order = c(3, 5)) +
  occ_plots_common +
  
  scale_x_date(breaks = seq(ymd("2022-06-01"), ymd("2022-09-01"), "months"),
               labels = str_c("Month ", 1:4)) +
  coord_cartesian(xlim = c(ymd("2022-06-01"), ymd("2022-09-16")),
                  ylim = c(0, NA)) +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  
  plot_theme +
  theme(legend.position = "none",
        strip.text = element_text(size = 12, colour = "white"))


ggsave(
  "results/pres/trace_occ_2.png",
  width = 9,
  height = 6,
  dpi = 400,
  bg = "white"
)



ggplot() +
  
  geom_step(aes(x = date, y = value, colour = name),
            ward_plot_data) +
  
  geom_point(aes(x = date, y = false_true_occ),
             ward_plot_data %>% filter(date <= ymd("2022-08-15"))) +
  
  facet_wrap(~compartment * name, scales = "free_y", ncol = 2, labeller = label_wrap_gen(multi_line=FALSE)) +
  
  geom_hline(yintercept = 0) +
  
  xlab(NULL) + ylab(NULL) +
  
  ggokabeito::scale_colour_okabe_ito(order = c(5)) +
  
  scale_x_date(breaks = seq(ymd("2022-06-01"), ymd("2022-09-01"), "months"),
               labels = str_c("Month ", 1:4)) +
  coord_cartesian(xlim = c(ymd("2022-07-25"), ymd("2022-09-05")),
                  ylim = c(0, NA)) +
  occ_plots_common +
  
  scale_y_continuous(expand = expansion(mult = c(0, 0.2))) +
  
  plot_theme +
  theme(legend.position = "none")


p_case_incidence_2 <- ggplot() +
  
  geom_ribbon(
    aes(x = date, ymin = lower * 0.6, ymax = upper * 0.6, fill = quant),
    case_quants
  ) +
  
  geom_point(
    aes(x = date_onset, y = count * 0.6),
    known_cases
  ) +
  
  geom_point(
    aes(x = date_onset, y = count * 0.6),
    known_cases,
    colour = "white", size = 0.4, stroke = 0.4
  ) +
  
  scale_fill_manual(values = case_cols) +
  
  occ_plots_common + 
  
  coord_cartesian(ylim = c(0, 12000),
                  xlim = c(ymd("2022-06-01"), ymd("2022-09-04"))) +
  
  ylab(NULL)


forecast_quants <- sim_results$trajectories %>%
  select(sample, date, group, count) %>% 
  pivot_wider(names_from = sample, values_from = count,
              names_prefix = "sim_") %>%
  make_results_quants()


p_ward <- ggplot(forecast_quants %>% filter(group == "ward", date >= ymd("2022-08-10"))) +
  
  geom_ribbon(
    aes(x = date, ymin = lower * 0.6 , ymax = upper * 0.6, fill = quant)
  ) +
  
  scale_fill_manual(values = ward_cols) +
  
  geom_point(aes(x = date, y = false_true_occ),
             ward_plot_data %>% filter(date <= ymd("2022-08-15"), compartment == "ward")) +
  
  geom_point(aes(x = date, y = false_true_occ),
             colour = "white", size = 0.4, stroke = 0.4,
             ward_plot_data %>% filter(date <= ymd("2022-08-15"), compartment == "ward")) +
  
  occ_plots_common +
  
  scale_x_date(breaks = seq(ymd("2022-06-01"), ymd("2022-09-01"), "months"),
               labels = str_c("Month ", 1:4)) +
  coord_cartesian(xlim = c(ymd("2022-06-01"), ymd("2022-09-05")),
                  ylim = c(0, 1600))  +
  
  ylab(NULL)


p_ICU <- ggplot(forecast_quants %>% filter(group == "ICU", date >= ymd("2022-08-10"))) +
  
  geom_ribbon(
    aes(x = date, ymin = lower * 4 - 15, ymax = upper * 4 - 15, fill = quant)
  ) +
  
  scale_fill_manual(values = ICU_cols) +
  
  geom_point(aes(x = date, y = false_true_occ),
             ward_plot_data %>% filter(date <= ymd("2022-08-15"), compartment == "ICU")) +
  
  geom_point(aes(x = date, y = false_true_occ),
             colour = "white", size = 0.4, stroke = 0.4,
             ward_plot_data %>% filter(date <= ymd("2022-08-15"), compartment == "ICU")) +
  
  occ_plots_common +
  
  scale_x_date(breaks = seq(ymd("2022-06-01"), ymd("2022-09-01"), "months"),
               labels = str_c("Month ", 1:4)) +
  coord_cartesian(xlim = c(ymd("2022-06-01"), ymd("2022-09-05")),
                  ylim = c(0, 400))  +
  
  ylab(NULL)


cowplot::plot_grid(
  p_case_incidence_2,
  ggplot() + theme_void(),
  p_ward,
  ggplot() + theme_void(),
  p_ICU,
  
  rel_heights = c(1, 0.1, 1, 0.1, 1),
  align = "v",
  
  ncol = 1
)  
  

ggsave(
  "results/pres/forecast_final.png",
  width = 7,
  height = 6,
  dpi = 400,
  bg = "white"
)

