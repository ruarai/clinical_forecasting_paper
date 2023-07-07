
library(tidyverse)
library(lubridate)
library(targets)


tar_make(past_forecasts_data)
forecast_quants <- tar_read(past_forecasts_data)$quants


source("../clinical_forecasting/R/read_occupancy_data.R")

occupancy_data <- read_occupancy_data("../clinical_forecasting/data/occupancy/NAT_2023-02-23_Data for Uni of Melbourne.xlsx")



ward_base_colour <- "#b53aa0"
ICU_base_colour <- "#008200"

source("R/plots_common.R")

ward_cols <- shades::opacity(ward_base_colour, c(0.3, 0.6, 1))
ICU_cols <- shades::opacity(ICU_base_colour, c(0.3, 0.6, 1))

color_list <- list("ward" = ward_cols, "ICU" = ICU_cols)


date_min <- ymd("2023-01-01")

forecast_subset_recent <- forecast_quants %>%
  filter(run_date >= date_min)


plot_single_forecast <- function(
    forecast_quants, occupancy_data,
    i_state, i_group, i_suffix, ylim
) {
  
  forecast_subset <- forecast_subset_recent %>%
    filter(state == i_state, suffix == i_suffix)
  
  
  runs <- forecast_subset %>%
    distinct(
      run_date,
      case_forecast_start
    )
  
  plot_data_mod <- forecast_subset %>%
    mutate(run_date = ymd(run_date)) %>%
    filter(state == i_state, group == i_group, suffix == i_suffix) %>%
    left_join(runs %>% mutate(run_date = ymd(run_date)), by = c("run_date", "case_forecast_start")) %>%
    filter(date >= case_forecast_start + days(7),
           date <= case_forecast_start + ddays(28),
           quant %in% c("50", "70", "90"))
  
  
  ward_known <- occupancy_data %>%
    filter(group == i_group, state == i_state)
  
  
  ggplot(plot_data_mod) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(run_date, quant), fill = quant),
                plot_data_mod) +
    
    geom_point(aes(x = date, y = count),
               color = "white",
               ward_known,
               
               size = 0.4) +
    
    geom_point(aes(x = date, y = count),
               pch = 1,
               color = "grey20",
               ward_known,

               size = 0.6, stroke = 0.6) +

    geom_vline(aes(xintercept = case_forecast_start + ddays(7)),
               runs, linetype = 'dashed') +

    facet_wrap(~run_date, ncol = 1, scales = "free") +

    scale_fill_manual(values = color_list[[i_group]]) +

    plot_theme +

    scale_x_date(labels = scales::label_date_short(c("", "%b"), sep = " "),
                 date_breaks = "months") +

    coord_cartesian(xlim = c(date_min, NA),
                    ylim = ylim) +

    geom_blank(aes(y = 0)) +

    xlab(NULL) + ylab("Count") +

    theme(legend.position = "none",
          strip.text = element_blank(),
          axis.line = element_line()) +
    
    ggtitle(str_c("Forecasted ", i_group, " occupancy"))
}

state_plot_lims <- tribble(
  ~state, ~group, ~ylim,
  "NSW", "ward", 2700,
  "NSW", "ICU", 90,
  "SA", "ward", 350,
  "SA", "ICU", 15,
  "VIC", "ward", 1000,
  "VIC", "ICU", 60,
  "NT", "ward", 100,
  "NT", "ICU", 10,
  "WA", "ward", 500,
  "WA", "ICU", 20,
  "QLD", "ward", 1000,
  "QLD", "ICU", 30,
  "TAS", "ward", 150,
  "TAS", "ICU", 20,
  "ACT", "ward", 180,
  "ACT", "ICU", 10
) %>%
  pivot_wider(names_from = "group", values_from = "ylim") %>%
  expand_grid(suffix = c("final")) %>%
  arrange(state)

plots <- pmap(
  state_plot_lims,
  function(state, ward, ICU, suffix) {
    
    p_ward <- plot_single_forecast(forecast_quants, occupancy_data, state, "ward", suffix, c(0, ward))
    p_ICU <- plot_single_forecast(forecast_quants, occupancy_data, state, "ICU", suffix, c(0, ICU))
    
    p <- cowplot::plot_grid(
      p_ward + ggtitle(str_c(state, " \u2013 Forecasted ward occupancy")),
      p_ICU + ggtitle(str_c(state, " \u2013 Forecasted ICU occupancy")),
      
      ncol = 2, align = 'v', axis = 'tb'
    )
    
    p
    
})



cairo_pdf(str_c("results/perf/recent_forecasts_", min(forecast_subset_recent$run_date),
                "_to_", max(forecast_subset_recent$run_date), ".pdf"),
          width = 22 / 2.54, height = 22 / 2.54, onefile = TRUE)
for (i in 1:length(plots)) {
  plot(plots[[i]])
}
dev.off()


source("R/get_performance_data.R")


forecast_trajs <- tar_read(past_forecasts_data)$trajs



forecast_trajs_recent <- forecast_trajs %>%
  filter(run_date >= date_min)




recent_perf <- get_performance_data(trajs = forecast_trajs_recent, occupancy_data = occupancy_data)








coverage <- recent_perf %>%
  filter(suffix == "final") %>%
  drop_na(true_count) %>% 
  
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
  
  ggplot() +
  
  geom_line(aes(x = quant, y = p_within, colour = group, group = group)) +
  geom_ribbon(aes(x = quant, ymin = within_lower, ymax = within_upper, fill = group),
              colour = "white",
              alpha = 0.4) +
  
  facet_wrap(~state, ncol = 4) +
  
  annotate(geom = "segment", x = 0, xend = 1, y = 0, yend = 1,
           linetype = 'dashed') +
  
  geom_hline(yintercept = 0, colour = 'grey20', size = 0.6) +
  geom_vline(xintercept = 0, colour = 'grey20', size = 0.6) +
  
  coord_fixed(ylim = c(0,1), xlim = c(0,1)) +
  
  xlab("Quantile") + ylab("Empirical coverage") +
  
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  
  scale_fill_manual(values = c("ward" = ward_base_colour, "ICU" = ICU_base_colour),
                    labels = c("ward" = "Ward", "ICU" = "ICU"),
                    name = NULL) +
  
  scale_colour_manual(values = c("ward" = ward_base_colour, "ICU" = ICU_base_colour),
                     labels = c("ward" = "Ward", "ICU" = "ICU"),
                     name = NULL) +
  
  plot_theme +
  
  theme(panel.spacing.x = unit(1, "cm"),
        legend.position = "bottom")


ggsave(
  str_c("results/perf/interval_coverage_", min(forecast_trajs_recent$run_date),
        "_to_", max(forecast_trajs_recent$run_date), ".png"),
  
  width = 8, height = 4.5,
  bg = "white"
)




intervals <- recent_perf %>%
  filter(suffix == "final") %>%
  drop_na(true_count) %>% 
  
  expand_grid(
    quant = c(0, 0.01, seq(0.05, 0.95, by = 0.05), 0.99, 1)
  ) %>%
  
  rowwise() %>%
  
  mutate(upper = quantile(forecast_count, 0.5 + quant / 2),
         lower = quantile(forecast_count, 0.5 - quant / 2)) %>%
  ungroup() %>%
  mutate(within = true_count >= lower & true_count <= upper)


intervals %>%
  filter(state == "VIC",
         group == "ward",
         quant %in% c(0.25, 0.5, 0.75, 0.9, 0.95)) %>%
  
  ggplot() +
  geom_linerange(aes(x = date, ymin = lower - true_count, ymax = upper - true_count, colour = within)) +
  
  facet_wrap(~quant)






