

library(targets)
library(tidyverse)
library(lubridate)

source("R/plots_common.R")

forecast_quants_retro <- tar_read(retro_forecasts_data)$quants
occupancy_data <- tar_read(occupancy_data_total)


performance_data_retro <- tar_read(retro_performance_data)

performance_data_retro %>%
  filter(date >= ymd("2023-03-01"), days_ahead > 14) %>% 
  select(suffix, state, group, run_date, case_forecast_start, date, metric = z_CRPS_forecast) %>%
  pivot_wider(names_from = suffix, values_from = metric) %>% 
  rename(
    baseline = test_pf_b_baseline,
    experimental = test_pf_b_2
  ) %>% 
  
  group_by(run_date, group, state) %>%
  summarise(baseline = mean(baseline), experimental = mean(experimental)) %>%
  
  mutate(skill_score = (baseline - experimental) / baseline) %>% 
  group_by(group, state) %>%
  summarise(p_better = sum(skill_score > 0) / n()) %>% 
  
  ggplot() +
  geom_point(aes(x = p_better, y = group)) +
  
  geom_vline(xintercept = 0.5) +
  
  facet_wrap(~state) +
  
  coord_cartesian(xlim = c(0, 1))


get_performance <- . %>% 
  
  summarise(
    CRPS_forecast = mean(z_log_CRPS_forecast),
    MAE_forecast = mean(AE_forecast),
    
    .groups = "drop"
  )


performance_data_retro %>%
  filter(date >= ymd("2023-03-01")) %>% 
  group_by(state, group, suffix) %>%
  get_performance() %>%
  
  ggplot() +
  geom_col(aes(x = state, y = CRPS_forecast, fill = suffix),
           position = position_dodge()) +
  
  plot_theme +
  
  facet_wrap(~group)


performance_data_retro %>%
  filter(group == "ward") %>% 
  group_by(state, group, days_ahead, suffix) %>%
  get_performance() %>%
  
  ggplot() +
  geom_line(aes(x = days_ahead, y = CRPS_forecast, colour = suffix),
            position = position_dodge()) +
  
  plot_theme +
  
  facet_wrap(~state)


performance_data_retro %>%
  group_by(state, group, suffix) %>%
  get_performance() %>%
  
  ggplot() +
  geom_col(aes(x = state, y = MAE_forecast, fill = suffix),
           position = position_dodge()) +
  
  plot_theme +
  
  facet_wrap(~group, scales = "free")

ggplot() +
  ggdist::stat_dots(aes(x = state, y = z_log_CRPS_forecast, colour = suffix),
                    side = "right",
                    performance_data_retro %>% filter(suffix == "test_pf_b_1")) +
  ggdist::stat_dots(aes(x = state, y = z_log_CRPS_forecast, colour = suffix),
                    side = "left",
                    performance_data_retro %>% filter(suffix == "test_pf_b_baseline")) +
  
  facet_wrap(~group) +
  
  coord_cartesian(ylim = c(0, 2)) +
  
  plot_theme



plot_data <- forecast_quants_retro %>%
  filter(quant == 50 | quant == 90)

date_range <- c(ymd("2022-11-15"), ymd("2023-06-01"))

runs <- plot_data %>%
  distinct(
    state,
    run_date,
    case_forecast_start
  ) %>%
  group_by(state) %>% 
  arrange(run_date) %>%
  
  mutate(id = match(run_date, unique(run_date)),
         id_mod = (id - 1) %% 3,
         run_date = ymd(run_date))

plot_data_mod <- plot_data %>%
  mutate(run_date = ymd(run_date)) %>%
  left_join(runs, by = c("run_date", "case_forecast_start", "state")) %>%
  filter(date > case_forecast_start + ddays(7),
         date <= case_forecast_start + ddays(28))

p_common <- list(
  plot_theme,
  scale_x_date(labels = scales::label_date_short(c("%Y", "%B"), sep = " "),
               date_breaks = "months"),
  scale_y_continuous(labels = scales::label_comma(), breaks = scales::breaks_extended(4), expand = expansion(mult = c(0, 0.05))),
  geom_blank(aes(y = 0)),
  xlab(NULL), ylab("Count"),
  
  theme(legend.position = "none",
        axis.text = element_text(size = 10),
        strip.text = element_blank(),
        panel.grid.major = element_blank())
)


performance_data_plot <- performance_data_retro %>%
  mutate(run_date = ymd(run_date)) %>% 
  
  left_join(
    runs %>% select(run_date, case_forecast_start, id_mod, state),
    by = c("run_date", "case_forecast_start", "state")
  )

color_list <- list("ward" = ward_base_colour, "ICU" = ICU_base_colour)

plot_meta <- tribble(
  ~i_state, ~i_group, ~y_upper, ~y_label, ~y_label_asterisk,
  "NSW", "ward", 3400, 3280, 3180,
  "NSW", "ICU", 150, 140, 136,
  
  "SA", "ward", 650, 640, 620,
  "SA", "ICU", 45, 40, 39,
  
  "VIC", "ward", 1400, 1350, 1300,
  "VIC", "ICU", 150, 140, 135,
  
  "NT", "ward", 150, 140, 135,
  "NT", "ICU", 25, 20, 17,
  
  "WA", "ward", 1200, 1100, 1050,
  "WA", "ICU", 45, 40, 39,
  
  "QLD", "ward", 1800, 1750, 1700,
  "QLD", "ICU", 60, 55, 54,
  
  "TAS", "ward", 280, 270, 265,
  "TAS", "ICU", 25, 20, 19,
  
  "ACT", "ward", 280, 275, 270,
  "ACT", "ICU", 35, 30, 29
  
) %>%
  expand_grid(i_suffix = c("test_pf_b_2", "test_pf_b_baseline"))

asterisk_runs <- plot_meta %>%
  right_join(
    plot_data_mod %>% 
      filter(quant == 90) %>% 
      group_by(state, run_date, case_forecast_start, id.y, id_mod, group) %>% 
      summarise(max_quant = max(upper)),
    by = c("i_state" = "state", "i_group" = "group")
  ) %>%
  rename(state = i_state, group = i_group) %>% 
  
  filter(max_quant > y_upper)  %>%
  
  bind_rows(
    expand_grid(state = names(state_nice_names), group = c("ward", "ICU"), id_mod = 0:2)
  )


plots <- pmap(
  plot_meta,
  function(i_state, i_group, y_upper, y_label, y_label_asterisk, i_suffix) {
    
    performance_data_plot_state_group_suffixless <- performance_data_plot %>%
      filter(state == i_state, group == i_group)
    
    performance_data_plot_state_group <- performance_data_plot %>%
      filter(state == i_state, group == i_group, suffix == i_suffix)
    
    p_grid <- map(
      0:2,
      function(i_mod) {
        
        p_occ <- ggplot(plot_data_mod %>% filter(state == i_state, group == i_group, id_mod == i_mod, suffix == i_suffix)) +
          
          
          # geom_tile(aes(x = date, width = 1, y = -10, height = Inf),
          #           fill = ggokabeito::palette_okabe_ito(5), alpha = 0.04,
          #           performance_data_plot_state_group %>% filter(id_mod == i_mod) %>%
          #             filter(days_ahead > 7, days_ahead <= 14)) +
          
          geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(run_date, quant, suffix), fill = suffix),
                      colour = "white",
                      alpha = 0.3) +
          
          geom_point(aes(x = date, y = count),
                     color = "grey20",
                     occupancy_data %>% filter(state == i_state, group == i_group),
                     
                     size = 0.3, stroke = 0.4) +
          
          scale_fill_manual(values = c("final" = ggokabeito::palette_okabe_ito(1),
                                       "test_pf3" = ggokabeito::palette_okabe_ito(2))) +
          
          p_common +
          
          geom_vline(aes(xintercept = case_forecast_start + ddays(7)),
                     colour = ggokabeito::palette_okabe_ito(5),
                     runs %>% filter(state == i_state, id_mod == i_mod), linetype = 'dashed', alpha = 0.3) +
          
          geom_label(aes(x = case_forecast_start + ddays(10), y = y_label, label = id),
                     label.r = unit(0, "cm"), label.size = 0, size = 3, colour = ggokabeito::palette_okabe_ito(5),
                     runs %>% filter(state == i_state, id_mod == i_mod)) +
          
          geom_label(aes(x = case_forecast_start + ddays(13), y = y_label_asterisk), label = "^",
                     label.r = unit(0, "cm"), label.size = 0, size = 4, colour = ggokabeito::palette_okabe_ito(5),
                     alpha = 0,
                     asterisk_runs %>% filter(state == i_state, group == i_group, id_mod == i_mod)) +
          
          facet_wrap(~id_mod, ncol = 1, scales = "free") +
          
          
          coord_cartesian(xlim = date_range,
                          ylim = c(0, y_upper)) +
          
          theme(axis.text.x = element_blank(),
                axis.ticks.x = element_blank())
        
        
        p_CRPS <- performance_data_plot_state_group %>%
          filter(id_mod == i_mod) %>% 
          ggplot() +
          
          
          # geom_tile(aes(x = date, width = 1, y = -10, height = Inf),
          #           fill = ggokabeito::palette_okabe_ito(5), alpha = 0.03,
          #           performance_data_plot_state_group %>% filter(id_mod == i_mod) %>%
          #             filter(days_ahead > 7, days_ahead <= 14)) +
          
          geom_line(aes(x = date, y = z_log_CRPS_forecast, group = interaction(run_date, suffix), colour = suffix)) +
          
          p_common +
          
          geom_vline(aes(xintercept = case_forecast_start + ddays(7)),
                     colour = ggokabeito::palette_okabe_ito(5),
                     runs %>% filter(state == i_state, id_mod == i_mod), linetype = 'dashed', alpha = 0.3) +
          
          ylab("CRPS") +
          
          coord_cartesian(xlim = date_range,
                          ylim = c(0, max(performance_data_plot_state_group_suffixless$z_log_CRPS_forecast)))
        
        
        
        cowplot::plot_grid(
          p_occ, p_CRPS, ncol = 1,
          rel_heights = c(2, 1),
          
          align = "v"
        )
      }
    ) %>%
      cowplot::plot_grid(plotlist = ., ncol = 1, align = "v")
    
    cowplot::plot_grid(
      ggplot() + theme_minimal() + ggtitle(str_c(i_suffix, " \u2012 Forecasted ", i_group, " occupancy \u2012 ", state_nice_names[i_state])) +
        theme(plot.title = element_text(margin = margin())),
      p_grid,
      rel_heights = c(0.04, 1),
      ncol = 1
    )
  }
)

plots[[1]]

plots_ordered <- plots[order(plot_meta$i_state)]



cairo_pdf("results/perf_pf_2023_b_2.pdf",
          width = 8.5, height = 9, onefile = TRUE)
for (i in 1:length(plots_ordered)) {
  plot(plots_ordered[[i]])
}
dev.off()

