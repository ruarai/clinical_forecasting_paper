


cases <- read_csv("~/mfluxunimelb/local_cases_input/local_cases_input_2022-10-04.csv")

forecast_quants <- tar_read(past_forecasts_data)$quants %>%
  filter(run_date < ymd("2022-08-25"))
occupancy_data <- tar_read(occupancy_data)

ward_base_colour <- "#b53aa0"
ICU_base_colour <- "#008200"

source("R/plots_common.R")

ward_cols <- shades::opacity(ward_base_colour, c(0.3, 0.6, 1))
ICU_cols <- shades::opacity(ICU_base_colour, c(0.3, 0.6, 1))
color_list <- list("ward" = ward_cols, "ICU" = ICU_cols)



get_quants_plot <- function(i_group, i_suffix, i_state, ylim) {
  
  runs <- forecast_quants %>%
    filter(state == i_state, suffix == i_suffix) %>%
    distinct(
      run_date,
      case_forecast_start
    ) %>%
    arrange(run_date) %>%
    
    mutate(id = row_number(),
           id_mod = (id - 1) %% 3,
           run_date = ymd(run_date))
  
  plot_data_mod <- forecast_quants %>%
    mutate(run_date = ymd(run_date),
           is_fit = run_date >= ymd("2022-03-15")) %>%
    filter(state == i_state, group == i_group, suffix == i_suffix) %>%
    left_join(runs, by = c("run_date", "case_forecast_start")) %>%
    filter(date >= case_forecast_start + ddays(7),
           date <= case_forecast_start + ddays(27),
           quant %in% c("20", "50", "90"))
  
  
  ward_known <- occupancy_data %>%
    filter(group == i_group, state == i_state)
  
  ggplot(plot_data_mod) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(run_date, quant), fill = quant),
                plot_data_mod %>% filter(is_fit)) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(run_date, quant), fill = quant),
                alpha = 0.2,
                plot_data_mod %>% filter(!is_fit)) +
    
    geom_point(aes(x = date, y = count),
               color = "white",
               ward_known,
               
               size = 0.3, stroke = 0)  +
    
    geom_point(aes(x = date, y = count),
               pch = 1,
               color = "grey20",
               ward_known,
               
               size = 0.3, stroke = 0.4) +
    
    geom_vline(aes(xintercept = case_forecast_start + ddays(7)),
               runs, linetype = 'dashed') +
    
    facet_wrap(~id_mod, ncol = 1, scales = "free") +
    
    scale_fill_manual(values = color_list[[i_group]]) +
    
    # geom_label(aes(x = run_date, y = ylim[2] * 0.95, label = as.character(run_date)),
    #            vjust = 1, hjust = 0.5, size = 3,
    # 
    #            data = runs) +
    
    plot_theme +
    
    scale_x_date(labels = scales::label_date_short(c("%Y", "%b"), sep = " "),
                 date_breaks = "months") +
    
    scale_y_continuous(
      breaks = scales::breaks_extended(),
      labels = scales::label_comma()
    ) +
    
    coord_cartesian(xlim = c(ymd("2022-02-01"), ymd("2022-09-01")),
                    ylim = ylim) +
    
    geom_blank(aes(y = 0)) +
    
    xlab(NULL) + ylab("Count") +
    
    theme(legend.position = "none",
          strip.text = element_blank(),
          axis.text.y = element_text(size = 9),
          axis.line = element_line()) +
    
    ggtitle(str_c("Forecasted ", i_group, " occupancy"))
}


get_skill_plot <- function(i_group, i_suffix, i_state) {
  performance_data %>% 
    filter(group == i_group,
           suffix == i_suffix,
           run_date < ymd("2022-08-25"),
           state == i_state,
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
                   alpha = 0.2,
                   size = 0.8) +
    geom_point(aes(x = date, y = y, colour = is_pos),
               alpha = 0.4,
               size = 0.4) +
    
    plot_theme +
    
    scale_x_date(labels = scales::label_date_short(c("%Y", "%b"), sep = " "),
                 date_breaks = "months") +
    
    xlab(NULL) + ylab("Skill score")+
    
    scale_colour_manual(values = paletteer::paletteer_d("LaCroixColoR::paired")[c(1, 3)]) +
    
    scale_y_continuous(
      breaks = trans_y(break_points),
      minor_breaks = trans_y(minor_break_points),
      labels = break_points,
      limits = trans_y(range(break_points))
    ) +
    
    coord_cartesian(xlim = c(ymd("2022-02-01"), ymd("2022-09-01"))) +
    
    
    geom_hline(yintercept = trans_y(0), colour = 'grey20', linetype = 'solid', size = 0.7) +
    
    theme(
      legend.position = "none",
      axis.text.y = element_text(size = 9)
    )
}

p_cases <- cases %>%
  filter(state == i_state,
         date_onset >= ymd("2022-02-01") - days(7)) %>%
  ggplot() +
  geom_point(aes(x = date_onset, y = count),
             size = 1,
             pch = 1) +
  
  scale_x_date(labels = scales::label_date_short(c("%Y", "%b"), sep = " "),
               date_breaks = "months") +
  
  scale_y_continuous(
    breaks = scales::breaks_extended(4),
    labels = scales::label_comma()
  ) +
  
  ggtitle("Case incidence") +
  
  xlab(NULL) + ylab("Count") +
  
  coord_cartesian(xlim = c(ymd("2022-02-01"), ymd("2022-09-01"))) +
  
  plot_theme +
  
  theme(
    axis.text.y = element_text(size = 9)
  )


state_plot_lims <- tribble(
  ~state, ~group, ~ylim,
  "NSW", "ward", 3600,
  "NSW", "ICU", 150,
  "SA", "ward", 650,
  "SA", "ICU", 35,
  "VIC", "ward", 1400,
  "VIC", "ICU", 150,
  "NT", "ward", 150,
  "NT", "ICU", 35,
  "WA", "ward", 700,
  "WA", "ICU", 40,
  "QLD", "ward", 1200,
  "QLD", "ICU", 50,
  "TAS", "ward", 200,
  "TAS", "ICU", 35,
  "ACT", "ward", 240,
  "ACT", "ICU", 35
) %>%
  pivot_wider(names_from = "group", values_from = "ylim") %>%
  expand_grid(suffix = c("final")) %>%
  arrange(state)



cairo_pdf("results/all_performance.pdf",
          width = 10, height = 9, onefile = TRUE)

for(i in 1:nrow(state_plot_lims)) {
  
  p_quants_ward <- get_quants_plot("ward", state_plot_lims$suffix[[i]], state_plot_lims$state[[i]], c(0, state_plot_lims$ward[[i]]))
  p_skill_ward <- get_skill_plot("ward", state_plot_lims$suffix[[i]], state_plot_lims$state[[i]])
  
  p_quants_ICU <- get_quants_plot("ICU", state_plot_lims$suffix[[i]], state_plot_lims$state[[i]], c(0, state_plot_lims$ICU[[i]]))
  p_skill_ICU <- get_skill_plot("ICU", state_plot_lims$suffix[[i]], state_plot_lims$state[[i]])
  
  p <- cowplot::plot_grid(
    p_cases,
    p_quants_ward,
    p_skill_ward,
    p_quants_ICU,
    p_skill_ICU,
    ncol = 1,
    rel_heights = c(1, 3, 1, 3, 1),
    align = "v", axis = "lr"
  )
  
  plot(p)
}

dev.off()



ggsave(
  "results/NSW_performance.pdf",
  height = 10,
  width = 8
)
