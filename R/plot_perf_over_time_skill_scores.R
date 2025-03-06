
plot_perf_over_time_skill_scores <- function(paper_performance_data, occupancy_data) {
  
  ordered_states <- state_nice_names[order(state_nice_names)]
  states_A <- ordered_states[1:4]
  states_B <- ordered_states[5:8]
  
  
  plot_data <- paper_performance_data %>%
    mutate(forecast_start_date = case_forecast_start + ddays(8),
           run_date = as_date(run_date),
           month = floor_date(run_date, "month")) %>% 
    group_by(forecast_start_date, group, state) %>% 
    summarise(log_CRPS_naive = mean(z_log_CRPS_naive, na.rm = TRUE), log_CRPS_forecast = mean(z_log_CRPS_forecast, na.rm = TRUE),
              mean_AE_naive = mean(AE_naive, na.rm = TRUE), mean_AE_forecast = mean(AE_forecast, na.rm = TRUE)) %>% 
    
    mutate(skill_log_CRPS = (log_CRPS_naive - log_CRPS_forecast) / log_CRPS_naive,
           skill_mean_AE = (mean_AE_naive - mean_AE_forecast) / mean_AE_naive) %>%
    
    mutate(group_label = factor(group, c("ward", "ICU"), c("Ward", "ICU")),
           state = state_nice_names[state])
  
  
  plot_data_occupancy <- occupancy_data %>% 
    group_by(state, group) %>%
    mutate(count = scales::rescale(count),
           group_label = factor(group, c("ward", "ICU"), c("Ward", "ICU")),
           state = state_nice_names[state])
  
  
  x_lims <- c(
    min(plot_data$forecast_start_date),
    max(plot_data$forecast_start_date) + days(21)
  )
  
  
  plot_states <- function(states) {
    
    p_skill <- ggplot() +
      
      geom_hline(yintercept = 0, linetype = "22") +
      geom_hline(yintercept = 1, linetype = "44") +
      
      geom_point(aes(x = forecast_start_date, y = skill_log_CRPS, colour = group_label),
                 size = 1.3,
                 plot_data %>% filter(state %in% states)) +
      
      geom_linerange(aes(xmin = forecast_start_date,
                         xmax = forecast_start_date + days(21),
                         y = skill_log_CRPS, colour = group_label),
                     size = 0.5, alpha = 0.2,
                     plot_data %>% filter(state %in% states)) +
      
      facet_wrap(~state, ncol = 4,
                 scales = "free_x") +
      
      scale_colour_manual(
        name = "Target",
        values = c("Ward" = color_list$ward[8], "ICU" = color_list$ICU[8])
      ) +
      
      xlab("Date") + ylab("Skill score (CRPS)") +
      
      scale_x_date(
        breaks = scales::breaks_width("month"),
        labels = scales::label_date_short()
      ) +
      
      scale_y_continuous(breaks = c(-6, -5, -4, -3, -2, -1, 0, 1)) +
      
      coord_cartesian(ylim = c(-6.5, 1.5),
                      xlim = x_lims) +
      
      plot_theme +
      theme(legend.position = "none",
            panel.spacing.x = unit(0.5, "cm"),
            strip.text = element_blank(),
            plot.margin = margin(r = 0.3, unit = "cm"))
    
    p_occ <- ggplot() +
      geom_step(aes(x = date, y = count, colour = group_label),
                plot_data_occupancy %>% filter(state %in% states)) +
      
      facet_wrap(~state, ncol = 4) +
      
      scale_colour_manual(
        name = "Target",
        values = c("Ward" = color_list$ward[8], "ICU" = color_list$ICU[8])
      ) +
      
      scale_y_continuous(breaks = c(0, 1)) +
      
      ylab("Occupancy") + xlab(NULL) +
      
      coord_cartesian(xlim = x_lims) +
      
      plot_theme +
      
      theme(legend.position = "none",
            axis.text.x = element_blank(), axis.ticks.x = element_blank(),
            axis.line.x = element_blank(), axis.text.y = element_text(colour = "white"),
            panel.spacing.x = unit(0.5, "cm"),
            plot.margin = margin(r = 0.3, unit = "cm"))
    
    cowplot::plot_grid(
      p_occ, p_skill, ncol = 1, align = "v", axis = "lr",
      rel_heights = c(1, 3)
    )
  }
  
  p_A <- plot_states(states_A)
  p_B <- plot_states(states_B)
  
  
  cowplot::plot_grid(
    p_A, p_B, ncol = 1, align = "v", axis = "lr",
    rel_heights = c(1, 1)
  )
  
  
  ggsave(
    "results/results_perf_over_time_skill_scores.png",
    bg = "white",
    scale = 10 / 16,
    width = 16, height = 16
  )
}
