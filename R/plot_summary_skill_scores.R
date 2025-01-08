
plot_summary_skill_scores <- function(paper_performance_data) {
  
  plot_data <- paper_performance_data %>%
    group_by(days_ahead, group, state) %>% 
    summarise(log_CRPS_naive = mean(z_log_CRPS_naive, na.rm = TRUE), log_CRPS_forecast = mean(z_log_CRPS_forecast, na.rm = TRUE),
              mean_AE_naive = mean(AE_naive, na.rm = TRUE), mean_AE_forecast = mean(AE_forecast, na.rm = TRUE)) %>% 
    
    mutate(skill_log_CRPS = (log_CRPS_naive - log_CRPS_forecast) / log_CRPS_naive,
           skill_mean_AE = (mean_AE_naive - mean_AE_forecast) / mean_AE_naive) %>%
    
    mutate(group_label = factor(group, c("ward", "ICU"), c("Ward", "ICU")))
  
  ggplot() +
    geom_line(aes(x = days_ahead, y = skill_log_CRPS, colour = state),
              plot_data) +
    
    geom_hline(yintercept = 0, linetype = "44") +
    geom_hline(yintercept = 1) +
    
    coord_cartesian(ylim = c(-1, 1)) +
    
    xlab("Days ahead") + ylab("Skill score (CRPS)") +
    
    ggokabeito::scale_colour_okabe_ito(name = "State/Territory") +
    
    facet_wrap(~group_label, ncol = 2, scales = "free_y") +
    
    plot_theme +
    theme(legend.position = "bottom")
  
  
  
  ggsave(
    "results/results_perf_summary_skill_scores.png",
    bg = "white",
    device = png,
    scale = 10 / 16,
    width = 16, height = 8
  )
  
}


