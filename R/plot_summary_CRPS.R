


plot_summary_CRPS <- function(performance_data) {
  
  get_performance <- . %>%
    
    summarise(
      CRPS_forecast = mean(z_log_CRPS_forecast),
      
      MAE_forecast = mean(AE_forecast),
      
      .groups = "drop"
    )
  
  perf_days_ahead <- performance_data %>%
    
    group_by(group, state, date, run_date, days_ahead) %>% 
    
    get_performance()
  
  
  alpha_vals <- scales::rescale(rev(1/1.7^(1:4)), to = c(0.15, 0.99))
  ward_cols <- shades::opacity(ward_base_colour, alpha_vals)
  ICU_cols <- shades::opacity(ICU_base_colour, alpha_vals)
  
  get_tbl_intervals <- function(
    tbl,
    column_name = "value",
    interval_widths = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  ) {
    quant_probs <- c(rev(1 - interval_widths) / 2, 0.5 + interval_widths / 2)
    quant_names <- c(rev(interval_widths) * 100, interval_widths * 100)
    quant_side <- c(rep("lower", length(interval_widths)), rep("upper", length(interval_widths)))
    
    
    tbl %>%
      summarise(
        result = list(
          tibble(
            quant = quant_names,
            side = quant_side,
            value = quantile(!!sym(column_name), quant_probs)
          )
        ),
        
        median = median(!!sym(column_name))
      ) %>%
      
      unnest(result) %>%
      pivot_wider(names_from = "side", values_from = "value") %>%
      mutate(quant = (factor(quant, unique(quant))))
  }
  
  
  plot_data <- perf_days_ahead %>%
    group_by(state, group, days_ahead) %>%
    get_tbl_intervals("CRPS_forecast", c(0.5, 0.75, 0.9, 0.95))
  
  plot_data_summ <- perf_days_ahead %>%
    group_by(state, group) %>%
    get_tbl_intervals("CRPS_forecast", c(0.5, 0.75, 0.9, 0.95))
  
  size_days_ahead <- 1.7
  
  p_ward_days_ahead <- ggplot() +
    
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    
    geom_linerange(aes(x = days_ahead, ymin = lower, ymax = upper, group = quant, colour = quant),
                   size = size_days_ahead,
                   plot_data %>% filter(group == "ward")) +
    
    geom_linerange(aes(x = days_ahead, ymin = median - 0.003, ymax = median + 0.003),
                   colour = "white", size = size_days_ahead,
                   plot_data %>% filter(group == "ward")) +
    
    
    
    scale_colour_manual(values = ward_cols) +
    
    facet_wrap(~state, ncol = 4) +
    
    xlab("Horizon (days ahead)") +
    
    ylab("CRPS") +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.05)),
                       breaks = c(0, 0.5, 1.0)) +
    
    scale_x_continuous(breaks = c(0, 7, 14, 21),
                       expand = expansion(c(0, 0.05))) +
    
    
    coord_cartesian(ylim = c(0.0, 1.15)) +
    
    plot_theme +
    
    theme(panel.grid.major = element_blank(),
          panel.spacing.x = unit(0.5, "cm"),
          legend.position = "none")
  
  
  
  
  p_ICU_days_ahead <- ggplot() +
    
    geom_hline(yintercept = 0, size = 1) +
    geom_vline(xintercept = 0, size = 1) +
    
    geom_linerange(aes(x = days_ahead, ymin = lower, ymax = upper, group = quant, colour = quant),
                   size = size_days_ahead,
                   plot_data %>% filter(group == "ICU")) +
    
    geom_linerange(aes(x = days_ahead, ymin = median - 0.005, ymax = median + 0.005),
                   colour = "white", size = size_days_ahead,
                   plot_data %>% filter(group == "ICU")) +
    
    scale_colour_manual(values = ICU_cols) +
    
    facet_wrap(~state, ncol = 4) +
    
    xlab("Horizon (days ahead)") +
    
    ylab("CRPS") +
    
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    
    scale_x_continuous(breaks = c(0, 7, 14, 21),
                       expand = expansion(c(0, 0.05))) +
    
    
    coord_cartesian(ylim = c(0.0, 1.7)) +
    
    plot_theme +
    
    theme(panel.grid.major = element_blank(),
          panel.spacing.x = unit(0.5, "cm"),
          legend.position = "none")
  
  
  ward_state_order <- plot_data_summ %>% filter(group == "ward", quant == 95) %>% arrange(median) %>% pull(state) %>% rev()
  
  p_ward_summ <- ggplot() +
    
    geom_linerange(aes(y = state, xmin = lower, xmax = upper, group = quant, colour = quant),
                   size = 2.5,
                   position = position_nudge(y = -0.15),
                   plot_data_summ %>% filter(group == "ward") %>% mutate(state = factor(state, ward_state_order))) +
    
    geom_linerange(aes(y = state, xmin = median - 0.002, xmax = median + 0.002),
                   colour = "white",
                   size = 2.5,
                   position = position_nudge(y = -0.15),
                   plot_data_summ %>% filter(group == "ward") %>% mutate(state = factor(state, ward_state_order)))  +
    
    ggdist::stat_histinterval(aes(y = state, x = CRPS_forecast),
                              breaks = seq(0, 10, by = 0.01), show_interval = FALSE,
                              
                              fill = "black", colour = "white", scale = 0.7,
                              perf_days_ahead %>% filter(group == "ward") %>% mutate(state = factor(state, ward_state_order))) +
    
    scale_colour_manual(values = ward_cols) +
    
    ylab(NULL) + xlab("CRPS") +
    
    coord_cartesian(xlim = c(0, 1.0)) +
    scale_x_continuous(expand = expansion()) +
    
    plot_theme +
    
    theme(legend.position = "none",
          panel.grid.major.x = element_blank()) +
    
    ggtitle("Ward forecast performance", "")
  
  ICU_state_order <- plot_data_summ %>% filter(group == "ICU", quant == 95) %>% arrange(median) %>% pull(state) %>% rev()
  
  p_ICU_summ <- ggplot() +
    
    geom_linerange(aes(y = state, xmin = lower, xmax = upper, group = quant, colour = quant),
                   size = 2.5,
                   position = position_nudge(y = -0.15),
                   plot_data_summ %>% filter(group == "ICU") %>% mutate(state = factor(state, ICU_state_order))) +
    
    geom_linerange(aes(y = state, xmin = median - 0.002, xmax = median + 0.002),
                   colour = "white",
                   size = 2.5,
                   position = position_nudge(y = -0.15),
                   plot_data_summ %>% filter(group == "ICU") %>% mutate(state = factor(state, ICU_state_order)))  +
    
    ggdist::stat_histinterval(aes(y = state, x = CRPS_forecast),
                              breaks = seq(0, 10, by = 0.02), show_interval = FALSE,
                              
                              fill = "black", colour = "white", scale = 0.7,
                              perf_days_ahead %>% filter(group == "ICU") %>% mutate(state = factor(state, ICU_state_order))) +
    
    scale_colour_manual(values = ICU_cols) +
    
    ylab(NULL) + xlab("CRPS") +
    
    coord_cartesian(xlim = c(0, 1.5)) +
    scale_x_continuous(expand = expansion()) +
    
    plot_theme +
    
    theme(legend.position = "none",
          panel.grid.major.x = element_blank()) +
    
    ggtitle("ICU forecast performance", "")
  
  
  plot_void <- ggplot() + geom_blank() + theme_void()
  cowplot::plot_grid(
    p_ward_summ, plot_void, p_ward_days_ahead,
    p_ICU_summ, plot_void, p_ICU_days_ahead,
    
    ncol = 3, align = "hv", axis = "lrtb",
    rel_widths = c(1, 0.2, 2)
  )
  
  
  
  
  ggsave(
    "results/results_perf_summary.png",
    bg = "white",
    scale = 10 / 16,
    width = 16, height = 9
  )

}


