

plot_perf_over_time_ICU <- function(performance_data, occupancy_data) {
  
  perf_bias <- performance_data %>% 
    rowwise() %>% 
    mutate(p1 = sum(unlist(forecast_count) <= true_count) / length(unlist(forecast_count)),
           p2 = sum(unlist(forecast_count) <= (true_count - 1)) / length(unlist(forecast_count)),
           bias = 1 - p1 - p2,
           
           sharpness = median(abs(forecast_count - forecast_median)) / 0.675,
           log_sharpness = median(abs(log(forecast_count + 1) - log(forecast_median + 1))) / 0.675)
  
  
  plot_perf_data <- perf_bias %>% 
    filter(group == "ICU",
           days_ahead > 14) %>%
    arrange(date) %>% 
    group_by(state) %>%
    mutate(ix = match(run_date, unique(run_date)),
           ix_mod_2 = ix %% 2) %>%
    ungroup() %>%
    
    bind_rows(
      tibble(
        state = "ACT", date = c(ymd("2022-04-15"), ymd("2022-04-17")), ix_mod_2 = 1
      )
    ) %>%
    mutate(state = state_nice_names[state])
  
  
  p_common <- list(
    plot_theme,
    theme(panel.grid.major = element_blank(),
          plot.margin = margin(l = 0.2, r = 0.4, b = 0.2, t = 0.1, unit = "cm")),
    xlab(NULL)
  )
  
  states <- unique(plot_perf_data$state)
  states <- states[order(states)]
  
  
  
  bias_colours <- c("#C70E89", "#0E6AC7")
  l_height <- 11
  
  
  plots <- map(
    states,
    function(i_state) {
      plot_perf_data_state <- plot_perf_data %>%
        filter(state == i_state)
      
      show_y_lab <- i_state %in% c("ACT", "SA", "NT", "VIC")
      
      p_occ <- ggplot() +
        
        
        geom_tile(aes(x = date, width = 1, y = -10, height = Inf, group = run_date),
                  fill = annotation_colour, alpha = 0.05,
                  plot_perf_data_state %>% filter(ix_mod_2 == 1)) +
        
        geom_step(aes(x = date, y = true_count),
                  plot_perf_data_state) +
        
        
        
        coord_cartesian(xlim = c(ymd("2022-03-15"), ymd("2022-09-01")),
                        ylim = c(0, NA)) +
        
        scale_y_continuous(breaks = scales::breaks_extended(3),
                           labels = scales::label_comma(),
                           expand = expansion(mult = c(0, 0.1))) +
        
        p_common +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        
        ylab("Count") +
        
        ggtitle(i_state)
      
      p_crps <- ggplot() +
        
        
        geom_tile(aes(x = date, width = 1, y = -10, height = Inf, group = run_date),
                  fill = annotation_colour, alpha = 0.05,
                  plot_perf_data_state %>% filter(ix_mod_2 == 1)) +
        
        
        geom_tile(aes(x = date, width = 1.2, y = 0, height = z_log_CRPS_forecast * 2, group = run_date),
                  fill = "grey20",
                  plot_perf_data_state) +
        
        scale_fill_distiller(palette = "YlGnBu", type = "seq", direction = 1) +
        
        
        coord_cartesian(xlim = c(ymd("2022-03-15"), ymd("2022-09-01")),
                        ylim = c(0, 1.0)) +
        
        scale_y_continuous(breaks = c(0, 0.5, 1), expand = c(0, 0)) +
        
        ylab("CRPS") +
        
        p_common +
        theme(axis.text.x = element_blank(),
              axis.ticks.x = element_blank())
      
      p_bias <- ggplot() +
        
        
        geom_tile(aes(x = date, width = 1, y = -10, height = Inf, group = run_date),
                  fill = annotation_colour, alpha = 0.05,
                  plot_perf_data_state %>% filter(ix_mod_2 == 1)) +
        
        
        geom_tile(aes(x = date, width = 1.2, y = bias / 2, height = bias, group = run_date,
                      fill = bias < 0),
                  plot_perf_data_state) +
        
        geom_hline(yintercept = 0, alpha = 0.8) +
        
        scale_fill_manual(values = as.character(bias_colours)) +
        
        
        coord_cartesian(xlim = c(ymd("2022-03-15"), ymd("2022-09-01")),
                        ylim = c(-1.0, 1.0), clip = "off") +
        
        scale_y_continuous(breaks = c(-1, 0, 1), expand = c(0, 0)) +
        
        ylab("Bias") +
        
        p_common +
        theme(legend.position = "none")
      
      if(!show_y_lab){
        p_occ <- p_occ + ylab(NULL)
        p_crps <- p_crps + ylab(NULL)
        p_bias <- p_bias + ylab(NULL)
      }
      
      
      list(p_occ, p_crps, p_bias)
      
      
    }
  )
  
  p <- map(
    list(c(1, 3, 5, 7), c(2, 4, 6, 8)),
    function(quad) {
      cowplot::plot_grid(
        plotlist = unlist(plots[quad], recursive = FALSE),
        align = "v",
        rel_heights = c(2, 1, 1.2, 2, 1, 1.2),
        ncol = 1
      )
    }
  ) %>%
    cowplot::plot_grid(
      plotlist = .,
      ncol = 2, align = "h"
    )
  
  p_legend <- ggplot() + geom_blank() + theme_void()
  
  p_with_legend <- cowplot::plot_grid(p, p_legend, rel_heights = c(1, 0.07), ncol = 1)
  
  ggsave(
    "results/results_perf_over_time_ICU_tall.png",
    p_with_legend,
    scale = 10 / 16,
    dpi = 300,
    width = 12, height = 14,
    bg = "white"
  )
  
}
