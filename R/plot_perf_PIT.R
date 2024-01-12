
plot_perf_PIT <- function(paper_forecasts_data, occupancy_data) {
  
  forecast_trajs <- paper_forecasts_data$trajs
  
  forecast_trajs_long <- forecast_trajs %>%
    pivot_longer(starts_with("sim_"),
                 names_to = "sim", names_prefix = "sim_",
                 values_to = "count_sim") %>%
    drop_na(count_sim)
  
  
  step_size <- 0.05
  
  forecast_quantiles <- forecast_trajs_long %>%
    
    group_by(state, date, group, run_date) %>%
    summarise(
      quants = list(
        tibble(
          prob = seq(0, 1, by = step_size),
          quant = quantile(count_sim, prob)
        )
      )
    ) %>%
    unnest(quants)
  
  
  filter_within <- function(count, quant, prob) {
    case_when(
      prob == 0 ~ count <= lead(quant),
      prob == 1 - step_size ~ count >= quant,
      TRUE ~ count >= quant & count <= lead(quant)
    )
  }
  
  empirical_coverage <- forecast_quantiles %>% 
    
    left_join(occupancy_data, by = c("date", "group", "state")) %>%
    drop_na(count) %>% 
    
    group_by(state, date, group, run_date) %>%
    #filter(count >= quant & count <= lead(quant, default = Inf)) %>%
    filter(filter_within(count, quant, prob)) %>% 
    mutate(weight = 1 / n()) %>% 
    ungroup() %>% 
    group_by(state, group, prob) %>%
    summarise(n_within = sum(weight))
  
  plot_data <- empirical_coverage %>% 
    mutate(group = factor(group, levels = c("ward", "ICU"), labels = c("Ward", "ICU")))
  
  
  group_lines <- plot_data %>% 
    group_by(state, group) %>% 
    summarise(n = sum(n_within) * step_size)
  
  plot_group <- function(i_group) {
    plot_data %>%
      filter(group == i_group) %>% 
      ggplot() +
      
      geom_linerange(aes(x = prob + step_size / 2, ymin = 0, ymax = n_within),
                     size = 3.5 * (step_size / 0.05), colour = "grey60") +
      
      geom_hline(aes(yintercept = n),
                 group_lines %>% filter(group == i_group),
                 colour = annotation_colour,
                 linetype = "dashed") +
      
      geom_hline(yintercept = 0) +
      
      facet_wrap(~state, ncol = 4) +
      
      scale_fill_manual(values = c("Ward" = ward_base_colour, "ICU" = ICU_base_colour),
                        name = NULL) +
      
      xlab("Quantile") + ylab("Frequency") +
      
      scale_y_continuous(expand = expansion()) +
      
      coord_cartesian(xlim = c(0, 1)) +
      
      plot_theme +
      theme(legend.position = "none",
            panel.spacing.x = unit(0.75, "cm"))
  }
  
  cowplot::plot_grid(
    plot_group("Ward") + ggtitle("Ward"), plot_group("ICU") + ggtitle("ICU"),
    ncol = 1, align = "v"
  )
  
  
  ggsave(
    "results/results_perf_PIT.png",
    bg = "white",
    scale = 10 / 16,
    width = 15, height = 13
  )
  
}



