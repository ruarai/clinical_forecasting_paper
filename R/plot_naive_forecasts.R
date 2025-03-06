

plot_naive_forecasts <- function(paper_performance_data, occupancy_data) {
  runs <- paper_performance_data %>%
    distinct(
      state,
      run_date,
      case_forecast_start
    ) %>%
    
    # Append removed NT forecast to ensure correct ordering
    bind_rows(tibble(state = "NT", run_date = "2022-07-15")) %>% 
    
    group_by(state) %>% 
    arrange(run_date) %>%
    
    mutate(id = row_number(),
           id_mod = (id - 1) %% 3,
           run_date = ymd(run_date))
  
  
  plot_data_quants <- paper_performance_data %>%
    select(state, group, run_date, date, naive_count) %>%
    mutate(run_date = as_date(run_date)) %>% 
    expand_grid(quant = seq(0.2, 0.9, by = 0.1)) %>% 
    rowwise() %>% 
    mutate(upper = quantile(naive_count, 1 - quant / 2),
           lower = quantile(naive_count, quant / 2)) %>%
    
    left_join(runs)
  
  
  p_common <- list(
    plot_theme,
    scale_x_date(labels = scales::label_date_short(c("%Y", "%B"), sep = " "),
                 date_breaks = "months", expand = expansion()),
    scale_y_continuous(labels = scales::label_comma()),
    # geom_blank(aes(y = 0)),
    xlab(NULL), ylab("Count"),
    
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          strip.text = element_blank(),
          axis.line.x = element_blank(),
          panel.spacing.y = unit(0.5, "cm"),
          panel.grid.major = element_blank()),
    geom_hline(yintercept = 0, linewidth = 1)
  )
  
  plot_meta <- expand_grid(
    i_state = c("ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC", "WA"),
    i_group = c("ward", "ICU"),
  ) %>%
    mutate(i = row_number())
  
  plots <- plot_meta %>%
    pmap(function(i, i_state, i_group) {
      
      ggplot() +
        geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(run_date, quant), fill = factor(quant)),
                    plot_data_quants %>% filter(state == i_state, group == i_group),
                    alpha = 0.2) +
        
        geom_point(aes(x = date, y = count),
                   color = "grey20",
                   occupancy_data %>% filter(state == i_state, group == i_group),
                   
                   size = 0.3, stroke = 0.4) +
        
        geom_vline(aes(xintercept = case_forecast_start + ddays(8)),
                   colour = annotation_colour,
                   runs %>% filter(state == i_state),
                   linetype = 'dashed', alpha = 0.3) +
        
        facet_wrap(~id_mod, nrow = 3) +
        
        scale_fill_manual(values = color_list[[i_group]]) +
        
        coord_cartesian(xlim = c(ymd("2022-03-11"), ymd("2022-08-25"))) +
        
        p_common +
        
        ggtitle(str_c("Naive (random walk) model forecasts (", i_state, ", ", i_group,")"))
    })
  
  plot_meta %>%
    pmap(function(i, i_state, i_group) {
      ggsave(
        str_c("results/supp_naive/naive_", i_state, "_", i_group, ".png"),
        plot = plots[[i]],
        bg = "white",
        device = png,
        scale = 10 / 16,
        width = 16, height = 8
      )
    })
  
  
}

