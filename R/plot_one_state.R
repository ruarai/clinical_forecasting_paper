
plot_one_state <- function(paper_forecasts_data, occupancy_data) {
  
  forecast_quants <- paper_forecasts_data$quants
  
  
  i_state <- "NSW"
  
  
  runs <- forecast_quants %>%
    filter(state == "NSW") %>%
    distinct(
      run_date,
      case_forecast_start
    ) %>%
    arrange(run_date) %>%
    
    mutate(id = row_number(),
           id_mod = (id - 1) %% 3,
           run_date = ymd(run_date))
  
  plot_data_mod <- forecast_quants %>%
    mutate(run_date = ymd(run_date)) %>%
    filter(state == i_state) %>%
    left_join(runs, by = c("run_date", "case_forecast_start")) %>%
    filter(date > case_forecast_start + ddays(7),
           date <= case_forecast_start + ddays(28))
  
  
  ward_known <- occupancy_data %>%
    filter(state == i_state)
  
  p_common <- list(
    plot_theme,
    scale_x_date(labels = scales::label_date_short(c("%Y", "%B"), sep = " "),
                 date_breaks = "months", expand = expansion()),
    scale_y_continuous(labels = scales::label_comma()),
    geom_blank(aes(y = 0)),
    xlab(NULL), ylab("Count"),
    facet_wrap(~id_mod, ncol = 1),
    geom_vline(aes(xintercept = case_forecast_start + ddays(8)),
               colour = annotation_colour,
               runs, linetype = 'dashed', alpha = 0.3),
    
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          strip.text = element_blank(),
          axis.line.x = element_blank(),
          panel.spacing.y = unit(0.5, "cm"),
          panel.grid.major = element_blank()),
    
    geom_hline(yintercept = 0, size = 1)
  )
  
  asterisk_runs <- runs %>%
    expand_grid(group = c("ward", "ICU")) %>%
    filter(
      case_when(
        id == 15 & group == "ward" ~ TRUE,
        id == 16 & group == "ward" ~ TRUE,
        id == 18 & group == "ward" ~ TRUE,
        id == 16 & group == "ICU" ~ TRUE,
        TRUE ~ FALSE
      )
    ) 
  
  plot_data_tile <- plot_data_mod %>%
    mutate(
      days_ahead = (date - case_forecast_start - ddays(7) / ddays(1))
    ) %>%
    filter(days_ahead > 7, days_ahead <= 14)
  
  
  p_ward <- ggplot(plot_data_mod %>% filter(group == "ward")) +
    
    
    geom_tile(aes(x = date, width = 1, y = -10, height = Inf),
              fill = annotation_colour, alpha = 0.04,
              plot_data_tile %>% filter(group == "ward", quant == 90)) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(run_date, quant), fill = quant),
                plot_data_mod %>% filter(group == "ward")) +
    
    geom_point(aes(x = date, y = count),
               color = "grey20",
               ward_known %>% filter(group == "ward"),
               
               size = 0.3, stroke = 0.4) +
    
    scale_fill_manual(values = color_list$ward) +
    
    p_common +
    geom_label(aes(x = case_forecast_start + ddays(10), y = 3280, label = id),
               label.r = unit(0, "cm"), label.size = 0, size = 3, colour = annotation_colour,
               runs) +
    
    geom_label(aes(x = case_forecast_start + ddays(13), y = 3180), label = "^",
               label.r = unit(0, "cm"), label.size = 0, size = 4, colour = annotation_colour,
               alpha = 0,
               asterisk_runs %>% filter(group == "ward")) +
    
    
    coord_cartesian(xlim = c(ymd("2022-03-11"), ymd("2022-08-25")),
                    ylim = c(0, 3600)) +
    
    ggtitle(str_c("Forecasted ward occupancy"))
  
  
  p_ward
  
  p_ICU <- ggplot(plot_data_mod %>% filter(group == "ICU")) +
    
    
    geom_tile(aes(x = date, width = 1, y = -10, height = Inf),
              fill = annotation_colour, alpha = 0.04,
              plot_data_tile %>% filter(group == "ICU", quant == 90)) +
    
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(run_date, quant), fill = quant),
                plot_data_mod %>% filter(group == "ICU")) +
    
    geom_point(aes(x = date, y = count),
               color = "grey20",
               ward_known %>% filter(group == "ICU"),
               
               size = 0.4, stroke = 0.4) +
    
    
    p_common +
    
    geom_label(aes(x = case_forecast_start + ddays(10), y = 140, label = id),
               label.r = unit(0, "cm"), label.size = 0, size = 3, colour = annotation_colour,
               runs) +
    
    geom_label(aes(x = case_forecast_start + ddays(13), y = 136), label = "^",
               label.r = unit(0, "cm"), label.size = 0, size = 4, colour = annotation_colour,
               alpha = 0,
               asterisk_runs %>% filter(group == "ICU")) +
    
    scale_fill_manual(values = color_list$ICU) +
    
    coord_cartesian(xlim = c(ymd("2022-03-11"), ymd("2022-08-25")),
                    ylim = c(0, 150)) +
    
    ylab("Count") +
    
    ggtitle(str_c("Forecasted ICU occupancy"))
  
  
  cowplot::plot_grid(
    p_ward, p_ICU, ncol = 1,
    
    align = "v"
  )
  
  ggsave(
    "results/results_one_state.png",
    bg = "white",
    scale = 10 / 16,
    width = 16, height = 14.5
  )
  
  
}
