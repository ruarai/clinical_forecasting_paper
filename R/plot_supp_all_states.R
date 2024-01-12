
plot_supp_all_states <- function(paper_forecasts_data, occupancy_data, performance_data) {
  
  forecast_quants <- paper_forecasts_data$quants
  
  alpha_vals <- scales::rescale(rev(1/1.7^(1:8)), to = c(0.05, 0.99))
  ward_cols <- shades::opacity(ward_base_colour, alpha_vals)
  ICU_cols <- shades::opacity(ICU_base_colour, alpha_vals)
  color_list <- list("ward" = ward_cols, "ICU" = ICU_cols)
  
  
  runs <- forecast_quants %>%
    distinct(
      state,
      run_date,
      case_forecast_start
    ) %>%
    group_by(state) %>% 
    arrange(run_date) %>%
    
    mutate(id = row_number(),
           id_mod = (id - 1) %% 3,
           run_date = ymd(run_date))
  
  
  plot_data_mod <- forecast_quants %>%
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
  
  
  performance_data_plot <- performance_data %>%
    mutate(run_date = ymd(run_date)) %>% 
    
    left_join(
      runs %>% select(run_date, case_forecast_start, id_mod, state),
      by = c("run_date", "case_forecast_start", "state")
    )
  
  
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
    
  )
  
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
    function(i_state, i_group, y_upper, y_label, y_label_asterisk) {
      
      
      performance_data_plot_state_group <- performance_data_plot %>%
        filter(state == i_state, group == i_group)
      
      p_grid <- map(
        0:2,
        function(i_mod) {
          
          p_occ <- ggplot(plot_data_mod %>% filter(state == i_state, group == i_group, id_mod == i_mod)) +
            
            
            geom_tile(aes(x = date, width = 1, y = -10, height = Inf),
                      fill = ggokabeito::palette_okabe_ito(5), alpha = 0.04,
                      performance_data_plot_state_group %>% filter(id_mod == i_mod) %>%
                        filter(days_ahead > 7, days_ahead <= 14)) +
            
            geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = interaction(run_date, quant), fill = quant)) +
            
            geom_point(aes(x = date, y = count),
                       color = "grey20",
                       occupancy_data %>% filter(state == i_state, group == i_group),
                       
                       size = 0.3, stroke = 0.4) +
            
            scale_fill_manual(values = color_list[[i_group]]) +
            
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
            
            
            coord_cartesian(xlim = c(ymd("2022-03-15"), ymd("2022-08-20")),
                            ylim = c(0, y_upper)) +
            
            theme(axis.text.x = element_blank(),
                  axis.ticks.x = element_blank())
          
          
          p_CRPS <- performance_data_plot_state_group %>%
            filter(id_mod == i_mod) %>% 
            ggplot() +
            
            
            geom_tile(aes(x = date, width = 1, y = -10, height = Inf),
                      fill = ggokabeito::palette_okabe_ito(5), alpha = 0.03,
                      performance_data_plot_state_group %>% filter(id_mod == i_mod) %>%
                        filter(days_ahead > 7, days_ahead <= 14)) +
            
            geom_line(aes(x = date, y = z_log_CRPS_forecast, group = run_date)) +
            
            p_common +
            
            geom_vline(aes(xintercept = case_forecast_start + ddays(7)),
                       colour = ggokabeito::palette_okabe_ito(5),
                       runs %>% filter(state == i_state, id_mod == i_mod), linetype = 'dashed', alpha = 0.3) +
            
            ylab("CRPS") +
            
            coord_cartesian(xlim = c(ymd("2022-03-15"), ymd("2022-08-20")),
                            ylim = c(0, max(performance_data_plot_state_group$z_log_CRPS_forecast)))
          
          
          
          cowplot::plot_grid(
            p_occ, p_CRPS, ncol = 1,
            rel_heights = c(2, 1),
            
            align = "v"
          )
        }
      ) %>%
        cowplot::plot_grid(plotlist = ., ncol = 1, align = "v")
      
      cowplot::plot_grid(
        ggplot() + theme_minimal() + ggtitle(str_c("Forecasted ", i_group, " occupancy \u2012 ", state_nice_names[i_state])) +
          theme(plot.title = element_text(margin = margin())),
        p_grid,
        rel_heights = c(0.04, 1),
        ncol = 1
      )
    }
  )
  
  
  for (i in 1:nrow(plot_meta)) {
    plot(plots[[i]])
    
    ggsave(
      str_c("results/supp/past_forecasts_", plot_meta$i_state[[i]], "_", plot_meta$i_group[[i]], ".png"),
      
      scale = 10 / 16,
      width = 16, height = 14.5,
      bg = "white"
    )
  }
  
  
  
  
  
  
  
  
  
}

