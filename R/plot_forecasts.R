

plot_single_forecast <- function(
  forecast_quants, occupancy_data,
  i_state, i_group, i_suffix, ylim
) {
  
  
  ward_base_colour <- "#b53aa0"
  ICU_base_colour <- "#008200"
  
  source("R/plots_common.R")
  
  ward_cols <- shades::opacity(ward_base_colour, c(0.3, 0.6, 1))
  ICU_cols <- shades::opacity(ICU_base_colour, c(0.3, 0.6, 1))
  color_list <- list("ward" = ward_cols, "ICU" = ICU_cols)
  
  
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
    
    coord_cartesian(xlim = c(ymd("2022-02-01"), ymd("2022-10-25")),
                    ylim = ylim) +
    
    geom_blank(aes(y = 0)) +
    
    xlab(NULL) + ylab("Count") +
    
    theme(legend.position = "none",
          strip.text = element_blank(),
          axis.line = element_line()) +
    
    ggtitle(str_c("Forecasted ", i_group, " occupancy"))
}


plot_forecasts <- function(
  forecast_quants,
  occupancy_data
) {
  
  
  
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
  
  plots <- pmap(state_plot_lims,
                function(state, ward, ICU, suffix) {
                  
                  p_ward <- plot_single_forecast(forecast_quants, occupancy_data, state, "ward", suffix, c(0, ward))
                  p_ICU <- plot_single_forecast(forecast_quants, occupancy_data, state, "ICU", suffix, c(0, ICU))
                  
                  cowplot::plot_grid(
                    p_ward,
                    p_ICU,
                    
                    ncol = 1, align = 'v', axis = 'tb'
                  )
                  
                  
                  
                  
                  p <- cowplot::plot_grid(
                    p_ward + ggtitle(str_c(state, " \u2013 Forecasted ward occupancy")),
                    p_ICU + ggtitle(str_c(state, " \u2013 Forecasted ICU occupancy")),
                    
                    ncol = 1, align = 'v', axis = 'tb'
                  )
                  
                  p
                  
                })
}
