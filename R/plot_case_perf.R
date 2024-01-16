

plot_case_perf <- function(paper_forecasts_data, occupancy_data, performance_data) {

  forecast_quants <- paper_forecasts_data$quants
  
  
  
  perf_bias <- performance_data %>%
    rowwise() %>% 
    mutate(p1 = sum(unlist(forecast_count) <= true_count) / length(unlist(forecast_count)),
           p2 = sum(unlist(forecast_count) <= true_count - 1) / length(unlist(forecast_count)),
           bias = 1 - p1 - p2,
           
           sharpness = median(abs(forecast_count - forecast_median)) / 0.675,
           log_sharpness = median(abs(log(forecast_count + 1) - log(forecast_median + 1))) / 0.675)
  
  
  
  case_forecast_performance <- read_rds("data/case_performance_subset.rds") %>%
    
    # Hacks to join later
    mutate(origin = case_when(
      state != "VIC" & origin == "2022-07-12" ~ ymd("2022-07-13"),
      state != "VIC" & origin == "2022-07-19" ~ ymd("2022-07-20"),
      state == "QLD" & origin == "2022-07-24" ~ ymd("2022-07-26"),
      state == "VIC" & origin == "2022-04-11" ~ ymd("2022-04-13"),
      state == "VIC" & origin == "2022-03-22" ~ ymd("2022-03-23"),
      TRUE ~ origin
    ))
  
  case_forecast_performance_summ <- case_forecast_performance %>%
    group_by(state, origin) %>%
    summarise(case_CRPS_log = mean(crps_log), .groups = "drop")
  
  
  
  plot_data <- performance_data %>%
    group_by(state, group, case_forecast_start) %>%
    summarise(occ_CRPS_log = mean(z_log_CRPS_forecast)) %>%
    left_join(case_forecast_performance_summ, by = c("case_forecast_start" = "origin", "state"))
  
  plot_data %>% filter(is.na(case_CRPS_log))
  
  plot_data %>%
    filter(group == "ward") %>% 
    ggplot() +
    
    geom_smooth(aes(case_CRPS_log, y = occ_CRPS_log), method = "lm",
                colour = annotation_colour, alpha = 0.2) +
    geom_point(aes(x = case_CRPS_log, y = occ_CRPS_log),
               size = 0.5, stroke = 0.5) +
    
    lemon::facet_rep_wrap(~state, nrow = 2) +
    
    coord_fixed(xlim = c(0, 1.1), ylim = c(0, 1.1), expand = FALSE) +
    scale_x_continuous(breaks = c(0, 0.5, 1)) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    
    geom_abline(slope = 1, intercept = 0, linetype = "82", alpha = 0.5,
                colour = annotation_colour) +
    
    plot_theme +
    
    theme(panel.spacing.x = unit(0.5, "cm"),
          panel.spacing.y = unit(-0.25, "cm"),
          plot.margin = margin(0, 0.5, 0, 0.5, "cm")) +
    
    xlab("Case performance (CRPS)") +
    ylab("Ward performance (CRPS)")
  
  
  
  ggsave(
    "results/results_case_ward_CRPS.png",
    bg = "white",
    scale = 10 / 16,
    width = 14, height = 7
  )
  
  
  
  plot_data %>%
    filter(group == "ICU") %>% 
    ggplot() +
    
    geom_smooth(aes(case_CRPS_log, y = occ_CRPS_log), method = "lm",
                colour = annotation_colour, alpha = 0.2) +
    geom_point(aes(x = case_CRPS_log, y = occ_CRPS_log),
               size = 0.5, stroke = 0.5) +
    
    lemon::facet_rep_wrap(~state, nrow = 2) +
    
    coord_fixed(xlim = c(0, 1.1), ylim = c(0, 1.1), expand = FALSE) +
    scale_x_continuous(breaks = c(0, 0.5, 1)) +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    
    geom_abline(slope = 1, intercept = 0, linetype = "82", alpha = 0.5,
                colour = annotation_colour) +
    
    plot_theme +
    
    theme(panel.spacing.x = unit(0.5, "cm"),
          panel.spacing.y = unit(-0.25, "cm"),
          plot.margin = margin(0, 0.5, 0, 0.5, "cm")) +
    
    xlab("Case performance (CRPS)") +
    ylab("ICU performance (CRPS)")
  
  
  ggsave(
    "results/results_case_ICU_CRPS.png",
    bg = "white",
    scale = 10 / 16,
    width = 14, height = 7
  )
  
  
  
  perf_bias %>%
    rename(occ_bias = bias, occ_sharpness = sharpness) %>% 
    filter(group == "ward") %>% 
    select(state, group, run_date, case_forecast_start, date, occ_bias, occ_sharpness) %>%
    group_by(state, case_forecast_start) %>%
    summarise(occ_bias = mean(occ_bias)) %>%
    
    left_join(case_forecast_performance %>%
                group_by(state, case_forecast_start = origin) %>%
                summarise(bias = mean(bias)),
              by = c("case_forecast_start", "state")) %>%
    
    ggplot() +
    
    geom_smooth(aes(x = bias, y = occ_bias), method = "lm",
                colour = annotation_colour, alpha = 0.2) +
    geom_point(aes(x = bias, y = occ_bias),
               size = 0.5, stroke = 0.5) +
    
    lemon::facet_rep_wrap(~state, nrow = 2) +
    
    geom_abline(slope = 1, intercept = 0, linetype = "82", alpha = 0.5,
                colour = annotation_colour) +
    
    coord_fixed(xlim = c(-1.0, 1.0), ylim = c(-1.0, 1.0), expand = FALSE) +
    scale_x_continuous(breaks = c(-1, 0, 1)) +
    scale_y_continuous(breaks = c(-1, 0, 1)) +
    
    plot_theme +
    
    theme(panel.spacing.x = unit(0.5, "cm"),
          panel.spacing.y = unit(-0.25, "cm"),
          plot.margin = margin(0, 0.5, 0, 0.5, "cm")) +
    
    xlab("Case forecast bias") +
    ylab("Ward forecast bias")
  
  
  ggsave(
    "results/results_case_ward_bias.png",
    bg = "white",
    scale = 10 / 16,
    width = 14, height = 7
  )
}
