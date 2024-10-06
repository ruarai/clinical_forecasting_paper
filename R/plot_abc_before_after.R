
plot_abc_before_after <- function(
  before_after_trajectories,
  occupancy_data
) {
  trajectories_sub <- before_after_trajectories %>%
    
    mutate(class = factor(class,
                          c("prior_without_adj", "prior_with_adj", "posterior"),
                          c("  ", "   ", " ")))
  
  date_case_forecast_start <- max(trajectories_sub$date) - days(28)
  date_forecast_start <- max(trajectories_sub$date) - days(21)
  
  trajectories_sub_sample <- trajectories_sub %>%
    filter(sample >= 10, sample < 20)
  
  
  quants_sub <- trajectories_sub %>%
    select(sample, group, state, class, date, count) %>% 
    pivot_wider(names_from = sample, names_prefix = "sim_", values_from = "count") %>% 
    make_results_quants()
  
  p_common <- list(
    plot_theme,
    scale_x_date(labels = scales::label_date_short(c("%Y", "%B"), sep = " "),
                 date_breaks = "months"),
    scale_y_continuous(labels = scales::label_comma()),
    geom_blank(aes(y = 0)),
    xlab(NULL), ylab("Count"),
    
    # geom_vline(xintercept = date_forecast_start,
    #            colour = annotation_colour,
    #            linetype = 'dashed', alpha = 0.3),
    # geom_vline(xintercept = date_case_forecast_start + days(1),
    #            colour = annotation_colour,
    #            linetype = '11', alpha = 0.3),
    
    theme(legend.position = "none",
          axis.text = element_text(size = 10),
          axis.line.x = element_blank(),
          strip.text = element_text(size = 14),
          plot.title = element_text(hjust = 1),
          panel.spacing.y = unit(0.5, "cm"),
          panel.grid.major = element_blank()),
    
    geom_hline(yintercept = 0, size = 0.5)
  )
  
  
  
  p_VIC_quants <- quants_sub %>% 
    filter(state == "VIC") %>% 
    
    ggplot() +
    annotate("rect",
             xmin = date_case_forecast_start + days(1), xmax = date_forecast_start,
             ymin = 0, ymax = Inf, fill = annotation_colour, alpha = 0.1) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
    
    geom_line(aes(x = date, y = count, group = sample),
              alpha = 0.2,
              trajectories_sub_sample %>% 
                filter(state == "VIC")) +
    
    geom_point(aes(x = date, y = count),
               occupancy_data %>% filter(state == "VIC", group == "ward",
                                         date <= date_forecast_start)) +
    
    geom_point(aes(x = date, y = count),
               size = 0.2, colour = "white",
               occupancy_data %>% filter(state == "VIC", group == "ward",
                                         date <= date_forecast_start)) +
    
    facet_wrap(~class, ncol = 1) +
    
    scale_fill_manual(values = color_list$ward) +
    
    coord_cartesian(ylim = c(0, 1200),
                    xlim = c(ymd("2022-05-15", NA))) +
    
    p_common +
    
    #ggtitle("Victoria") +
    
    theme(plot.margin = margin(r = 0.5, t = 0.25, l = 0.25, b = 0.25, unit = "cm"))
  
  p_VIC_quants
  
  
  p_NT_quants <- quants_sub %>% 
    filter(state == "NT") %>% 
    
    ggplot() +
    annotate("rect",
             xmin = date_case_forecast_start + days(1), xmax = date_forecast_start,
             ymin = 0, ymax = Inf, fill = annotation_colour, alpha = 0.1) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
    geom_ribbon(aes(x = date, ymin = lower, ymax = upper, group = quant, fill = quant)) +
    
    geom_line(aes(x = date, y = count, group = sample),
              alpha = 0.2,
              trajectories_sub_sample %>% 
                filter(state == "NT")) +
    
    geom_point(aes(x = date, y = count),
               occupancy_data %>% filter(state == "NT", group == "ward",
                                         date <= date_forecast_start)) +
    
    geom_point(aes(x = date, y = count),
               size = 0.2, colour = "white",
               occupancy_data %>% filter(state == "NT", group == "ward",
                                         date <= date_forecast_start)) +
    
    facet_wrap(~class, ncol = 1) +
    
    scale_fill_manual(values = color_list$ward) +
    
    coord_cartesian(ylim = c(0, 100),
                    xlim = c(ymd("2022-05-15", NA))) +
    
    p_common +
    
    #ggtitle("Northern Territory") +
    
    theme(plot.margin = margin(l = 0.5, t = 0.25, b = 0.25, r = 0.25, unit = "cm"))
  
  
  
  cowplot::plot_grid(
    p_VIC_quants, p_NT_quants, ncol = 2
  ) + 
    cowplot::draw_text("A \u2013 Model outputs (without H, L)", x = 0.03, y = 0.95, hjust = 0) +
    cowplot::draw_text("B \u2013 Model outputs (with H, L)", x = 0.03, y = 0.64, hjust = 0) +
    cowplot::draw_text("C \u2013 Fit model outputs (with H, L)", x = 0.03, y = 0.32, hjust = 0)
  
  ggsave(
    "results/results_ABC_before_after.png",
    bg = "white",
    scale = 10 / 16,
    width = 16, height = 12
  )
  
  ggsave(
    "results/results_ABC_before_after.pdf",
    bg = "white",
    scale = 10 / 16,
    width = 16, height = 12
  )

}


make_results_quants <- function(tbl, probs = c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), na.rm = FALSE) {
  data_matrix <- tbl %>%
    select(starts_with("sim_")) %>%
    as.matrix()
  
  id_tbl <- tbl %>%
    select(!starts_with("sim_"))
  
  medians <- data_matrix %>%
    matrixStats::rowMedians() %>%
    tibble(median = .)
  
  quant_probs <- c(rev(1 - probs) / 2, 0.5 + probs / 2)
  quant_names <- c(str_c("lower_", rev(probs) * 100), str_c("upper_", probs * 100))
  
  quants <- data_matrix %>%
    matrixStats::rowQuantiles(probs = quant_probs, na.rm = na.rm) %>%
    `colnames<-`(quant_names) %>%
    as_tibble() %>%
    bind_cols(id_tbl, medians, .) %>%
    pivot_longer(cols = -all_of(c(colnames(id_tbl), colnames(medians))),
                 names_to = c("type", "quant"),
                 names_sep = "_") %>%
    pivot_wider(names_from = "type",
                values_from = "value") %>%
    
    mutate(quant = factor(quant, levels = as.character(probs * 100)) %>% fct_rev())
  
  quants
}








