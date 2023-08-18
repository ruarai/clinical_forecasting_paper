
source("R/plots_common.R")

retro_trajs <- tar_read(retro_forecasts_data)$trajs %>%
  mutate(run_date = ymd(run_date))


forecast_trajs_long <- retro_trajs %>%
  pivot_longer(starts_with("sim_"),
               names_to = "sim", names_prefix = "sim_",
               values_to = "count_sim") %>%
  drop_na(count_sim)


step_size <- 0.05

forecast_quantiles <- forecast_trajs_long %>%
  
  group_by(state, date, group, run_date, suffix) %>%
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
  
  left_join(tar_read(occupancy_data_total), by = c("date", "group", "state")) %>%
  drop_na(count) %>% 
  
  group_by(state, date, group, suffix, run_date) %>%
  #filter(count >= quant & count <= lead(quant, default = Inf)) %>%
  filter(filter_within(count, quant, prob)) %>% 
  mutate(weight = 1 / n()) %>% 
  ungroup() %>% 
  group_by(state, group, suffix, prob) %>%
  summarise(n_within = sum(weight))


clean_for_plots <- . %>%
  mutate(group = factor(group,
                        c("ward", "ICU"),
                        labels = c("Ward", "ICU")),
         suffix = factor(suffix,
                         c("test_pf_b_baseline", "test_abc_2"),
                         c("Old model", "New model")))

plot_data <- empirical_coverage %>%
  
  clean_for_plots()
plot_data_ext <- plot_data %>%
  bind_rows(plot_data %>% 
              filter(prob == max(prob)) %>%
              mutate(prob = prob + step_size)) 



group_lines <- plot_data %>% 
  group_by(state, suffix, group) %>% 
  summarise(n = sum(n_within) * step_size)

plot_group <- function(i_group) {
  plot_data_ext %>%
    filter(group == i_group) %>% 
    ggplot() +
    
    geom_step(aes(x = prob, y = n_within, group = suffix, colour = suffix),
              size = 1) +
    
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
    theme(legend.position = "bottom",
          panel.spacing.x = unit(1, "cm"))
}



plot_group("Ward") + ggtitle("Ward") +
  
  facet_wrap(~state, ncol = 2)

plot_group("ICU") + ggtitle("ICU") +
  
  facet_wrap(~state, ncol = 2)


plot_data_summ <- plot_data %>%
  left_join(group_lines) %>% 
  filter(group == "ward") %>% 
  group_by(state, group, suffix) %>% 
  summarise(err = sum(abs(n_within - n)))

plot_data_summ %>%
  
  ggplot() +
  
  geom_path(aes(y = suffix, x = err, colour = state, group = state)) +
  
  coord_cartesian(xlim = c(0, 400)) +
  
  xlab("Importation risk") +
  ylab("Calibration error") +
  
  plot_theme 
