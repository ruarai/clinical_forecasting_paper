
source("R/plots_common.R")

retro_trajs <- retro_forecasts_data$trajs %>%
  mutate(run_date = ymd(run_date))


forecast_trajs_long <- bind_rows(forecast_trajs, retro_trajs) %>%
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

plot_data <- empirical_coverage %>% 
  mutate(group = factor(group, levels = c("ward", "ICU"), labels = c("Ward", "ICU")),
         rate = seq(-10, -6, by = 0.5)[as.numeric(suffix)]) %>%
  mutate(rate = round(exp(rate) * 10000, 2),
         rate = replace_na(rate, 0)) %>%
  
  filter(state %in% c("TAS", "NT", "ACT")) %>%
  filter(suffix != "test_pf_b_3")

plot_data_ext <- plot_data %>%
  bind_rows(plot_data %>% 
              filter(prob == max(prob)) %>%
              mutate(prob = prob + step_size)) 


group_lines <- plot_data %>% 
  group_by(state, rate, group) %>% 
  summarise(n = sum(n_within) * step_size)

plot_group <- function(i_group) {
  plot_data_ext %>%
    filter(group == i_group) %>% 
    ggplot() +
    
    geom_step(aes(x = prob, y = n_within),
                   size = 1, colour = "grey60") +
    
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

plot_group("Ward") + ggtitle("Ward") +
  
  facet_grid(rows = vars(rate), cols = vars(state))



plot_data %>%
  left_join(group_lines) %>% 
  filter(group == "Ward") %>% 
  group_by(state, group, rate) %>% 
  summarise(err = sum(abs(n_within - n))) %>%
  
  ggplot() +
  geom_line(aes(x = rate, y = err)) +
  geom_point(aes(x = rate, y = err)) +
  
  facet_wrap(~state, scales = "free_y") +
  
  coord_cartesian(ylim = c(0, 300)) +
  
  xlab("Importation risk") +
  ylab("Calibration error") +
  
  plot_theme 
