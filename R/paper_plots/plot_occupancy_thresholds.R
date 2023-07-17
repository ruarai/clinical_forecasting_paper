


library(targets)
library(tidyverse)
library(lubridate)


paper_forecasts_data <- tar_read(paper_forecasts_data)

occupancy_data <- tar_read(occupancy_data)


forecast_trajs <- paper_forecasts_data$trajs


source("R/plots_common.R")


forecast_trajs_long <- forecast_trajs %>%
  pivot_longer(starts_with("sim_"),
               names_to = "sim", names_prefix = "sim_",
               values_to = "count_sim") %>%
  drop_na(count_sim) %>%
  
  mutate(forecast_start = case_forecast_start + days(7))


capacities_table <- occupancy_data %>%
  filter(date <= ymd("2022-06-15"),
         date >= ymd("2022-01-01")) %>%
  
  group_by(state, group) %>%
  slice_max(count, n = 1, with_ties = FALSE) %>%
  ungroup() %>% 
  select(state, group, count_capacity = count) %>%
  expand_grid(multiplier = 1) %>%
  mutate(count_capacity = count_capacity * multiplier)

true_capacity_reached <- occupancy_data %>% 
  filter(date >= ymd("2022-06-15")) %>% 
  left_join(capacities_table) %>%
  mutate(at_or_above_capacity = count > count_capacity) %>% 
  group_by(state, group, multiplier) %>%
  mutate(at_or_above_capacity = !cumprod(!at_or_above_capacity))

true_capacity_reached_points <- true_capacity_reached %>%
  filter(at_or_above_capacity & !lag(at_or_above_capacity))


sim_capacity_reached_probability <- forecast_trajs_long %>% 
  filter(forecast_start >= ymd("2022-06-10")) %>%
  arrange(date) %>% 
  left_join(capacities_table) %>%
  mutate(at_or_above_capacity = count_sim > count_capacity) %>% 
  group_by(state, group, forecast_start, run_date, sim, multiplier) %>%
  mutate(at_or_above_capacity = !cumprod(!at_or_above_capacity)) %>%
  
  group_by(state, group, date, forecast_start, run_date, multiplier) %>%
  summarise(p_at_or_above_capacity = sum(at_or_above_capacity) / n()) 

sim_capacity_reached_final <- sim_capacity_reached_probability %>%
  group_by(state, group, forecast_start, run_date, multiplier) %>%
  slice(n()) %>%
  ungroup() %>%
  left_join(true_capacity_reached %>% ungroup() %>% select(state, group, date, at_or_above_capacity, multiplier),
            by = c("forecast_start" = "date", "state", "group", "multiplier")) %>%
  filter(!at_or_above_capacity)




plot_data_capacity <- sim_capacity_reached_probability %>%
  ungroup() %>%
  filter(run_date <= ymd("2022-08-01")) %>%
  left_join(sim_capacity_reached_final %>% select(state, group, run_date, at_or_above_capacity)) %>%
  drop_na(at_or_above_capacity) %>%
  
  mutate(run_ix = match(run_date, unique(forecast_trajs$run_date)))


white_rect <- expand_grid(
  run_date = unique(sim_capacity_reached_probability$run_date),
  state = unique(sim_capacity_reached_probability$state),
  group = c("ward", "ICU")
) %>%
  filter(run_date <= ymd("2022-08-01")) %>% 
  mutate(run_ix = match(run_date, unique(forecast_trajs$run_date))) %>%
  anti_join(plot_data_capacity)

ggplot() +
  
  geom_hline(yintercept = 0, alpha = 0.5) + geom_hline(yintercept = 1, alpha = 0.5) +
  
  geom_line(aes(x = date, y = p_at_or_above_capacity, group = forecast_start),
            
            linewidth = 1,
            colour = ggokabeito::palette_okabe_ito(5),
            
            plot_data_capacity %>% filter(group == "ward")) +
  
  geom_vline(aes(xintercept = date),
             true_capacity_reached_points %>% filter(group == "ward")) +
  
  
  geom_rect(aes(xmin = ymd("2020-01-01"), xmax = ymd("2023-01-01"),
                ymin = -Inf, ymax = Inf),
            fill = "white", alpha = 1.0,
            white_rect %>% filter(group == "ward")) +
  
  geom_hline(yintercept = 0, alpha = 0.5) + geom_hline(yintercept = 1, alpha = 0.5) +
  
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(ymd("2022-06-01"), ymd("2022-08-20"))) +
  
  plot_theme +
  
  xlab(NULL) + ylab("Probability") +
  
  facet_grid(
    rows = vars(run_ix),
    cols = vars(state)
  ) +
  
  scale_y_continuous(breaks = c(0, 0.5, 1), expand = c(0, 0)) +
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%B"))) +
  
  theme(panel.spacing.y = unit(0.5, "cm"))



ggplot() +
  
  geom_hline(yintercept = 0, alpha = 0.5) + geom_hline(yintercept = 1, alpha = 0.5) +
  
  geom_line(aes(x = date, y = p_at_or_above_capacity, group = forecast_start),
            
            linewidth = 1,
            colour = ggokabeito::palette_okabe_ito(5),
            
            plot_data_capacity %>% filter(group == "ICU")) +
  
  geom_vline(aes(xintercept = date),
             linetype = "dashed",
             true_capacity_reached_points %>% filter(group == "ICU")) +
  
  
  geom_rect(aes(xmin = ymd("2020-01-01"), xmax = ymd("2023-01-01"),
                ymin = -Inf, ymax = Inf),
            fill = "white", alpha = 1.0,
            white_rect %>% filter(group == "ICU")) +
  
  geom_hline(yintercept = 0, alpha = 0.5) + geom_hline(yintercept = 1, alpha = 0.5) +
  
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(ymd("2022-06-01"), ymd("2022-08-20"))) +
  
  plot_theme +
  
  xlab(NULL) + ylab("Probability") +
  
  facet_grid(
    rows = vars(run_ix),
    cols = vars(state)
  ) +
  
  scale_y_continuous(breaks = c(0, 0.5, 1), expand = c(0, 0)) +
  scale_x_date(date_breaks = "months",
               labels = scales::label_date_short(format = c("%Y", "%B"))) +
  
  theme(panel.spacing.y = unit(0.5, "cm"))

ggsave("test.pdf")




fix_names <- . %>%
  mutate(group = factor(group, c("ward", "ICU"),
                        c("Ward occupancy threshold exceeded", "ICU occupancy threshold exceeded")))

second_axis <- sec_axis(
  trans = identity,
  breaks = plot_data_capacity %>% filter(state == "NSW") %>% pull(forecast_start) %>% unique(),
  labels = unique(plot_data_capacity$run_ix)
)

ggplot() +
  
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1) +
  
  geom_line(aes(x = date, y = p_at_or_above_capacity, group = forecast_start, colour = factor(run_ix %% 3)),
            alpha = 0.7, linewidth = 0.8,
            plot_data_capacity %>% fix_names) +
  
  geom_point(aes(x = date, y = p_at_or_above_capacity, group = forecast_start, colour = factor(run_ix %% 3)),
             alpha = 0.8, size = 1.2,
             plot_data_capacity %>% filter(date == forecast_start) %>% fix_names)  +
  
  geom_point(aes(x = date, y = p_at_or_above_capacity, group = forecast_start, colour = factor(run_ix %% 3)),
             alpha = 0.8, size = 1.0, pch = 1,
             plot_data_capacity %>% filter(date == forecast_start + days(21)) %>% fix_names) +
  
  # geom_linerange(aes(xmin = forecast_start, xmax = forecast_start + days(21),
  #                y = p_at_or_above_capacity,
  #                group = multiplier,
  #                colour = factor(multiplier)),
  #                linewidth = 1,
  #                sim_capacity_reached_final %>% filter(group == "ward")) +
  
  geom_vline(aes(xintercept = date), linetype = "11", size = 1,
             true_capacity_reached_points %>% fix_names) +
  
  facet_grid(rows = vars(state), cols = vars(group)) +
  
  scale_y_continuous(breaks = c(0, 0.5, 1), expand = c(0, 0)) +
  
  scale_x_date(date_breaks = "months",
               labels = scales::label_date(format = c("%d %B %Y")),
               sec.axis = second_axis) +
  
  ggokabeito::scale_colour_okabe_ito(order = c(1, 5, 9)) +
  
  coord_cartesian(ylim = c(0, 1), clip = FALSE) +
  
  xlab(NULL) + ylab("Probability") +
  
  plot_theme +
  
  theme(strip.placement = "outside",
        axis.text.x.top = element_text(colour = annotation_colour %>% shades::opacity(0.6), size = 8),
        legend.position = "none",
        panel.grid.major.y = element_blank(),
        strip.text = element_text(size = 11),
        strip.text.y = element_text(angle = 0),
        panel.spacing.y = unit(0.5, "cm"))



ggsave(
  "results_paper/results_occupancy_thresholds.png",
  bg = "white",
  scale = 10 / 16,
  width = 14, height = 10
)
