

state_nice_names <- c(
  "SA" = "South Australia",
  "VIC" = "Victoria",
  "NSW" = "New South Wales",
  "QLD" = "Queensland",
  "ACT" = "Australian Capital Territory",
  "WA" = "Western Australia",
  "NT" = "Northern Territory",
  "TAS" = "Tasmania"
)


plot_theme <- list(
  theme_minimal(),
  theme(panel.grid = element_blank(),
        panel.grid.major = element_line(colour = "grey80", linetype = "dotted"),
        axis.ticks = element_line(colour = "grey60"),
        axis.ticks.length = unit(5, "pt"),
        axis.line = element_line(colour = "grey40"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        text = element_text(family = "Helvetica"),
        strip.text = element_text(hjust = 0, size = 10),
        plot.title = element_text(size = 12))
)

plot_age_groups <- c("20-29", "50-59", "80+")



ward_base_colour <- "#b53aa0"
ICU_base_colour <- "#008200"

rename_ages <- . %>% mutate(age_group = str_c("Ages ", age_group))