

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
        axis.ticks = element_line(colour = "grey10"),
        axis.ticks.length = unit(5, "pt"),
        axis.line = element_line(colour = "grey10"),
        axis.text = element_text(size = 11, colour = "grey10"),
        axis.title = element_text(size = 12),
        text = element_text(family = "Helvetica"),
        strip.text = element_text(hjust = 0, size = 10),
        plot.title = element_text(size = 12))
)

plot_age_groups <- c("20-29", "50-59", "80+")



ward_base_colour <- "#b53aa0"
ICU_base_colour <- "#008200"
annotation_colour <- "#0072B2"


alpha_vals <- scales::rescale(rev(1/1.7^(1:8)), to = c(0.05, 0.99))
ward_cols <- c("#B53AA00D", "#B53AA011", "#B53AA018", "#B53AA024", "#B53AA039", "#B53AA05C", "#B53AA097", "#B53AA0FC")
ICU_cols <- c("#0082000D", "#00820011", "#00820018", "#00820024", "#00820039", "#0082005C", "#00820097", "#008200FC")
color_list <- list("ward" = ward_cols, "ICU" = ICU_cols)

rename_ages <- . %>% mutate(age_group = str_c("Ages ", age_group))



plot_dpi <- 300
plot_default_width <- 12
get_dpi <- function(width) {plot_dpi * plot_default_width / width}


