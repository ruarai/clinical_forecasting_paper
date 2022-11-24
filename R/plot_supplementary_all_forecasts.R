
plot_supplementary_all_forecasts <- function(plots) {
  cairo_pdf("results/supplementary_all_forecasts.pdf",
            width = 18 / 2.54, height = 18 / 2.54, onefile = TRUE)
  for (i in 1:length(plots)) {
    plot(plots[[i]])
  }
  dev.off()
}