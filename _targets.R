library(targets)

library(tidyverse)
library(lubridate)

library(distributional)
library(tsibble)
library(fabletools)

library(lemon)
library(cowplot)
library(ggokabeito)
library(ggdist)

library(arrow)

options(tidyverse.quiet = TRUE)

source("R/get_performance_data.R")

source("R/plots_common.R")

source("R/plot_one_state.R")
source("R/plot_summary_CRPS.R")
source("R/plot_perf_over_time.R")
source("R/plot_case_perf.R")

source("R/plot_perf_sharpness_bias.R")
source("R/plot_perf_PIT.R")
source("R/plot_perf_over_time_ICU.R")

source("R/plot_supp_all_states.R")

list(
  tar_target(
    paper_forecasts_data,
    read_rds("data/paper_forecasts_data.rds"),
    format = "qs"
  ),
  
  tar_target(
    occupancy_data,
    read_csv("data/occupancy_compiled_Sep2022.csv", show_col_types = FALSE)
  ),
  
  tar_target(
    paper_performance_data,
    
    get_performance_data(
      paper_forecasts_data$trajs,
      occupancy_data
    )
  ),
  
  tar_target(figure_one_state, plot_one_state(paper_forecasts_data, occupancy_data)),
  tar_target(figure_summary_CRPS, plot_summary_CRPS(paper_performance_data)),
  tar_target(figure_perf_over_time, plot_perf_over_time(paper_performance_data, occupancy_data)),
  tar_target(figure_case_perf, plot_case_perf(paper_forecasts_data, occupancy_data, paper_performance_data)),
  tar_target(figure_perf_sharpness_bias, plot_perf_sharpness_bias(paper_performance_data, occupancy_data)),
  tar_target(figure_perf_PIT, plot_perf_PIT(paper_forecasts_data, occupancy_data)),
  tar_target(figure_perf_over_time_ICU, plot_perf_over_time_ICU(paper_performance_data, occupancy_data)),
  tar_target(figure_supp_all_states, plot_supp_all_states(paper_forecasts_data, occupancy_data, paper_performance_data))
)