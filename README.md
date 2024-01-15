## Figure plotting code for COVID-19 hospital burden forecasts

This repository contains the minimal code required to reproduce the figures as presented in "Forecasting COVID-19 hospital burden to support the Australian public health response".

Note that this repository does not include the forecasting model itself, which can be found in the following repositories: [clinical_forecast](https://github.com/ruarai/clinical_forecasts) and [curvemush](https://github.com/ruarai/curvemush).

This project uses [targets](https://books.ropensci.org/targets/) to handle basic reproducibility. To reproduce all figures, open the project in R and run the following code:

```R
# Install the requisitive R packages, if necessary:
install.packages(
  c("tidyverse", "targets",
    
    "distributional", "tsibble", "fabletools",
    
    "lemon", "cowplot", "ggokabeito", "ggdist", 
    
    "arrow")
)

# Run the `targets` workflow to produce all figures:
targets::tar_make()

# Outputs may be found in `results`
```

### Data

This repository includes a number of input data files:

- `data/paper_forecasts_data.rds` - an RDS file containing all forecasts examined in the paper, with both trajectories and pointwise credible intervals
- `data/occupancy_compiled_Sep2022.csv` - the occupancy data used to calculate the performance of the forecasts, retrieved from https://github.com/M3IT/COVID-19_Data
- `data/case_performance.parquet` - parquet file containing summary performance data for the ensemble case forecasts used as input to the model
- `data/paper_forecast_runs/` - directory containing diagnostic plots as produced for each forecast throughout the study period





