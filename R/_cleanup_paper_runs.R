
file.copy(forecast_meta$fc_dir, "data/paper_forecast_runs/", recursive = TRUE)

files_to_delete <- list.files("data/paper_forecast_runs/", pattern = "*.csv", recursive = TRUE, full.names = TRUE)
file.remove(files_to_delete)
files_to_delete <- list.files("data/paper_forecast_runs/", pattern = "*.qs", recursive = TRUE, full.names = TRUE)
file.remove(files_to_delete)
files_to_delete <- list.files("data/paper_forecast_runs/", pattern = "*.fst", recursive = TRUE, full.names = TRUE)
file.remove(files_to_delete)


files_to_delete <- list.files("data/paper_forecast_runs/", pattern = "*absenteeism*", recursive = TRUE, full.names = TRUE)
file.remove(files_to_delete)

x <- list.dirs("data/paper_forecast_runs/")
file.remove(x[str_detect(x, "archive")])

files_to_delete <- list.files("data/paper_forecast_runs/", pattern = "*.png", recursive = TRUE, full.names = TRUE)
file.remove(files_to_delete)

files_to_delete <- list.files("data/paper_forecast_runs/", pattern = "*.zip", recursive = TRUE, full.names = TRUE)
file.remove(files_to_delete)


files_to_delete <- list.files("data/paper_forecast_runs/", pattern = "*_cw_*", recursive = TRUE, full.names = TRUE)
file.remove(files_to_delete)
