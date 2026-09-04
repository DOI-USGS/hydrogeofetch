source(system.file("extdata", "utils.R", package = "hydrogeofetch"))

data_dir <- file.path(tempdir(check = TRUE), "hydrogeofetch")

download_pkg_data("sample_natseamless.gpkg",
                  "https://doi-usgs.github.io/hydrogeofetch/data/sample_natseamless.gpkg",
                  data_dir)

sample_data <- file.path(data_dir, "sample_natseamless.gpkg")
