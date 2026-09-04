# nolint start
source(system.file("extdata", "utils.R", package = "hydrogeofetch"))

if(!exists("work_dir")) {
  work_dir <- normalizePath(file.path(tempdir(check = TRUE), "hydrogeofetch"), mustWork = FALSE)
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
}

if(!file.exists(file.path(work_dir, "03_sub.gpkg"))) {

  download_pkg_data("03_sub.zip", "https://doi-usgs.github.io/hydrogeofetch/data/03_sub.zip", work_dir)

}

hr_source <- file.path(work_dir, "03_sub.gpkg")
hr_gpkg <- file.path(work_dir, "hr_data.gpkg")

if(!exists("layers")) {
  layers <- NULL
}
suppressWarnings(
  hr_data <- hydrogeofetch::get_nhdplushr(work_dir, layers = layers,
                                         out_gpkg = hr_gpkg,
                                         pattern = "03_sub.gpkg")
)
# hr_catchment <- sf::read_sf(hr_path, "NHDPlusCatchment")
# hr_catchment <- sf::st_transform(hr_catchment, proj)
#
# hr_flowline <- hydrogeofetch:::get_hr_data(hr_path, "NHDFlowline")
# hr_flowline <- sf::st_transform(hr_flowline, proj)
#
# hr_wbd <- sf::read_sf(system.file("extdata/new_hope_wbd.gpkg", package = "hydrogeofetch"),
#                       "HUC12")
# nolint end
