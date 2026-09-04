source(system.file("extdata", "utils.R", package = "hydrogeofetch"))

data_dir <- file.path(tempdir(check = TRUE), "hydrogeofetch")

download_pkg_data("petapsco_flowlines.gpkg",
                  "https://doi-usgs.github.io/hydrogeofetch/data/petapsco_flowlines.gpkg",
                  data_dir)

sample_flines <- sf::read_sf(file.path(data_dir, "petapsco_flowlines.gpkg"))
