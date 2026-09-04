# See hyRefactor package for how this data was created.
# nolint start
source(system.file("extdata", "utils.R", package = "hydrogeofetch"))

data_dir <- file.path(tempdir(check = TRUE), "hydrogeofetch")

f <- "walker.gpkg"

if(!file.exists(file.path(data_dir, f))) {
  download_pkg_data(f, "https://doi-usgs.github.io/hydrogeofetch/data/walker.gpkg", data_dir)
}

proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
walker_catchment <- sf::read_sf(file.path(data_dir, f), "CatchmentSP")
walker_catchment <- sf::st_transform(walker_catchment, proj)
walker_flowline <- sf::read_sf(file.path(data_dir, f), "NHDFlowline_Network")
walker_flowline <- sf::st_transform(walker_flowline, proj)
walker_flowline <-suppressWarnings(sf::st_cast(walker_flowline, "LINESTRING"))
# nolint end
# walker_fline_ref <- sf::read_sf(file.path(data_dir, "walker_refactor.gpkg"))
# walker_fline_rec <- sf::read_sf(file.path(data_dir, "walker_reconcile.gpkg"))
# walker_catchment_rec <- sf::read_sf(file.path(data_dir, "walker_cat_rec.gpkg"))
