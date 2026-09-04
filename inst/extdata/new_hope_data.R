# See hyRefactor package for how this data was created.
# nolint start
source(system.file("extdata", "utils.R", package = "hydrogeofetch"))

data_dir <- file.path(tempdir(check = TRUE), "hydrogeofetch")

f <- "new_hope.gpkg"
f2 <- "new_hope_wbd.gpkg"

if(!file.exists(file.path(data_dir, "new_hope.gpkg"))) {

  download_pkg_data(f,
                    "https://doi-usgs.github.io/hydrogeofetch/data/new_hope.gpkg",
                    data_dir)

  download_pkg_data(f2,
                    "https://doi-usgs.github.io/hydrogeofetch/data/new_hope_wbd.gpkg",
                    data_dir)
}

proj <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
new_hope_catchment <- sf::read_sf(file.path(data_dir, f), "CatchmentSP")
new_hope_catchment <- sf::st_transform(new_hope_catchment, proj)
new_hope_flowline <- sf::read_sf(file.path(data_dir, f), "NHDFlowline_Network")
new_hope_flowline <- sf::st_transform(new_hope_flowline, proj)
new_hope_flowline <-suppressWarnings(sf::st_cast(new_hope_flowline, "LINESTRING"))
# new_hope_fline_ref <- sf::read_sf(file.path(data_dir, "new_hope_refactor.gpkg"))
# new_hope_fline_rec <- sf::read_sf(file.path(data_dir, "new_hope_reconcile.gpkg"))
# new_hope_catchment_rec <- sf::read_sf(file.path(data_dir, "new_hope_cat_rec.gpkg"))
new_hope_wbd <- sf::read_sf(file.path(data_dir, f2), "HUC12")
# nolint end
