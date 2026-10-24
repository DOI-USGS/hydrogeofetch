# Provenance: the New Hope 3DHP flowline sample covers the drainage basin of
# the New Hope sample's outlet, NHDPlusV2 COMID 8897784. It is every 3DHP
# flowline within 100 m of the NLDI basin plus the rest of each mainstem those
# flowlines are on, following 3dhp_yahara_flowlines.R. The New Hope
# ref/mainstems are 2408056 (New Hope River), 2408627 (Northeast Creek),
# 2408847 (Little Creek) and 2408909 (Morgan Creek). 3DHP featuredate as
# served Oct 2026.
#
# basin <- hydrogeofetch::get_nldi_basin(list(featureSource = "comid",
#                                             featureID = 8897784))
#
# area <- hydrogeofetch::get_3dhp(basin, type = "flowline", buffer = 100)
#
# full <- hydrogeofetch::get_3dhp(ids = unique(area$mainstemid[!is.na(area$mainstemid)]),
#                                 type = "flowline")
#
# keep <- c("id3dhp", "mainstemid", "hydrosequence", "dnhydrosequence",
#           "divergence", "lengthkm")
#
# # flowlines with no mainstemid are kept from the area fetch
# out <- rbind(full[, keep], area[is.na(area$mainstemid), keep])
# out <- out[!duplicated(out$id3dhp), ]  # 2710 flowlines, 886 mainstems
#
# sf::write_sf(out, "docs/data/new_hope_3dhp.gpkg")

source(system.file("extdata", "utils.R", package = "hydrogeofetch"))

data_dir <- file.path(tempdir(check = TRUE), "hydrogeofetch")

# Attempt the hosted download. The file is served from the pkgdown site once
# the hydrogeofetch commit lands; until then the URL 404s, so the attempt is
# allowed to fail quietly and the fallback below supplies the local copy.
try(
  download_pkg_data(
    "new_hope_3dhp.gpkg",
    "https://doi-usgs.github.io/hydrogeofetch/data/new_hope_3dhp.gpkg",
    data_dir),
  silent = TRUE)

downloaded <- file.path(data_dir, "new_hope_3dhp.gpkg")

# A 404 still leaves a file: req_perform() writes the error-page body to the
# download path before raising, so "no file" means "no readable GeoPackage",
# not merely a missing path. Drop a download that will not open as a layer.
if(file.exists(downloaded) &&
   inherits(try(sf::st_layers(downloaded), silent = TRUE), "try-error")) {
  unlink(downloaded)
}

# Fallback when the download left no file: HYDROGEOFETCH_CHECKOUT names a local
# hydrogeofetch checkout; this branch is unused once the file is hosted.
checkout <- Sys.getenv("HYDROGEOFETCH_CHECKOUT")

local_copy <- if(nzchar(checkout)) {
  file.path(checkout, "docs", "data", "new_hope_3dhp.gpkg")
} else {
  ""
}

sample_new_hope_3dhp <- if(file.exists(downloaded)) {
  downloaded
} else if(nzchar(local_copy) && file.exists(local_copy)) {
  local_copy
} else {
  downloaded
}
