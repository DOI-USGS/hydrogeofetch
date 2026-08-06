#' @title Plot NHDPlus
#' @description Given a list of outlets, get their basin boundaries and network and return a plot in
#' EPSG:3857 Web Mercator Projection.
#' @param outlets list of nldi outlets. Other inputs are coerced into nldi outlets, see details.
#' @param bbox object of class bbox with a defined crs. See examples.
#' @param streamorder integer only streams of order greater than or equal will be returned
#' @param nhdplus_data geopackage containing source nhdplus data (omit to download)
#' @param gpkg path and file with .gpkg ending. If omitted, no file is written.
#' @param overwrite passed on to \link{subset_nhdplus}.
#' @param plot_config list containing plot configuration, see details.
#' @param basemap character indicating which basemap type to use. Chose from:
#' \link[maptiles]{get_tiles}.
#' @param zoom integer passed on to \link[maptiles]{get_tiles}. This value will
#' override the default set by the package.
#' @param add boolean should this plot be added to an already built map.
#' @param actually_plot boolean actually draw the plot? Used to get data subset only.
#' @param flowline_only boolean only subset and plot flowlines only, default=FALSE
#' @param cache_data character path to rds file where all plot data can be cached.
#' If file doesn't exist, it will be created. If set to FALSE, all caching will
#' be turned off -- this includes basemap tiles.
#' @return data.frame plot data is returned invisibly in NAD83 Lat/Lon.
#' @details plot_nhdplus supports several input specifications. An unexported function "as_outlet"
#' is used to convert the outlet formats as described below.
#' \enumerate{
#'   \item if outlets is omitted, the bbox input is required and all nhdplus data
#'   in the bounding box is plotted.
#'   \item If outlets is a list of integers, it is assumed to be NHDPlus IDs (comids)
#'   and all upstream tributaries are plotted.
#'   \item if outlets is an integer vector, it is assumed to be all NHDPlus IDs (comids)
#'   that should be plotted. Allows custom filtering.
#'   \item If outlets is a character vector, it is assumed to be NWIS site ids.
#'   \item if outlets is a list containing only characters, it is assumed to be a list
#'   of nldi features and all upstream tributaries are plotted.
#'   \item if outlets is a data.frame with point geometry, a point in polygon match
#'   is performed and upstream with tributaries from the identified catchments is plotted.
#' }
#'
#' The \code{plot_config} parameter is a list with names "basin", "flowline", "outlets",
#'  "network_wtbd", and "off_network_wtbd".
#' The following shows the defaults that can be altered.
#' \enumerate{
#'   \item basin \preformatted{
#'     list(lwd = 1, col = NA, border = "black")}
#'   \item flowline \preformatted{
#'     list(lwd = 1, col = "blue")}
#'   \item outlets \preformatted{
#'    list(default = list(col = "black", border = NA, pch = 19, cex = 1),
#'         nwissite = list(col = "grey40", border = NA, pch = 17, cex = 1),
#'         huc12pp = list(col = "white", border = "black", pch = 22, cex = 1),
#'         wqp = list(col = "red", border = NA, pch = 20, cex = 1))}
#'   \item network_wtbd \preformatted{
#'     list(lwd = 1, col = "lightblue", border = "black")}
#'   \item off_network_wtbd \preformatted{
#'     list(lwd = 1, col = "darkblue", border = "black")}
#'
#' }
#'
#' If adding additional layers to the plot, data must be projected to EPSG:3857 with
#' `sf::st_transform(x, 3857)` prior to adding to the plot.
#'
#' @export
#' @importFrom graphics par
#' @importFrom hydroloom navigate_hydro_network
#' @examples
#' \donttest{
#' # Beware plot_nhdplus caches data to the default location.
#' # If you do not want data in "user space" change the default.
#' old_dir <- hydrogeofetch::hydrogeofetch_data_dir()
#' hydrogeofetch_data_dir(tempdir())
#'
#' plot_nhdplus("05428500")
#'
#' plot_nhdplus("05428500", streamorder = 2)
#'
#' plot_nhdplus(list(13293970, 13293750))
#'
#' source(system.file("extdata/sample_data.R", package = "hydrogeofetch"))
#'
#' plot_nhdplus(list(13293970, 13293750), streamorder = 3, nhdplus_data = sample_data)
#'
#' plot_nhdplus(list(list("comid", "13293970"),
#'                   list("nwissite", "USGS-05428500"),
#'                   list("huc12pp", "070900020603"),
#'                   list("huc12pp", "070900020602")),
#'              streamorder = 2,
#'              nhdplus_data = sample_data)
#'
#'
#' plot_nhdplus(sf::st_as_sf(data.frame(x = -89.36083,
#'                                      y = 43.08944),
#'                           coords = c("x", "y"), crs = 4326),
#'              streamorder = 2,
#'              nhdplus_data = sample_data)
#'
#' plot_nhdplus(list(list("comid", "13293970"),
#'                   list("nwissite", "USGS-05428500"),
#'                   list("huc12pp", "070900020603"),
#'                   list("huc12pp", "070900020602")),
#'              streamorder = 2,
#'              nhdplus_data = sample_data,
#'              plot_config = list(basin = list(lwd = 2),
#'                                 outlets = list(huc12pp = list(cex = 1.5),
#'                                                comid = list(col = "green"))))
#'
#' bbox <- sf::st_bbox(c(xmin = -89.43, ymin = 43, xmax = -89.28, ymax = 43.1),
#'                     crs = "+proj=longlat +datum=WGS84 +no_defs")
#'
#' fline <- sf::read_sf(sample_data, "NHDFlowline_Network")
#' comids <- hydroloom::navigate_hydro_network(fline, 13293970, "UT")
#'
#' plot_nhdplus(comids)
#'
#' #' # With Local Data
#' plot_nhdplus(bbox = bbox, nhdplus_data = sample_data)
#'
#' # With downloaded data
#' plot_nhdplus(bbox = bbox, streamorder = 3)
#'
#' # Can also plot on top of the previous!
#' plot_nhdplus(bbox = bbox, nhdplus_data = sample_data,
#'              plot_config = list(flowline = list(lwd = 0.5)))
#' plot_nhdplus(comids, nhdplus_data = sample_data, streamorder = 3, add = TRUE,
#'              plot_config = list(flowline = list(col = "darkblue")))
#'
#' hydrogeofetch::hydrogeofetch_data_dir(old_dir)
#' }

plot_nhdplus <- function(outlets = NULL, bbox = NULL, streamorder = NULL,
                         nhdplus_data = NULL, gpkg = NULL, plot_config = NULL,
                         basemap = "Esri.NatGeoWorldMap",
                         zoom = NULL, add = FALSE, actually_plot = TRUE,
                         overwrite = TRUE, flowline_only = NULL,
                         cache_data = NULL) {

  gt <- function(x) { sf::st_geometry(sf::st_transform(x, 3857)) }

  # Work with cache data
  save <- FALSE
  fetch <- TRUE
  if(!isFALSE(cache_data)) {
    if(!is.null(cache_data)) {
      if(file.exists(cache_data)) {
        pd <- readRDS(cache_data)
        fetch <- FALSE
      } else {
        save <- TRUE
      }
    }
    cache_tiles <- tile_cache_dir()
  } else {
    cache_tiles <- file.path(tempdir(check = TRUE), "tile_cache_dir")
  }

  if(fetch)
    pd <- get_plot_data(outlets, bbox, streamorder,
                        nhdplus_data, gpkg, overwrite, flowline_only)

  if(save) {
    saveRDS(pd, cache_data)
  }

  if(actually_plot) {
    st <- get_styles(plot_config)

    suppressWarnings({
      if(!add) {
        tryCatch({
          check_pkg("maptiles")
          check_pkg("mapsf")

          bb <- if(!is.null(pd$plot_bbox)) {
            plot_bbox_to_3857(pd$plot_bbox)
          } else {
            gt(pd[[get_plot_bound(pd)]])
          }

          if(is.null(zoom)) {
            zoom <- set_zoom(bb)
          }

          if(interactive())
            message(paste("Zoom set to:", zoom))

          tiles <- maptiles::get_tiles(bb, zoom = zoom, crop = TRUE,
                                       cachedir = cache_tiles,
                                       verbose = FALSE, provider = basemap)

          old_par <- par(mar = c(0, 0, 0, 0))
          on.exit(par(old_par))

          mapsf::mf_map(bb, type = "base", col = NA, border = NA)

          maptiles::plot_tiles(tiles, add = TRUE)

          mapsf::mf_map(bb, type = "base", add = TRUE, col = NA, border = NA)
          mapsf::mf_arrow()
          mapsf::mf_scale()

        },error = function(e) {
          warning(paste("Something went wrong trying to get the basemap. \n", e))
          return(NULL)
        })
      }
      # plot(gt(catchment), lwd = 0.5, col = NA, border = "grey", add = TRUE)
      if(!is.null(pd$basin))
        graphics::plot(gt(pd$basin), lwd = st$basin$lwd, col = st$basin$col,
                       border = st$basin$border, add = TRUE, reset = FALSE)
      if(!is.null(pd$network_wtbd))
        graphics::plot(gt(pd$network_wtbd),
                       lwd = st$network_wtbd$lwd,
                       col = st$network_wtbd$col,
                       border = st$network_wtbd$border, add = TRUE, reset = FALSE)
      if(!is.null(pd$off_network_wtbd))
        graphics::plot(gt(pd$off_network_wtbd),
                       lwd = st$off_network_wtbd$lwd,
                       col = st$off_network_wtbd$col,
                       border = st$off_network_wtbd$border, add = TRUE, reset = FALSE)
      graphics::plot(gt(pd$flowline),
                     lwd = st$flowline$lwd,
                     col = st$flowline$col,
                     add = TRUE, reset = FALSE)
      if(!is.null(pd$outlets)) {
        for(type in unique(pd$outlets$type)) {
          st_type <- "default"
          if(type %in% names(st$outlets)) st_type <- type
          graphics::plot(gt(pd$outlets[pd$outlets$type == type, ]),
                         col = st$outlets[[st_type]]$col,
                         pch = st$outlets[[st_type]]$pch,
                         bg = st$outlets[[st_type]]$bg,
                         cex = st$outlets[[st_type]]$cex, add = TRUE, reset = FALSE)
        }
      }
    })
  }
  return(invisible(pd))
}

tile_cache_dir <- function() {
  tile_cache_dir <- file.path(hydrogeofetch_data_dir(),
                       "tile_cache_dir")

  # Checks to see if tile_cache_dir is writable
  test_dir <- file.path(tile_cache_dir, "test")
  dir.create(test_dir, recursive = TRUE, showWarnings = FALSE)

  if(!dir.exists(test_dir)) {
    # just use tempdir() which is for sure writable.
    return(file.path(tempdir(check = TRUE), "tile_cache_dir")) #notest
  } else {
    # cleanup and return
    unlink(test_dir, recursive = TRUE)
    return(tile_cache_dir)
  }
}


get_styles <- function(plot_config) {
  conf <- list(basin = list(lwd = 1, col = NA, border = "black"),
               flowline = list(lwd = 1, col = "blue"),
               network_wtbd = list(lwd = 1, col = "lightblue", border = "black"),
               off_network_wtbd = list(lwd = 1, col = "darkblue", border = "black"),
               outlets = list(default = list(col = "black", bg = NA, pch = 19, cex = 1),
                              nwissite = list(col = "grey40", bg = NA, pch = 17, cex = 1),
                              huc12pp = list(col = "black", bg = "white", pch = 22, cex = 1),
                              wqp = list(col = "red", bg = NA, pch = 20, cex = 1)))

  validate_plot_config(plot_config)

  for(x in names(plot_config)) {
    for(y in names(plot_config[[x]])) {
      if(!is.list(plot_config[[x]][[y]])) {
        conf[[x]][[y]] <- plot_config[[x]][[y]]
      } else {
        if(!y %in% names(conf[[x]])) conf[[x]][[y]] <- conf[[x]][["default"]]
        for(z in names(plot_config[[x]][[y]])) {
          conf[[x]][[y]][[z]] <- plot_config[[x]][[y]][[z]]
        }
      }
    }
  }

  conf

}

get_plot_bound <- function(pd) {

  if(!is.null(pd$catchment)) {
    "catchment"
  } else if(!is.null(pd$basin)) {
    "basin"
  } else {
    "flowline"
  }

}

set_zoom <- function(pb) {
  bb <- st_bbox(pb)

  dim <- max(abs(bb[3] - bb[1]), abs(bb[4] - bb[2]))

  if(dim < 100000 & dim >= 60000) {
    10
  } else if(dim < 60000 & dim >= 20000) {
    11
  } else if(dim < 20000) {
    12
  } else if(dim < 150000 & dim >= 80000) {
    9
  } else if(dim < 200000 & dim >= 150000) {
    8
  } else {
    7
  }
}

validate_plot_config <- function(plot_config) {
  if(is.null(plot_config)) return(invisible(NULL))

  if(!all(names(plot_config) %in% c("basin", "flowline", "outlets", "network_wtbd", "off_network_wtbd")))
    stop(paste('Expected one or more of "basin", "flowline", "outlets", "network_wtbd", or "off_network_wtbd" in plot_config, got:',
               names(plot_config)))

  if("basin" %in% names(plot_config)) {
    if(!all(names(plot_config$basin) %in% c("lwd", "col", "border")))
      stop('Expected one ore more of "lwd", "col", or "border" in basins plot_config, got:',
           names(plot_config$basin))
  }

  if("waterbody" %in% names(plot_config)) {
    if(!all(names(plot_config$waterbody) %in% c("lwd", "col", "border")))
      stop('Expected one ore more of "lwd", "col", or "border" in basins plot_config, got:',
           names(plot_config$waterbody))
  }

  if("flowline" %in% names(plot_config)) {
    if(!all(names(plot_config$flowline) %in% c("lwd", "col")))
      stop('Expected one ore more of "lwd" and "col" in flowlines plot_config, got:',
           names(plot_config$flowline))
  }

  if(any(sapply(plot_config$outlets, function(x) !all(names(x) %in% c("col", "bg", "pch", "cex")))))
    stop(paste('Expected one or more of "col", "bg", "pch", or "cex" in outlets plot_config, got:',
               paste(unique(unlist(sapply(plot_config$outlets, names, simplify = TRUE))), collapse = ", ")))

  return(invisible(NA))
}

get_plot_data <- function(outlets = NULL, bbox = NULL,
                          streamorder = NULL, nhdplus_data = NULL,
                          gpkg = NULL, overwrite = TRUE, flowline_only = NULL, ...) {

  if(!is.null(outlets) & !is.null(bbox)) stop("Both bbox and outlets not supported.")
  if(!is.null(nhdplus_data)) {
    if(!file.exists(nhdplus_data)) {
      if(!is.null(gpkg) && nhdplus_data != gpkg)
        stop("couldn't find nhdplus_data and output data not requested by the same path.")
      nhdplus_data <- NULL
    } else if(!is.null(gpkg) && nhdplus_data == gpkg) {
      gpkg <- NULL
    }
  }

  if(!is.null(bbox)) {
    flowline <- dl_plot_data_by_bbox(bbox, nhdplus_data, gpkg, overwrite,
                                     streamorder = streamorder, flowline_only = flowline_only)
    catchment <- flowline$catchment
    basin <- flowline$basin
    nexus <- flowline$nexus
    wtbdy <- flowline$waterbody
    flowline <- flowline$flowline
    if (!is.null(wtbdy)){
      network_wtbd <- on_network(wtbdy,
                                 check_names(flowline, "on_off_network", tolower = TRUE))
      off_network_wtbd <- off_network(wtbdy,
                                      check_names(flowline, "on_off_network", tolower = TRUE))
    } else {
      network_wtbd <- NULL
      off_network_wtbd <- NULL
    }
  }

  comids <- NULL
  if(methods::is(outlets, "integer")) {
    comids <- outlets
    outlets <- NULL
  }

  if(!is.null(outlets)) {
    outlets <- as_outlets(outlets)
    outlet_type <- sapply(outlets, function(x) x$featureSource)
  }

  fline_layer = get_flowline_layer_name(nhdplus_data)
  catchment_layer <- get_catchment_layer_name(simplified = TRUE, nhdplus_data)

  if(!is.null(nhdplus_data) & !is.null(outlets)) {

    flowline <- sf::st_zm(sf::read_sf(nhdplus_data, fline_layer))
    flowline <- align_nhdplus_names(flowline)
    # For the "COMID" inputs we don't have to go to the NLDI,
    nexus <- lapply(outlets, get_comid_outlets, flowline = flowline)

    flowline <- sf::st_set_geometry(flowline, NULL)

    empty <- sapply(nexus, nrow) == 0
    outlets <- outlets[empty]
    nexus <- nexus[!empty]

    if(length(outlets) > 0) {
      nexus <- c(nexus, lapply(outlets, get_outlet_from_nldi))
    }

    if(is.null(tail(nexus, 1)[[1]])) {
      warning("something went wrong trying to get web service data.")
      return(NULL)
    }

    all_comids <- lapply(nexus, function(x) navigate_hydro_network(align_nhdplus_names(flowline), x$comid, "UT"))

    subsets <- subset_nhdplus(comids = unlist(all_comids), output_file = gpkg,
                              nhdplus_data = nhdplus_data, status = FALSE,
                              overwrite = overwrite, streamorder = streamorder,
                              flowline_only = flowline_only)
    if(is.null(subsets)) {
      warning("Something went wrong trying to get data subset, can't continue.")
      return(NULL)
    }

    flowline <- sf::st_zm(subsets[[fline_layer]])

    if("CatchmentSP" %in% names(subsets)) {
      catchment <- subsets[[catchment_layer]]

      basin <- do.call(rbind, lapply(all_comids, function(x, catchment_layer, subsets) {
        make_basin(subsets, catchment_layer, x)
      }, catchment_layer = catchment_layer, subsets = subsets))
    } else {
      catchment <- NULL
      basin <- NULL
    }
    if ('NHDWaterbody' %in% names(subsets) & !is.null(subsets$NHDWaterbody) & !is.character(subsets$NHDWaterbody)){
      names(flowline) <- tolower(names(flowline))
      network_wtbd <- on_network(subsets$NHDWaterbody,
                                 check_names(flowline, "on_off_network", tolower = TRUE))
      off_network_wtbd <- off_network(subsets$NHDWaterbody,
                                      check_names(flowline, "on_off_network", tolower = TRUE))
    } else {
      network_wtbd <- NULL
      off_network_wtbd <- NULL
    }

    nexus <- do.call(rbind, nexus)

  } else if(all(!is.null(outlets))) {
    basin <- do.call(rbind, lapply(outlets, get_nldi_basin))

    nexus <- do.call(rbind, lapply(outlets, get_outlet_from_nldi))

    if(is.null(basin) | is.null(nexus)) {
      warning("something went wrong trying to get data.")
      return(NULL)
    }

    if((ba <- max(sf::st_area(basin))) > units::set_units(1000^2, "m^2")) {
      if(is.null(flowline_only)) flowline_only <- TRUE
      if(is.null(streamorder)) streamorder <- auto_streamorder(ba)
    }

    nhd_data <- dl_plot_data_by_bbox(sf::st_bbox(basin), nhdplus_data,
                                     gpkg, overwrite, streamorder = streamorder,
                                     flowline_only = flowline_only)
    flowline <- align_nhdplus_names(nhd_data$flowline)

    flowline <- do.call(rbind, lapply(nexus$comid, function(x) {
      flowline[flowline$COMID %in% navigate_hydro_network(align_nhdplus_names(flowline), x, "UT"), ]
    }))

    catchment <- nhd_data$catchment
    if ('waterbody' %in% names(nhd_data) & !is.null(nhd_data$waterbody)){
      network_wtbd <- on_network(nhd_data$waterbody,
                                 check_names(flowline, "on_off_network", tolower = TRUE))
      off_network_wtbd <- off_network(nhd_data$waterbody,
                                      check_names(flowline, "on_off_network", tolower = TRUE))
    } else {
      network_wtbd <- NULL
      off_network_wtbd <- NULL
    }
  }

  if(!is.null(comids)) {
    if(is.null(nhdplus_data)) nhdplus_data <- "download"
    nhd_data <- subset_nhdplus(comids, nhdplus_data = nhdplus_data,
                               status = FALSE, overwrite = overwrite,
                               flowline_only = flowline_only)

    if(is.null(nhd_data)) {
      warning("Something went wrong trying to subset nhdplus data.")
      return(NULL)
    }

    if("CatchmentSP" %in% names(nhd_data)) {
      bbox <- sf::st_bbox(nhd_data$CatchmentSP)
      catchment <- nhd_data[[catchment_layer]]
      basin <- make_basin(nhd_data, catchment_layer = catchment_layer)
    } else {
      bbox <- sf::st_bbox(nhd_data$NHDFlowline_Network)
      catchment <- NULL
      basin <- NULL
    }
    flowline <- sf::st_zm(nhd_data$NHDFlowline_Network)
    nexus <- NULL
    if ('NHDWaterbody' %in% names(nhd_data) & !is.null(nhd_data$NHDWaterbody)){
      network_wtbd <- on_network(nhd_data$NHDWaterbody,
                                 check_names(flowline, "on_off_network", tolower = TRUE))
      off_network_wtbd <- off_network(nhd_data$NHDWaterbody,
                                      check_names(flowline, "on_off_network", tolower = TRUE))
    } else {
      network_wtbd <- NULL
      off_network_wtbd <- NULL
    }
  }

  if(!is.null(outlets)) {
    nexus["type"] <- outlet_type
  }

  if(is.null(bbox)) {
    if(is.null(basin)) {
      bbox <- sp_bbox(sf::st_transform(flowline, 4326))
    } else {
      bbox <- sp_bbox(sf::st_transform(basin, 4326))
    }
  }

  if(!is.null(streamorder) && "StreamOrde" %in% names(flowline)) {
    flowline <- flowline[flowline$StreamOrde >= streamorder, ]
  }
  return(list(plot_bbox = bbox, outlets = nexus, flowline = flowline,
              basin = basin, catchment = catchment, network_wtbd=network_wtbd,
              off_network_wtbd=off_network_wtbd))
}

auto_streamorder <- function(x) {
  if(x <= units::set_units(10000^2, "m^2")) return(1)
  if(x <= units::set_units(18000^2, "m^2")) return(2)
  if(x <= units::set_units(28000^2, "m^2")) return(3)
  return(4)
}

dl_plot_data_by_bbox <- function(bbox, nhdplus_data, gpkg, overwrite, streamorder = NULL, flowline_only = FALSE) {

  bbox <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(bbox), 4326))

  if(!is.null(nhdplus_data)) {
    source <- nhdplus_data
  } else {
    source <- "download"
  }
  if(is.null(flowline_only) && source == "download") {
    flowline_only <- TRUE
  } else if(is.null(flowline_only)) {
    flowline_only <- FALSE
  }

  d <- subset_nhdplus(bbox = bbox, output_file = gpkg, nhdplus_data = source,
                      simplified = TRUE, status = FALSE,
                      overwrite = overwrite, flowline_only = flowline_only,
                      streamorder = streamorder)

  if(is.null(d)) {
    warning("Something went wrong trying to subset NHDPlus data.")
    return(NULL)
  }

  d <- lapply(d, align_nhdplus_names)

  return(list(catchment = d$CatchmentSP,
              flowline = d$NHDFlowline_Network,
              waterbody = d$NHDWaterbody,
              nexus = NULL, basin = NULL))
}

sp_bbox <- function(g) {
  matrix(as.numeric(sf::st_bbox(g)),
         nrow = 2, dimnames = list(c("x", "y"),
                                   c("min", "max")))
}

plot_bbox_to_3857 <- function(b) {
  if(inherits(b, "bbox")) {
    crs <- sf::st_crs(b)
    if(is.na(crs)) crs <- sf::st_crs(4326)
    sfc <- sf::st_as_sfc(b)
    sf::st_crs(sfc) <- crs
  } else {
    sfc <- sf::st_as_sfc(sf::st_bbox(c(xmin = b[1, 1], ymin = b[2, 1],
                                       xmax = b[1, 2], ymax = b[2, 2]),
                                     crs = 4326))
  }
  sf::st_geometry(sf::st_transform(sfc, 3857))
}

make_basin <- function(x, catchment_layer, comids = NULL) {

  if(Sys.getenv("MAKE_BASIN") == "FALSE") {
    return(NULL)
  }

  x <- x[[catchment_layer]]
  if(!is.null(comids)) {
    x <- x[x$FEATUREID %in% comids, ]
  }
  sf::st_precision(x) <- ifelse(sf::sf_use_s2(), 1e8, 10000) # kills slivers -- doesn't work with s2?
  sf::st_sf(geom = sf::st_union(sf::st_geometry(x)))
}

get_comid_outlets <- function(o, flowline) {
  if(o[1] %in% c("comid", "COMID")) {
    f <- flowline[flowline$COMID == as.integer(o[2]), ][c("COMID", attr(flowline, "sf_column"))]
    names(f)[names(f) == "COMID"] <- "comid"
    return(make_point(f))
  }
  return(dplyr::tibble())
}

get_outlet_from_nldi <- function(outlet) {
  f <- get_nldi_feature(outlet)

  if(is.null(f)) return(NULL)

  make_point(f)
}

make_point <- function(x, crs = 4326) {
  sf::st_geometry(x) <-
    suppressWarnings(sf::st_centroid(sf::st_geometry(x)))
  x <- set_geom_name(x)
  x <- sf::st_transform(x, crs)
  return(dplyr::select(x, "comid"))
}

set_geom_name <- function(x, new_name = "geom") {
  g <- attr(x, "sf_column")
  names(x)[names(x) == g] <- new_name
  attr(x, "sf_column") <- new_name
  x
}

make_nwis_nldi_feature <- function(x) {
  if(length(x) > 1) stop("not and nwis outlet")
  if(!grepl("^USGS-.*", x)) x <- paste0("USGS-", x)
  if(!grepl("USGS-[0-9][0-9][0-9]", x)) stop("Found invalid NWIS ID trying to intperet outlet.")
  list(featureSource = "nwissite", featureID = x)
}

make_comid_nldi_feature <- function(x) {
  list(featureSource = "comid", featureID = as.character(x))
}

as_outlets <- function(o) {
  tryCatch({
    if(is.null(o)) return(NULL)

    if(!is.list(o) && all_int(o))
      return(list(subset = o))

    if((is.list(o) && all_int(o)) |
       is.character(o) |
       is.list(o) && !"sf" %in% class(o)) {
      return(tryCatch(lapply(o, individual_outlets),
                      error = function(e) list(individual_outlets(o))))

    }

    if("sf" %in% class(o)) {
      out <- c()
      for(i in seq_len(nrow(o))) {
        out <- c(out, make_comid_nldi_feature(discover_nhdplus_id(sf::st_geometry(o)[i])))
      }
    }
    return(list(check_nldi_feature(out, convert = FALSE)))
  }, error = function(f) {
    stop(paste0("Error trying to interpret outlet specification. Original error was:\n\n", f))
  })
}

# Works on individual outlets rather than sets of them.
individual_outlets <- function(o) {
  if(length(o) == 1 && all_int(o))
    return(make_comid_nldi_feature(o))

  if(is.character(o))
    o <- tryCatch({
      return(make_nwis_nldi_feature(o))
    }, error = function(f) as.list(o))


  if(is.list(o) && !"sf" %in% class(o)) {
    if(length(o) == 2 && !is.list(o[[1]])) o <- as.list(o)
    return(check_nldi_feature(o, convert = FALSE))
  }
}

all_int <- function(o) {
  tryCatch(all(sapply(o, function(x) x %% 1) == 0),
           error = function(f) FALSE)
}

# Define on-network and off-network waterbodies
on_network <- function(waterbody, flowline) {
  network_wtbdarea <- dplyr::filter(waterbody,
                                    COMID %in% as.numeric(flowline$wbareacomi))
  return(network_wtbdarea)
}

off_network <- function(waterbody, flowline) {
  off_network_wtbdarea <- dplyr::filter(waterbody,
                                        !COMID %in% as.numeric(flowline$wbareacomi))
  return(off_network_wtbdarea)
}

#' Get Waterbody Outlet
#' @param lake_id integer COMID (or character permanent identifier for hi res) of lake.
#' @param network data.frame of network features containing wbareacomi, and Hydroseq
#' @export
#' @return sf data.frame with single record of network COMID
#' associated with most-downstream reach in the NHD Waterbody
#' @examples
#' \donttest{
#'
#' source(system.file("extdata/sample_data.R", package = "hydrogeofetch"))
#'
#' fline <- sf::read_sf(sample_data, "NHDFlowline_Network")
#' wtbdy <- sf::read_sf(sample_data, "NHDWaterbody")
#'
#' lake_COMID <- wtbdy$COMID[wtbdy$GNIS_NAME=='Lake Mendota 254']
#'
#' get_wb_outlet(13293262, fline)
#'
#' }
get_wb_outlet <- function(lake_id, network) {

  if(any(grepl("WBArea_Permanent_Identifier", names(network), ignore.case = TRUE))) {
    network <- check_names(network, "get_wb_outlet_hires",
                           align = FALSE, tolower = FALSE)
    if (lake_id %in% network$WBArea_Permanent_Identifier){
    outlet <- network |>
      filter(.data$WBArea_Permanent_Identifier == lake_id) |>
      group_by(.data$WBArea_Permanent_Identifier) |>
      filter(.data$Hydroseq == min(.data$Hydroseq))
    return(outlet)
    } else {
      stop("Lake Permanent Identifier no associated with NHDPlus HR Flowlines")
    }
  } else {
    network <- check_names(network, "get_wb_outlet_mres", tolower = TRUE)

    if (lake_id %in% network$wbareacomi){
      outlet <- network |>
        filter(.data$wbareacomi == lake_id) |>
        group_by(.data$wbareacomi) |>
        filter(.data$hydroseq == min(.data$hydroseq))
      return(outlet)
    } else {
      stop("Lake COMID is not associated with NHDPlus flowlines and no outlet")
    }
  }

}
