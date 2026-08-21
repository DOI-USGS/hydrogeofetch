get_arcrest_service_info <- memoise::memoise(function(service = "3DHP_all") {

  stopifnot(service %in% c("3DHP_all", "NHDPlus_HR"))

  if(service == "3DHP_all") {
    url_base <- paste0(get("arcrest_3dhp_root", envir = hydrogeofetch_env),
                       "usgs_3dhp_all",
                       "/FeatureServer/")
  } else {
    url_base <- paste0(get("arcrest_root", envir = hydrogeofetch_env),
                       service,
                       "/MapServer/")
  }

  all_layers <- hgf_json(paste0(url_base, "?f=json"), simplifyVector = FALSE)

  id_name <- "id3dhp"
  if(service == "NHDPlus_HR") id_name <- "nhdplusid"

  list(url_base = url_base, all_layers = all_layers, id = id_name)

})

# this function is intended to be a very general internal utility. It's initial
# implementation is quite specific but this should become more general over time.
#' @title Query USGS Hydro ESRI Rest Server
#' @description Query the USGS Hydro ESRI Rest Server for spatial data by location,
#' area, or ID.
#' @details The returned object(s) will have the same
#' Spatial Reference System (SRS) as the input AOI. If a individual or set of
#' IDs are used to query, then the default CRS of EPSG:4269 is
#' preserved. In all cases, a user-defined SRS can be passed to \code{t_srs}
#' which will override all previous SRS (either input or default).
#' All buffer and distance operations are handled internally using in
#' EPSG:5070 Albers Equal Area projection
#' @param AOI sf (MULTI)POINT or (MULTI)POLYGON. An 'area of interest' can
#' be provided as either a location (sf POINT) or area (sf POLYGON)
#' in any Spatial Reference System.
#' @param ids character. A set of identifier(s) from the data
#' type requested, for 3dhp, this is id3dhp.
#' @param type character. Type of feature to return
#' If NULL (default) a data.frame of available types is returned
#' @param service character chosen from "3DHP_all", "NHDPlus_HR"
#' @param where character An where clause to pass to the server.
#' @param t_srs  character (PROJ string or EPSG code) or numeric (EPSG code).
#' A user specified - target -Spatial Reference System (SRS/CRS) for returned objects.
#' Will default to the CRS of the input AOI if provided, and to 4326 for ID requests.
#' @param buffer numeric. The amount (in meters) to buffer a POINT AOI by for an
#' extended search. Default = 0.5
#' @param page_size numeric default number of features to request at a time. Reducing
#' may help if 500 errors are experienced.
#' @return a simple features (sf) object or valid types if no type supplied
#' @keywords internal
#' @importFrom sf st_crs st_geometry_type st_buffer st_transform st_zm read_sf st_bbox st_as_sfc
#' @importFrom dplyr filter
#' @importFrom methods as
query_usgs_arcrest <- function(AOI = NULL,  ids = NULL,
                               type = NULL, service = NULL,
                               where = NULL,
                               t_srs = NULL,
                               buffer = 0.5,
                               page_size = 2000) {

  si <- get_arcrest_service_info(service)

  source <- data.frame(user_call = sapply(si$all_layers$layers, \(x) tolower(x$name)),
                       layer = sapply(si$all_layers$layers, \(x) x$id))

  group_layers <- si$all_layers$layers[sapply(si$all_layers$layers,
                                              \(x) grepl("Group Layer", x$type))]

  if(is.null(type)) {
    message("`type` input must be one of: \n\t\"",
            paste(source$user_call, collapse = "\"\n\t\""), "\"")
    return(source)
  }

  exact_match <- type %in% source$user_call
  # Allow a prefix to act as a virtual group, e.g. "hydrolocation" expands to
  # all user_call entries of the form "hydrolocation - ...".
  prefix_match <- !exact_match & startsWith(source$user_call, paste0(type, " - "))

  if(!exact_match && !any(prefix_match)) {
    warning("\"", type, "\" not in `type` input. Must be one of: \n\t\"",
            paste(source$user_call, collapse = "\"\n\t\""), "\"")
    return(NULL)
  }

  if(any(prefix_match)) {
    need_layers <- as.integer(source$layer[prefix_match])
  } else if(length(group_layers) > 0 &&
            grepl(paste(sapply(group_layers, \(x) x$name),
                        collapse = "|"),
                  type, ignore.case = TRUE)) {
    layer_id <- filter(source, .data$user_call == !!type)$layer

    group_layer <- group_layers[[sapply(group_layers, \(x) x$id == layer_id)]]

    need_layers <- as.integer(group_layer$subLayerIds)
  } else {
    need_layers <- as.integer(filter(source, .data$user_call == !!type)$layer)
  }

  all_out <- rep(list(list()), length(need_layers))

  for(l in seq_len(length(all_out))) {

    layer <- need_layers[l]

    type <- filter(source, .data$layer == !!layer)$user_call

    AOI <- check_query_params(AOI, ids, type, where, source, t_srs, buffer)
    t_srs <- AOI$t_srs
    AOI <- AOI$AOI

    here <- filter(source, .data$user_call == !!type)

    URL <- paste0(si$url_base, here$layer, "/query")

    spat_filter <- spatial_filter_esri(AOI)

    if(!is.null(ids)) {

      if(!is.null(where)) stop("can't specify ids and where")

      if(si$id == "nhdplusid") {
        where <- paste0(si$id, " IN (",
                        paste(ids, collapse = ", "), ")")
      } else {
        where <- paste0(si$id, " IN ('",
                        paste(ids, collapse = "', '"), "')")
      }

    }

    post_body <- list(where = where,
                      f = "json",
                      returnIdsOnly = "true")

    if(length(spat_filter[[1]]) > 0) {
      post_body <- c(list(geometry = spat_filter[[1]]),
                     post_body)
    }

    all_ids <- NULL
    tryCatch({
      resp <- hgf_json(URL, body = post_body, encode = "form",
                        simplifyVector = FALSE)
      all_ids <- unlist(resp$objectIds)

    }, error = function(e) {
      warning("Something went wrong trying to access a service.")
    })

    length_ids <- length(all_ids)

    if(is.null(all_ids) | length(all_ids) == 0) {
      warning(paste("No", here$user_call, "features found in area of interest."), call. = FALSE)
      out <- NULL
    } else {

      chunk_size <- page_size
      all_ids <- split(all_ids, ceiling(seq_along(all_ids)/chunk_size))

      out <- rep(list(list()), length(all_ids))

      for(i in seq_along(all_ids)) {

        if(length_ids > chunk_size) {
          top <- i * chunk_size
          if(top > length_ids) top <- length_ids
          message("Getting features ", (i - 1) * chunk_size, " to ", top,  " of ", length_ids)
        }

        post_body <- list(objectIds = paste(all_ids[[i]], collapse = ","),
                          outFields = "*",
                          f = "geojson")

        out[i] <- list(hgf_sf(URL, body = post_body, encode = "form"))
      }

      # a page that failed comes back NULL from hgf_sf; drop those so bind_rows
      # doesn't choke on them and warn that the result is incomplete.
      ok <- vapply(out, inherits, logical(1), what = "data.frame")

      if(any(!ok)) {
        warning(sum(!ok), " of ", length(ok),
                " feature requests failed, returned features are incomplete.",
                call. = FALSE)
        out <- out[ok]
      }

      if(length(out) > 0) {
        out <- bind_rows(unify_types(out))

        if("id3dhp" %in% names(out)) {
          out <- hydroloom::check_valid(out[!duplicated(out[["id3dhp"]]), ],
                             out_prj = t_srs)
        } else {
          out <- hydroloom::check_valid(out, out_prj = t_srs)
        }

      } else {

        out <- NULL

      }

      if(any(is.null(out), nrow(out) == 0)) {

        out = NULL

      } else if(!is.null(AOI)){

        out = sf::st_filter(sf::st_transform(out, t_srs),
                            sf::st_transform(AOI, t_srs))

        if(nrow(out) == 0){
          warning(paste(here$user_call, "features found but were outside area of interest polygon."))
          out = NULL

        }
      }

      if(!is.null(out)) {
        out <- select(out, -any_of("OBJECTID"))
      } else {
        warning(paste("No", here$user_call, "features found"), call. = FALSE)
        out <- NULL
      }
    }
    all_out[[l]] <- out
  }
  tryCatch(sf::st_sf(data.table::rbindlist(all_out)), error = function(e) NULL)
}


#' @title Construct a bounding box geometry filter for an ArcGIS REST service
#' @description From an 'area of interest' object (sf POINT or POLYGON),
#' generate an envelope geometry filter to pass to an ArcGIS REST query.
#' @inheritParams get_nhdplus
#' @return a list holding the envelope as JSON with an EPSG:4326 spatial
#' reference, or a list holding an empty list when AOI is NULL
#' @keywords internal
#' @noRd
#' @importFrom sf st_geometry_type st_buffer st_transform st_bbox

spatial_filter_esri  <- function(AOI) {

  if(is.null(AOI)) return(list(list()))


  bb_list <- st_transform(AOI, 4326) |>
    st_bbox() |>
    list()

  # {
  #   "xmin": -109.55,
  #   "ymin": 25.76,
  #   "xmax": -86.39,
  #   "ymax": 49.94,
  #   "spatialReference": {
  #     "wkid": 4326
  #   }
  # }

  lapply(bb_list, function(bb) {
    jsonlite::toJSON(
      c(bb, list(spatialReference = list(wkid = 4326))),
      auto_unbox = TRUE)
  })

}
