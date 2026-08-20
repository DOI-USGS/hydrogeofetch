#' @title Query USGS Water OGC API Features
#' @description Query the USGS Water OGC API for spatial data by location,
#' area, or ID.
#' @details The returned object(s) will have the same
#' Spatial Reference System (SRS) as the input AOI. If a individual or set of
#' IDs are used to query, then the default server CRS of EPSG:4326 is
#' preserved. In all cases, a user-defined SRS can be passed to \code{t_srs}
#' which will override all previous SRS (either input or default).
#' All buffer and distance operations are handled internally using an
#' EPSG:5070 Albers Equal Area projection
#'
#' @param AOI sf (MULTI)POINT or (MULTI)POLYGON. An 'area of interest' can
#' be provided as either a location (sf POINT) or area (sf POLYGON)
#' in any Spatial Reference System.
#' @param ids character or numeric. A set of identifier(s) from the data
#' type requested, for example if NHDPlusV2, then a set of COMID(s).
#' @param type character. Type of feature to return
#' If NULL (default) a data.frame of available resources is returned
#' @param filter character. An filter to pass to the query
#' @param t_srs  character (PROJ string or EPSG code) or numeric (EPSG code).
#' A user specified - target -Spatial Reference System (SRS/CRS) for returned objects.
#' Will default to the CRS of the input AOI if provided, and to 4326 for ID requests.
#' @param buffer numeric. The amount (in meters) to buffer a POINT AOI by for an
#' extended search. Default = 0.5
#' @param properties character vector. Column names to return. When NULL (default),
#'   all columns are returned.
#' @param skip_geometry logical. If TRUE, omit geometry from the response and
#'   return a plain data.frame. Default FALSE.
#' @return a simple features (sf) object, or a data.frame when
#'   \code{skip_geometry = TRUE}
#' @keywords internal
query_usgs_oafeat <- function(AOI = NULL,  ids = NULL,
                              type = NULL, filter = NULL,
                              t_srs = NULL,
                              buffer = 0.5,
                              properties = NULL,
                              skip_geometry = FALSE) {

  base <- get_water_url()

  source <- data.frame(server = 'usgs_oafeat',
                       user_call  = c('huc02_2020', 'huc04_2020', 'huc06_2020',
                                      'huc08_2020', 'huc10_2020', 'huc12_2020',
                                      'huc02_2025', 'huc04_2025', 'huc06_2025',
                                      'huc08_2025', 'huc10_2025', 'huc12_2025',
                                      'huc02_nhdplusv2', 'huc04_nhdplusv2', "huc06_nhdplusv2",
                                      'huc08_nhdplusv2', 'huc10_nhdplusv2', "huc12_nhdplusv2",
                                      'huc02_nhdplushr', 'huc04_nhdplushr', "huc06_nhdplushr",
                                      'huc08_nhdplushr', 'huc10_nhdplushr', "huc12_nhdplushr",
                                      'nhd','catchment', 'nhdarea',
                                      'nonnetwork',
                                      'waterbodies',
                                      'gagesII', "gagesII-basin"),
                       layer_name  = c("wbd02_2020", "wbd04_2020", "wbd06_2020",
                                       "wbd08_2020", "wbd10_2020", "wbd12_2020",
                                       "wbd02_2025", "wbd04_2025", "wbd06_2025",
                                       "wbd08_2025", "wbd10_2025", "wbd12_2025",
                                       'nhdplusv2-huc02', "nhdplusv2-huc04", 'nhdplusv2-huc06',
                                       'nhdplusv2-huc08', 'nhdplusv2-huc10', 'nhdplusv2-huc12',
                                       'nhdplushr-huc02', "nhdplushr-huc04", 'nhdplushr-huc06',
                                       'nhdplushr-huc08', 'nhdplushr-huc10', 'nhdplushr-huc12',
                                       "nhdflowline_network", "catchmentsp", 'nhdarea',
                                       "nhdflowline_nonnetwork",
                                       "nhdwaterbody",
                                       "gagesii", "gagesii-basins"),
                       geom_name = c("SHAPE", "SHAPE", "SHAPE",
                                     "SHAPE", "SHAPE", "SHAPE",
                                     "geometry", "geometry", "geometry",
                                     "geometry", "geometry", "geometry",
                                     "geometry", "geometry", "geometry",
                                     "geometry", "geometry", "geometry",
                                     "geometry", "geometry", "geometry",
                                     "geometry", "geometry", "geometry",
                                     "the_geom", "the_geom", "the_geom",
                                     "the_geom",
                                     "the_geom",
                                     "the_geom", "the_geom"),
                       ids        = c("huc2", "huc4", "huc6",
                                      "huc8", "huc10", "huc12",
                                      "huc2", "huc4", "huc6",
                                      "huc8", "huc10", "huc12",
                                      "02", "04", "06",
                                      "08", "10", "huc_12",
                                      "02", "04", "06",
                                      "08", "10", "huc_12",
                                      "comid", "featureid", "comid",
                                      "comid",
                                      "comid",
                                      "staid", "gage_id"),
                       page       = c(FALSE, FALSE, FALSE,
                                      FALSE, FALSE, FALSE,
                                      FALSE, FALSE, FALSE,
                                      FALSE, FALSE, FALSE,
                                      FALSE, FALSE, FALSE,
                                      FALSE, FALSE, FALSE,
                                      FALSE, FALSE, FALSE,
                                      FALSE, FALSE, FALSE,
                                      TRUE, TRUE, TRUE,
                                      TRUE,
                                      TRUE,
                                      FALSE, TRUE))

  if(is.null(type)) {
    return(source)
  }

  AOI <- check_query_params(AOI, ids, type, NULL, source, t_srs, buffer)
  t_srs <- AOI$t_srs
  AOI <- AOI$AOI

  here <- filter(source, .data$user_call == !!type)

  type <- here$layer_name

  if(!is.null(ids)) {
    ids <- stats::setNames(list(ids), here$ids)
  }

  out <- get_oafeat(base, AOI = AOI, type = type, ids = ids, filter = filter,
                     t_srs = t_srs, buffer = buffer, status = FALSE,
                     properties = properties, skip_geometry = skip_geometry)

  if(skip_geometry) {
    if(any(is.null(out), nrow(out) == 0)) {
      warning(paste("No", here$user_call, "features found"), call. = FALSE)
      return(NULL)
    }
    return(out)
  }

  out <- hydroloom::check_valid(out)

  if(any(is.null(out), nrow(out) == 0)) {

    out = NULL

  } else if(!is.null(AOI)){

    out = st_filter(st_transform(out, t_srs),
                    st_transform(AOI, t_srs))
    if(nrow(out) == 0){
      out = NULL
    }
  }

  if(!is.null(out)) {
    return(out)
  } else {
    warning(paste("No", here$user_call, "features found"), call. = FALSE)
    NULL
  }

}

#' discover OGC API feature layers
#' @description
#' Queries an OGC API feature server for available layers and
#' attributes.
#'
#' @return data.frame containing layers available and fields that are available to query.
#' @param landing_url url of landing page of OGC API
#' @noRd
#'
discover_oafeat <- function(landing_url) {

  landing <- mem_get_json(landing_url)
  if(is.null(landing)) {
    warning("OGC API landing page unavailable at ", landing_url,
            call. = FALSE)
    return(NULL)
  }

  collections <- landing$links |>
    filter_list_kvp("rel", "data", n = 1) |>
    extract("href") |>
    mem_get_json()
  if(is.null(collections)) {
    warning("OGC API collections endpoint unavailable from ", landing_url,
            call. = FALSE)
    return(NULL)
  }

  collections_meta <- dplyr::bind_rows(
    lapply(collections$collections,
           \(x) c(x[c("id", "title", "description", "itemType")],
                  list(url = filter_list_kvp(x$links,
                                             "rel", "self", n = 1)$href)))) |>
    dplyr::filter(.data$itemType == "feature") |>
    dplyr::select(-"itemType")


  q_ables <- dplyr::bind_rows(lapply(collections$collections, \(x) {
    if(x$itemType != "feature") return(NULL)

    q <- filter_list_kvp(x$links, "rel", "http://www.opengis.net/def/rel/ogc/1.0/queryables",
                         type = "application/schema+json", n = 1)$href |>
      mem_get_json() |>
      (\(y) if(is.null(y)) y else list(id = x$id, qs = y$properties))()

    q$qs <- q$qs[vapply(q$qs, \(x) all(c("title", "type") %in% names(x)), TRUE)]

    data.frame(id = rep(q$id, length(q$qs)),
               attribute = vapply(q$qs, \(x) x$title, ""),
               type = vapply(q$qs, \(x) x$type, ""), row.names = NULL)
  }))

  dplyr::left_join(collections_meta, q_ables, by = "id")

}

#' get geoconnex reference feature layers
#' @description
#' Queries the geoconnex reference feature server for features of interest.
#'
#' @param AOI  bbox, sf polygon or point, or a URL that will return an sf object when passed to
#' \link[sf]{read_sf}
#' @param type character the feature type desired
#' @param filter character CQL filter string
#' @inheritParams query_usgs_oafeat
#' @param status boolean print status or not
#' @return sf data.frame containing requested features
#' @noRd
get_oafeat <- function(base,
                       AOI,
                       ids = NULL,
                       type = NULL,
                       filter = NULL,
                       t_srs = NULL,
                       buffer = 0.5,
                       status = TRUE,
                       properties = NULL,
                       skip_geometry = FALSE) {

  avail <- discover_oafeat(base)

  if(is.null(avail)) return(NULL)

  if(is.null(type)) {
    warning("type is required, returning choices.")
    return(avail)
  }

  test <- grepl(paste0("^", type), unique(avail$id))
  if(sum(test) == 1) type <- unique(avail$id)[test]

  if(!type %in% avail$id) stop("Type must be in available ids: ", paste(unique(avail$id), collapse = ", "), ".")

  avail <- filter(avail, .data$id == !!type)

  base_call <- paste0(base, "collections/", type, "/items")
  post_body <- list()

  limit <- 500

  if(!is.null(AOI)) {
    if(is.character(AOI)) {

      if(grepl("^https?://", AOI)) {
        AOI <- hgf_sf(AOI)
      } else {
        AOI <- try(sf::read_sf(AOI), silent = TRUE)
      }

      if(!inherits(AOI, "sf")) {
        stop("AOI did not return an sf object when read")
      }

      AOI <- st_transform(AOI, 4326)

    }

    if(!inherits(AOI, "bbox") &&
       grepl("point", sf::st_geometry_type(AOI), ignore.case = TRUE)) {
      AOI <- st_transform(AOI, 5070) |>
        sf::st_buffer(units::as_units(buffer, "m")) |>
        st_transform(4326) |>
        st_bbox()
    } else if(!inherits(AOI, "bbox")) {
      AOI <- st_bbox(st_transform(AOI, 4326))
    }

    # pull features with paging if necessary

    bbox <- paste(round_bbox_out(AOI), collapse = ",")

    base_call <- paste0(base_call, "?bbox=", bbox)

  } else if(!is.null(ids)) {

    if(!is.null(filter)) stop("filter not supported with ids")

    id_attribute <- names(ids)

    ids <- ids[[1]]

    if(length(ids) == 1) {
      base_call <- paste0(base_call, paste0("?", id_attribute, "=", ids))
    } else {
      post_body <- list(ids = ids, id_attribute = id_attribute)
    }

  }

  if(!is.null(filter)) {
    base_call <- paste0(add_sep(base_call), "filter=", filter)
  }

  if(skip_geometry)
    base_call <- paste0(add_sep(base_call), "skipGeometry=true")

  if(!is.null(properties))
    base_call <- paste0(add_sep(base_call), "properties=",
                        paste(properties, collapse = ","))

  out <- get_features_paging(base_call, post_body, limit = limit, status = status)

  if(skip_geometry) {
    if(inherits(out, "sf"))
      out <- sf::st_drop_geometry(out)
    return(out)
  }

  if(!is.null(t_srs)) out <- st_transform(out, t_srs)

  out
}

add_sep <- function(bc) {
  if(!grepl("\\?", bc)) {
    bc <- paste0(bc, "?")
  } else {
    bc <- paste0(bc, "&")
  }
  bc
}

get_features_paging <- function(base_call, ids_list = list(), limit = 1000, status = TRUE) {

  use_ids <- !identical(ids_list, list())

  if(use_ids) {
    ids <- split_equal_size(ids_list$ids, limit)
  }

  base_call <- add_sep(base_call)

  if(status & interactive()) message("Starting download of first set of features.")

  out <- list()
  id_batch <- 1
  offset <- 0

  keep_going <- TRUE

  while(keep_going) {

    if(use_ids) {
      post_body <- id_filter_cql(ids[[id_batch]], ids_list$id_attribute)
    } else {
      post_body <- ""
    }

    req <- paste0(base_call, "limit=", limit, "&offset=", offset)

    if(post_body != "") {
      page <- hgf_sf(req, body = post_body,
                      content_type = "application/query-cql-json")
    } else {
      page <- hgf_sf(req)
    }

    if(!inherits(page, "sf")) {
      warning("Something went wrong requesting data.")
      keep_going <- FALSE
      next
    }

    if(nrow(page) > 0) out <- c(out, list(page))

    if(nrow(page) < limit) {
      if(use_ids && id_batch < length(ids)) {
        id_batch <- id_batch + 1
        offset <- 0
        if(status) message("starting next download from batch ", id_batch,
          " of ", length(ids), ".")
      } else {
        keep_going <- FALSE
      }
    } else {
      offset <- offset + limit
      if(status) message("Starting next download from offset ", offset, ".")
    }
  }

  if(length(out) == 0) return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))

  sf::st_sf(dplyr::bind_rows(unify_types(out)))
}

id_filter_cql  <- function(ids, name = "comid"){

  operator <- "in"

  if(length(ids) == 1) operator <- "="

  jsonlite::toJSON(list(op = operator, args = list(list(property = name), c(ids))),
                   auto_unbox = TRUE)

}

unify_types <- function(out) {
  all_class <- bind_rows(lapply(out, function(x) {
    vapply(x, function(x) class(x)[1], character(1))
  }))

  set_type <- function(out, n, type) {
    lapply(out, function(x) {
      x[[n]] <- as(x[[n]], type)
      x
    })
  }

  for(n in names(all_class)) {
    if(length(unique(all_class[[n]])) > 1) {
      if("numeric" %in% all_class[[n]]) { # prefer numeric
        out <- set_type(out, n, "numeric")
      } else if("integer" %in% all_class[[n]]) { # then integer
        out <- set_type(out, n, "integer")
      } else if("character" %in% all_class[[n]] | "datetime" %in% all_class[[n]]) {
        out <- set_type(out, n, "character")
      }
    }
  }

  rows <- sapply(out, nrow)

  if(any(rows > 0))
    out <- out[rows > 0]

  out
}

#' Get HUC12 features filtered by HUC parent IDs
#'
#' Queries the nhdplusv2-huc12 OGC API collection for all HUC12 polygons
#' belonging to the specified HUC08 or HUC10 units.
#'
#' @param huc_ids character vector of 8-digit HUC08 or 10-digit HUC10 IDs.
#'   All IDs must be the same length (all 8-digit or all 10-digit).
#' @param t_srs target spatial reference system (optional)
#' @return sf data.frame of HUC12 features
#' @noRd
get_huc12_by_huc <- function(huc_ids, t_srs = NULL) {
  if(length(huc_ids) == 0)
    return(sf::st_sf(geometry = sf::st_sfc(crs = 4326)))

  base <- get_water_url()

  n <- unique(nchar(huc_ids))
  if(length(n) != 1 || !n %in% c(8, 10))
    stop("huc_ids must be all 8-digit (HUC08) or all 10-digit (HUC10)")

  id_name <- if(n == 8) "huc_8" else "huc_10"

  message("  Fetching HUC12s for ", length(huc_ids), " ",
    id_name, " IDs...")
  ids_list <- stats::setNames(list(huc_ids), id_name)
  get_oafeat(base, AOI = NULL, type = "nhdplusv2-huc12",
    ids = ids_list, t_srs = t_srs, status = FALSE)
}
