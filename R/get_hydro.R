#' @title Find WBD HUC unit subsets
#' @description Subsets WBD features by location (POINT),
#' area (POLYGON), or set of HUC IDs.
#'
#' @inherit query_usgs_oafeat details return params
#' @param id WBD HUC ID(s)
#' @param type character. Type of feature to return. If `NULL` (default) and
#' `id` is provided, the HUC level is autodetected from the character length
#' of the IDs (e.g. 2-character IDs → `huc02`, 12-character → `huc12`).
#' A version suffix alone (e.g. `"_2020"`, `"_nhdplusv2"`) can also be
#' provided to combine autodetected HUC level with a specific version.
#' If `NULL` and no `id` is provided, defaults to `huc12`.
#' Bare types (`huc02`-`huc12`) default to the 2025 WBD version. Versioned
#' types are also available with suffixes `_2025`, `_2020`, `_nhdplusv2`, and
#' `_nhdplushr` (e.g. `huc12_nhdplusv2`).
#'
#' See https://api.water.usgs.gov/fabric/pygeoapi for the web service.
#'
#' @export
#'
get_huc <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5, type = NULL) {

  huc_levels <- c('huc02', 'huc04', 'huc06', 'huc08', 'huc10', 'huc12')
  versions <- c('_2025', '_2020', '_nhdplusv2', '_nhdplushr')

  allow_types <- c(huc_levels,
                   as.vector(outer(huc_levels, versions, paste0)))

  if(!is.null(id)) {
    nchar_id <- unique(nchar(id))
    if(length(nchar_id) != 1) {
      stop("All IDs must be the same length to autodetect type. ",
           "Provide type explicitly for mixed-length IDs.")
    }
    valid_lengths <- c(2, 4, 6, 8, 10, 12)
    if(!nchar_id %in% valid_lengths) {
      stop("ID length ", nchar_id, " does not correspond to a valid HUC type. ",
           "Expected lengths: ", paste(valid_lengths, collapse = ", "))
    }
    huc_level <- paste0("huc", sprintf("%02d", nchar_id))

    if(is.null(type)) {
      type <- huc_level
      message("Inferred type '", type, "' from ID length")
    } else if(type %in% versions) {
      type <- paste0(huc_level, type)
      message("Inferred type '", type, "' from ID length and version suffix")
    }
  } else if(is.null(type)) {
    type <- "huc12"
  }

  if(!type %in% allow_types) {
    stop("type must be one of ", paste(allow_types, collapse = " "))
  }

  if(type %in% huc_levels) {
    type <- paste0(type, "_2025")
    message("defaulting to 2025 version of WBD")
  }

  query_usgs_oafeat(AOI = AOI, ids = id, type = type,
                    t_srs = t_srs, buffer = buffer)

}

#' @title Find NHDPlusV2 Water Bodies
#' @description Subsets NHDPlusV2 waterbody features by location (POINT),
#' area (POLYGON), or set of IDs. See \link{download_nhdplusv2} for source data documentation.
#' @inherit query_usgs_oafeat details return
#' @inheritParams query_usgs_oafeat
#' @param id NHD Waterbody COMID(s)
#' @export

get_waterbodies <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5){
  query_usgs_oafeat(AOI = AOI, ids = id,
                       type = "waterbodies",
                       t_srs = t_srs,
                       buffer = buffer)
}

#' @title Find NHDPlusV2 Areas
#' @description Subsets NHDPlusV2 Area features by location (POINT),
#' area (POLYGON), or set of IDs. See \link{download_nhdplusv2} for source data documentation.
#' @inherit query_usgs_oafeat details return
#' @inheritParams query_usgs_oafeat
#' @param id NHD Area COMID(s)
#' @export

get_nhdarea <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5){
  query_usgs_oafeat(AOI = AOI, ids = id, type = "nhdarea",
                       t_srs = t_srs, buffer = buffer)
}


#' @title Find gagesII Features
#' @description Subsets the gagesII dataset by location (POINT),
#' area (POLYGON), or set of IDs. See <doi:10.5066/P96CPHOT> for documentation of source data.
#' @inherit query_usgs_oafeat details return
#' @inheritParams query_usgs_oafeat
#' @param id character NWIS Gage ID(s)
#' @param basin logical should the gagesII basin also be returned? If True,
#' return value will be a list with "site" and "basin" elements.
#' @export

get_gagesII <- function(AOI = NULL, id = NULL, t_srs = NULL, buffer = .5,
                        basin = FALSE){

  out <- query_usgs_oafeat(AOI = AOI, ids = id, type = "gagesII",
                              t_srs = t_srs, buffer = buffer)

  if(basin) {
    return(list(site = out,
                basin = query_usgs_oafeat(
                  ids = out[["staid"]], type = "gagesII-basin",
                  t_srs = t_srs, buffer = buffer)))
  }

  out
}

#' @title Discover USGS NWIS Stream Gages
#' @description Returns a POINT feature class of active, stream network,
#' NWIS gages for an Area of Interest. If a POINT feature is used as an AOI,
#' then the returned sites within the requested buffer, are sorted by distance (in meters) from that POINT.
#' @inherit query_usgs_oafeat details return
#' @inheritParams query_usgs_oafeat
#' @param buffer numeric. The amount (in meters) to buffer a POINT AOI by
#' for an extended search. Default = 20,000. Returned results are arranged
#' by distance from POINT AOI
#' @importFrom xml2 xml_root xml_children xml_attr read_xml
#' @importFrom sf st_geometry_type st_transform st_buffer st_as_sf st_bbox st_nearest_feature st_distance
#' @importFrom dplyr filter mutate
#' @export

get_nwis <- function(AOI = NULL, t_srs = NULL, buffer = 20000){

  # If t_src is not provided set to AOI CRS
  if(is.null(t_srs)){ t_srs  <- sf::st_crs(AOI)}

  AOI_type = st_geometry_type(AOI)

  if(AOI_type == "POINT"){
    pt  <-  AOI
    AOI <-  sf::st_buffer(sf::st_transform(AOI, 5070), buffer) |>
      sf::st_bbox() |>
      sf::st_as_sfc()
  }

  bb <-  sf::st_transform(AOI, 4326)
  bb <-  round(sf::st_bbox(bb), 7)

  dX = bb$xmax - bb$xmin
  dY = bb$ymax - bb$ymin

  if(dX > 4.599 | dY > 7.599){
    stop(paste0("Bounding Box too large [", round(dX,1),"x", round(dY,1), " degrees].
                Your requested width must be less than or equal to
                7.6 degrees at latitude 44.4
                with requested height of 4.6 degrees."))
  }

  u <- paste0("https://waterservices.usgs.gov/nwis/site/?format=mapper&bBox=",
                bb$xmin, ",", bb$ymin, ",",
                bb$xmax, ",", bb$ymax,
                "&siteType=ST&siteStatus=active")

  # go through the package request helper so transient failures are retried
  # rather than being reported as an empty result
  get_xml <- function(u) {
    read_xml(httr2::resp_body_string(httr2::req_perform(build_hgf_req(u))))
  }

  resp <- tryCatch(get_xml(u), error = function(e) NULL)

  if(is.null(resp)){
    if(AOI_type == "POINT"){
      warning("No gages with defined buffer of this location")
      return(NULL)
    } else {
      warning("No gages found in this AOI.")
      return(NULL)
    }
  } else {
    doc        <- xml2::xml_root(resp)
    sc         <- xml2::xml_children(doc)
    sites      <- xml2::xml_children(sc)

    sites_sf <- data.frame(agency_cd  = xml2::xml_attr(sites, "agc"),
                           site_no    = xml2::xml_attr(sites, "sno"),
                           station_nm = xml2::xml_attr(sites, "sna"),
                           site_type  = xml2::xml_attr(sites, "cat"),
                           lat = as.numeric(xml2::xml_attr(sites, "lat")),
                           lon = as.numeric(xml2::xml_attr(sites, "lng"))) |>
      st_as_sf(coords = c("lon", "lat"), crs = 4326)

    if(AOI_type == "POINT"){
      sites_sf$distance_m <- st_distance(st_transform(sites_sf, 5070),
                                         st_transform(pt, 5070))
      sites_sf <- arrange(sites_sf, .data$distance_m)
    }

    return(st_transform(sites_sf, t_srs))
  }
}

#' Get NHDPlusHR Data
#' @description
#' Calls the NHDPlus_HR web service and returns sf data.frames for the selected
#' layers. See https://hydro.nationalmap.gov/arcgis/rest/services/NHDPlus_HR/MapServer
#' for source data documentation.
#'
#' @inherit query_usgs_arcrest details return params
#'
#' @param type character. Type of feature to return e.g.
#' c("networknhdflowline", nonnetworknhdflowline", nhdwaterbody", "nhdpluscatchment").
#' If NULL (default) a data.frame of available types is returned
#'
#' @param ids character vector of nhdplusid ids
#'
#' @param reachcode character vector of reachcodes
#' NOTE: performance of this query is currently very poor,
#' spatial queries are the primary use of this function.
#'
#' @export
#' @examples
#' \donttest{
#' AOI <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
#'                                    xmax = -89.24681, ymax = 43.17192),
#'                                  crs = "+proj=longlat +datum=WGS84 +no_defs"))
#'
#' # get flowlines and hydrolocations
#' flowlines <- get_nhdphr(AOI = AOI, type = "networknhdflowline")
#' point <- get_nhdphr(AOI = AOI, type = "nhdpoint")
#' waterbody <- get_nhdphr(AOI = AOI, type = "nhdwaterbody")
#'
#' if(!is.null(waterbody) & !is.null(flowlines) & !is.null(point)) {
#' plot(sf::st_geometry(waterbody), col = "lightblue", border = "lightgrey")
#' plot(sf::st_geometry(flowlines), col = "blue", add = TRUE)
#' plot(sf::st_geometry(point), col = "grey", pch = "+", add = TRUE) }
#'
#' # given universalreferenceid (reachcodes), can query for them but only
#' # for hydrolocations. This is useful for looking up mainstem ids.
#'
#' get_nhdphr(reachcode = "13020101021927", type = "networknhdflowline")
#'}
get_nhdphr <- function(AOI = NULL, ids = NULL, type = NULL,
                       reachcode = NULL,
                       t_srs = NULL, buffer = 0.5,
                       page_size = 2000) {

  if(!is.null(reachcode) && !isTRUE(grepl("nhdplusgage|nhdpoint|networknhdflowline|nonnetworknhdflowline|flowdirection|nhdwaterbody",
                                          type))) {
    stop("reachcode not defined for ", type)
  }

  where <- NULL
  if(!is.null(reachcode)) {
    where <- paste(paste0("reachcode IN ('",
                          paste(reachcode, collapse = "', '"), "')"))
    if(!is.null(ids)) stop("can not specify both reachcode and other ids")
  }

  query_usgs_arcrest(AOI, ids, type, "NHDPlus_HR", where, t_srs, buffer, page_size)

}
