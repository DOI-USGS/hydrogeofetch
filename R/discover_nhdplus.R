#' @title Discover NHDPlus ID
#' @description Multipurpose function to find a COMID of interest.
#'
#' Note that NHDPlusV2 uses "featureid" for catchment polygons and "comid" for
#' flowline linestrings. These two identifiers are the same where a
#' flowline/catchment pair exists. In some cases, a catchment will not have a
#' flowline and in others, a flowline will not have a catchment.
#'
#' If a point is provided, and raindrop is false, the comid/featureid integer
#' of the catchment the point is in is returned. This options uses a web
#' service here:
#' https://api.water.usgs.gov/fabric/pygeoapi/collections/catchmentsp
#'
#' If a point is provided, and raindrop is true, the response is the result of
#' a call to \link{get_raindrop_trace}.
#'
#' If no point is provided, the raindrop argument is ignored and the result
#' is a comid integer derived from a call to \link{get_nldi_feature}.
#'
#' @param point sfc POINT including crs as created by:
#' \code{sf::st_sfc(sf::st_point(.. ,..), crs)}
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' \link[dataRetrieval]{get_nldi_sources} and the `featureSource` is a known identifier
#' from the specified `featureSource`.
#' @param raindrop logical if \code{TRUE} will call a raindrop trace web service and
#' return will be the same as \link{get_raindrop_trace} with direction "none".
#' @return integer COMID or list containing COMID and raindrop trace.
#' @export
#' @examples
#' \donttest{
#' point <- sf::st_sfc(sf::st_point(c(-76.874, 39.482)), crs = 4326)
#' discover_nhdplus_id(point)
#'
#' discover_nhdplus_id(point, raindrop = TRUE)
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
#' discover_nhdplus_id(nldi_feature = nldi_nwis)
#' }
#'

discover_nhdplus_id <- function(point = NULL, nldi_feature = NULL, raindrop = FALSE) {
  if (!is.null(point)) {

    point <- check_point(point)

    if(raindrop) {
      out <- get_raindrop_trace(point, "none")

      return(out)
    }
    # check_point() transformed to EPSG:4326, and a transform leaves digits
    # that differ between PROJ builds; round so the same point always asks
    # the same question. 1e-7 degrees is about a centimeter.
    coords = round(sf::st_coordinates(point), 7)

    comid <- tryCatch({

      URL <- paste0(get_water_url(),
                    "collections/catchmentsp/items",
                    "?bbox=", coords[1], ",", coords[2], ",", coords[1], ",", coords[2],
                    "&properties=featureid&skipGeometry=true")

      d <- hgf_json(utils::URLencode(URL))
      if(is.null(d)) return(NULL)

      featureid <- d$features$properties$featureid
      if(is.null(featureid) || length(featureid) == 0) return(NULL)

      as.integer(featureid)

    }, error = function(e) NULL)

    return(comid)
  } else if (!is.null(nldi_feature)) {

    nldi <- get_nldi_feature(nldi_feature)
    if(is.null(nldi)) return(NULL)

    return(as.integer(nldi$comid))

  } else {
    stop("Must provide point or nldi_feature input.")
  }
}


