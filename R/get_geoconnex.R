#' discover geoconnex reference feature layers
#' @description
#' Queries the geoconnex.us reference feature server for available layers and
#' attributes.
#'
#' @return data.frame containing layers available and fields that are available to query.
#' @export
#' @examples
#' \donttest{
#'   discover_geoconnex_reference()
#' }
#'
discover_geoconnex_reference <- function() {

  discover_oafeat(get("gocnx_ref_base_url", envir = hydrogeofetch_env))

}

#' get geoconnex reference feature layers
#' @description
#' Queries the geoconnex reference feature server for features of interest.
#'
#' @param AOI  bbox, sf polygon or point, or a URL that will return an sf object when passed to
#' \link[sf]{read_sf}
#' @param type character the feature type to query. Must match one of the `id`
#' entries returned by \link{discover_geoconnex_reference} (the `type` column of
#' that output is not a valid value here).
#' @inheritParams query_usgs_oafeat
#' @param status boolean print status or not
#' @return sf data.frame containing requested reference features
#' @export
#' @examples
#' \donttest{
#' dplyr::distinct(discover_geoconnex_reference()[c("id", "title")])
#'
#' AOI <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
#'                                    xmax = -89.24681, ymax = 43.17192),
#'                                  crs = "+proj=longlat +datum=WGS84 +no_defs"))
#'
#' get_geoconnex_reference(AOI, type = "hu04")
#'
#' get_geoconnex_reference("https://geoconnex.us/ref/mainstems/315626", type = "hu04", )
#'
#' AOI <- sf::st_sfc(sf::st_point(c(-89.56684, 42.99816)),
#'                   crs = "+proj=longlat +datum=WGS84 +no_defs")
#'
#' get_geoconnex_reference(AOI, type = "hu04", buffer = 100000)
#'
#' }
get_geoconnex_reference <- function(AOI,
                                    type = NULL,
                                    t_srs = NULL,
                                    buffer = 0.5,
                                    status = TRUE) {

  base <- get("gocnx_ref_base_url", envir = hydrogeofetch_env)

  get_oafeat(base, AOI, type = type, t_srs = t_srs, buffer = buffer, status = status)

}

