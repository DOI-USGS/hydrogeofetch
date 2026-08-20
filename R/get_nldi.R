#' @title Navigate NLDI
#' @description Navigate the Network Linked Data Index network.
#' @param nldi_feature list with names `featureSource` and `featureID` where
#' `featureSource` is derived from the "source" column of  the response of
#' \link[dataRetrieval]{get_nldi_sources} and the `featureID` is a known identifier
#' from the specified `featureSource`.
#' @param mode character chosen from ("UM", "UT", DM", "DD"). See examples.
#' @param data_source character chosen from "source" column of the response
#' of \link[dataRetrieval]{get_nldi_sources} or empty string for flowline geometry.
#' @param distance_km numeric distance in km to stop navigating.
#' @return sf data.frame with result
#' @export
#' @importFrom utils tail
#' @examples
#' \donttest{
#' library(sf)
#' library(dplyr)
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-05428500")
#'
#' navigate_nldi(nldi_feature = nldi_nwis,
#'               mode = "upstreamTributaries")$UT |>
#'   st_geometry() |>
#'   plot()
#'
#' navigate_nldi(nldi_feature = nldi_nwis,
#'               mode = "UM")$UM |>
#'   st_geometry() |>
#'   plot(col = "blue", add = TRUE)
#'
#'
#'
#' nwissite <- navigate_nldi(nldi_feature = nldi_nwis,
#'                           mode = "UT",
#'                           data_source = "nwissite")$UT_nwissite
#'
#' st_geometry(nwissite) |>
#'   plot(col = "green", add = TRUE)
#'
#' nwissite
#' }
#'
navigate_nldi <- function(nldi_feature, mode = "upstreamMain",
                          data_source = "flowlines", distance_km = 10) {

  nldi_feature <- check_nldi_feature(nldi_feature)

  nav_lookup <- list(upstreamMain = "UM",
                     upstreamTributaries = "UT",
                     downstreamMain = "DM",
                     downstreamDiversions = "DD")

  if (nchar(mode) > 2) {
    if (nchar(mode) < 30) {
      mode <- nav_lookup[[mode]]
    } else {
      mode <- tail(unlist(strsplit(mode, "/")), n = 1)
    }
  }

  # For backward compatibility
  if(data_source == "flowline" | data_source == "") {
    data_source <- "flowlines"
    warning("data source specified as flowline or '' is deprecated")
  }

  tryCatch(dataRetrieval::findNLDI(origin = nldi_feature,
                                   nav = mode,
                                   find = data_source,
                                   distance_km = distance_km,
                                   no_sf = FALSE),
           error = function(e) NULL)
}

#' @title Get NLDI Basin Boundary
#' @description Get a basin boundary for a given NLDI feature.
#' @details Only resolves to the nearest NHDPlus catchment divide. See:
#' https://waterdata.usgs.gov/blog/nldi-intro/ for more info on the nldi.
#' @inheritParams navigate_nldi
#' @param simplify logical should response geometry be simplified for
#' visualization and performance?
#' @param split logical should response resolve precisely to the location
#' of the `nldi_feature`? Setting `TRUE` calls an additional service and
#' will be slower and less robust.
#' @return sf data.frame with result basin boundary
#' @export
#' @importFrom sf read_sf
#' @examples
#' \donttest{
#' library(sf)
#' library(dplyr)
#'
#' nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-05428500")
#'
#' site <- get_nldi_feature(nldi_nwis)
#'
#' basin <- get_nldi_basin(nldi_feature = nldi_nwis)
#'
#' plot(st_geometry(basin))
#'
#' basin
#'
#' basin2 <- get_nldi_basin(nldi_feature = nldi_nwis,
#'                          simplify = FALSE, split = TRUE)
#'
#' if(inherits(basin, "sf") & inherits(basin2, "sf")) {
#'
#' length(st_coordinates(basin))
#' length(st_coordinates(basin2))
#'
#' plot(st_geometry(st_buffer(st_transform(site, 5070),
#'                           units::set_units(3000, "m"))), border = NA)
#'
#' plot(st_geometry(st_transform(site, 5070)), add = TRUE)
#' plot(st_geometry(st_transform(basin2, 5070)), add = TRUE)
#'
#' plot(st_geometry(st_transform(basin, 5070)), border = "red", add = TRUE)
#'
#' }
#' }
get_nldi_basin <- function(nldi_feature, simplify = TRUE, split = FALSE) {

  tryCatch({
  nldi_feature <- check_nldi_feature(nldi_feature, convert = FALSE)

  url <- utils::URLencode(paste0(
    get_nldi_url(), "/linked-data/",
    nldi_feature[["featureSource"]], "/",
    nldi_feature[["featureID"]],
    "/basin?simplify=", ifelse(simplify, "true", "false"),
    "&splitCatchment=", ifelse(split, "true", "false")))

  out <- hgf_sf(url)

  if(is.null(out))
    warning("Are you sure your featureID exists in the NLDI?",
            call. = FALSE)

  out
  }, error = function(e) {
    warning(e)
    return(NULL)
  })

}


#' @title Get NLDI Feature
#' @description Get a single feature from the NLDI
#' @inheritParams navigate_nldi
#' @return sf data.frame with one feature
#' @examples
#' \donttest{
#' get_nldi_feature(list("featureSource" = "nwissite", featureID = "USGS-05428500"))
#' }
#' @export
get_nldi_feature <- function(nldi_feature) {
  nldi_feature <- check_nldi_feature(nldi_feature)

  out <- tryCatch(dataRetrieval::findNLDI(origin = nldi_feature),
                  error = function(e) NULL)

  if(is.null(out)) {
    warning(paste("No feature found from NLDI, it is not in the featureSource",
                  "you are looking in because it was not indexed or is outside",
                  "the CONUS NLDI domain."))
  }

  return(out$origin)
}

#' Get NLDI Index
#' @description uses the Network Linked Data Index to retrieve an estimated
#' network location for the given point. If not within a grid cell of a flowline,
#' will use a raindrop trace service to find the nearest downslope flowline
#' location.
#' @param location numeric WGS84 lon/lat pair (X, Y)
#' @return sf data.frame of estimated network locations for the given point.
#' NULL if the NLDI service is unavailable.
#' @export
#' @examples
#' \donttest{
#' index <- get_nldi_index(c(-89.276, 42.988))
#'
#' if(inherits(index, "sf")) {
#'
#' plot_nhdplus(
#'   bbox = sf::st_bbox(
#'     sf::st_buffer(
#'       sf::st_transform(index[1,], 5070), units::set_units(1000, "m")
#'       )
#'     )
#' )
#' plot(sf::st_geometry(sf::st_transform(index, 3857)), add = TRUE)
#'
#' }
#' }
get_nldi_index <- function(location) {

  url <- utils::URLencode(paste0(
    get_nldi_url(), "/linked-data/hydrolocation?coords=POINT(",
    location[1], "%20", location[2], ")"))

  out <- hgf_sf(url)

  if(is.null(out))
    warning("Make sure your POINT is lon,lat and in the NHDPlusV2 domain.",
            call. = FALSE)

  out

}

#' @importFrom jsonlite fromJSON
#' @noRd
query_nldi <- function(query, base_path = "/linked-data", err_mess = "") {
  nldi_base_url <- paste0(get_nldi_url(), base_path)

  url <- utils::URLencode(paste(nldi_base_url, query, sep = "/"))

  out <- hgf_json(url)

  if(is.null(out) && nchar(err_mess) > 0)
    warning("Something went wrong accessing the NLDI.\n", err_mess,
            call. = FALSE)

  out
}

#' @noRd
check_nldi_feature <- function(nldi_feature, convert = TRUE) {
  expect_names <- c("featureSource", "featureID")
  if (!all(expect_names %in% names(nldi_feature))) {
    if(length(nldi_feature) != 2 | !all(sapply(nldi_feature, is.character)))
      stop(paste0("Missing some required input for NLDI. ",
                  "Expected length 2 character vector or list with optional names: ",
                  paste(expect_names[which(!(expect_names %in%
                                               names(nldi_feature)))],
                        collapse = ", ")))
  }

  nldi_feature <- as.list(nldi_feature)
  names(nldi_feature) <- expect_names

  if(nldi_feature$featureSource == "nwissite" && convert) {
    if(grepl("^USGS-", nldi_feature$featureID)) {
      if(utils::packageVersion("dataRetrieval") <= "2.7.21") {
        nldi_feature$featureID <- gsub("USGS-", "", nldi_feature$featureID)
      }
      nldi_feature$featureSource <- "nwis"
    }
  }

  names(nldi_feature) <- expect_names

  if(convert) {
    stats::setNames(list(nldi_feature[["featureID"]]), nldi_feature[["featureSource"]])
  } else {
    nldi_feature
  }
}
