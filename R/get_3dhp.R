#' Get 3DHP Data
#' @description
#' Calls the 3DHP_all web service and returns sf data.frames for the selected
#' layers. See https://3dhp.nationalmap.gov/arcgis/rest/services/usgs_3dhp_all/FeatureServer
#' for source data documentation.
#'
#' @inherit query_usgs_arcrest details return params
#' @param type character. Type of feature to return. e.g.
#' ("hydrolocation", "flowline", "waterbody", "drainage area", "catchment").
#' If NULL (default) a data.frame of available types is returned
#' @param ids character vector of id3dhp ids, mainstem uris, or
#' workunitid prefixed ids (e.g. "workunitid:300585")
#' @param universalreferenceid character vector of hydrolocation universal
#' reference ids such as reachcodes
#' @export
#' @examples
#' \donttest{
#' AOI <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
#'                                    xmax = -89.24681, ymax = 43.17192),
#'                                  crs = "+proj=longlat +datum=WGS84 +no_defs"))
#'
#' # get flowlines and hydrolocations
#' flowlines <- get_3dhp(AOI = AOI, type = "flowline")
#' hydrolocation <- get_3dhp(AOI = AOI, type = "hydrolocation")
#' waterbody <- get_3dhp(AOI = AOI, type = "waterbody")
#'
#' if(!is.null(waterbody) & !is.null(flowlines) & !is.null(hydrolocation)) {
#' plot(sf::st_geometry(waterbody), col = "lightblue", border = "lightgrey")
#' plot(sf::st_geometry(flowlines), col = "blue", add = TRUE)
#' plot(sf::st_geometry(hydrolocation), col = "grey", pch = "+", add = TRUE) }
#'
#' # given mainstem ids from any source, can query for them in ids.
#'
#' SU <- get_3dhp(ids = "https://geoconnex.us/ref/mainstems/194408",
#'                type = "flowline")
#'
#' if(!is.null(SU))
#'   plot(sf::st_geometry(SU), col = "blue")
#'
#' # get all the waterbodies along the Susquehanna river
#' SU_wb <- get_3dhp(ids = unique(SU$waterbodyid3dhp), type = "waterbody")
#'
#' if(!is.null(SU_wb)) {
#' plot(sf::st_geometry(SU_wb[grepl("Otsego", SU_wb$gnisidlabel),]),
#'      col = "blue", border = "NA") }
#'
#' # given a workunitid, can query for all features in that work unit with
#' # ids = "workunitid:300585" -- not run here as a work unit is tens of
#' # thousands of features.
#'
#' # given universalreferenceid (reachcodes), can query for them but only
#' # for hydrolocations. This is useful for looking up mainstem ids.
#'
#' if(!is.null(hydrolocation)) {
#' get_3dhp(universalreferenceid =
#'            head(unique(hydrolocation$universalreferenceid), 5),
#'          type = "hydrolocation")
#' }
#'}
get_3dhp <- function(AOI = NULL, ids = NULL, type = NULL,
                     universalreferenceid = NULL,
                     t_srs = NULL, buffer = 0.5,
                     page_size = 2000) {

  if(!is.null(universalreferenceid) & (!is.null(type) && !grepl("outlet|reach|hydrolocation", type))) {
    stop("universalereferenceid can only be specified for hydrolocation features")
  }

  where <- NULL
  if(!is.null(universalreferenceid)) {
    if(!is.null(ids)) stop("can not specify both universalreferenceid and other ids")

    # Large "universalreferenceid IN (...)" queries exceed the service gateway
    # timeout (HTTP 504), so split them into smaller requests and combine. A
    # failed chunk degrades to NULL and is dropped rather than failing the
    # whole call.
    id_chunks <- split(universalreferenceid,
                       ceiling(seq_along(universalreferenceid) / 100))

    out <- lapply(id_chunks, function(chunk) {
      chunk_where <- paste0("universalreferenceid IN ('",
                           paste(chunk, collapse = "', '"), "')")
      query_usgs_arcrest(AOI, ids, type, "3DHP_all", chunk_where, t_srs, buffer,
                         page_size)
    })

    out <- out[!vapply(out, is.null, logical(1))]

    if(length(out) == 0) return(NULL)

    return(sf::st_sf(data.table::rbindlist(out)))
  }

  if(!is.null(ids) && grepl("^https://", ids[1])) {
    where <- paste(paste0("mainstemid IN ('",
                          paste(ids, collapse = "', '"), "')"))
    ids <- NULL
  } else if(!is.null(ids) && grepl("^workunitid:", ids[1])) {
    wuids <- sub("^workunitid:", "", ids)
    if(any(wuids == "NHD"))
      stop("\"NHD\" is the default workunitid and is not a useful filter")
    where <- paste0("workunitid IN ('",
                    paste(wuids, collapse = "', '"), "')")
    ids <- NULL
  }

  query_usgs_arcrest(AOI, ids, type, "3DHP_all", where, t_srs, buffer, page_size)

}

#' @title Index Points to the 3DHP Network
#' @description Addresses (snaps) point features to the 3D Hydrography Program
#' (3DHP) network using the HydroAdd3D web service. For each input point the
#' service determines the nearest 3DHP flowline, the snapped location and its
#' elevation, the mainstem of that flowline, the measure along the mainstem,
#' and associated GNIS attributes.
#' @details Points are sent to the service in batches. The `mainstemid` is a
#' geoconnex mainstem uri
#' (e.g. `https://geoconnex.us/ref/mainstems/312091`) that can be passed to
#' \link{get_3dhp} or used with the functions in \link{add_mainstems}.
#'
#' See https://apps.usgs.gov/hydroadd3d for the web service.
#' @param points sf data.frame of POINT features to address. Must have a
#' coordinate reference system with a resolvable EPSG code.
#' @param t_srs character or object compatible with \link[sf]{st_crs}. Target
#' coordinate reference system of the returned features. Defaults to the
#' coordinate reference system of `points`.
#' @param convert_missing logical. If `TRUE` (default), the service no-data
#' values (`zsnap = -9999`, `m = -1`, `snapdistance = -1`) are converted to
#' `NA` and a logical `snapped` column flags points that addressed
#' successfully. If `FALSE`, no data values are returned as-is.
#' @param batch_size integer number of points sent to the service per request.
#' Default 100.
#' @return sf data.frame of POINT (XYZ) features in `t_srs`, one row per input
#' point. The geometry is the snapped location on the 3DHP flowline: the X and
#' Y coordinates are the snapped position and the Z coordinate is the snapped
#' elevation in meters. Points that did not snap carry their original input
#' location as the geometry. Attribute columns:
#' \itemize{
#'   \item `source_id` - input row, echoed for joins
#'   \item `zsnap` - snapped elevation in meters (the geometry Z coordinate)
#'   \item `mainstemid` - geoconnex mainstem uri
#'   \item `m` - measure along the mainstem, 0 = downstream, 100 = upstream
#'   \item `gnisidlabel` / `gnisid` - GNIS name and id
#'   \item `featuredate` - date the 3DHP feature was loaded
#'   \item `snapdistance` - distance in meters from the original to the snapped
#'   point
#'   \item `snapdate` - timestamp of the addressing run
#'   \item `snapped` - logical, added when `convert_missing = TRUE`, flagging
#'   points that addressed successfully
#' }
#' @export
#' @importFrom jsonlite toJSON
#' @examples
#' \donttest{
#' points <- sf::st_sf(
#'   id = c(1, 2, 3),
#'   geometry = sf::st_sfc(sf::st_point(c(-76.86934, 39.49328)),
#'                         sf::st_point(c(-76.91711, 39.40884)),
#'                         sf::st_point(c(-76.88081, 39.36354)),
#'                         crs = 4326))
#'
#' indexed <- get_3dhp_index(points)
#'
#' if(inherits(indexed, "sf")) {
#'
#' # points that did not snap to a flowline
#' indexed[!indexed$snapped, ]
#'
#' # pull the flowline a point was addressed to
#' get_3dhp(ids = indexed$mainstemid[indexed$snapped][1], type = "flowline")
#'
#' }
#' }
get_3dhp_index <- function(points, t_srs = NULL, convert_missing = TRUE,
                           batch_size = 100) {

  if(!inherits(points, "sf"))
    stop("points must be an sf data.frame.", call. = FALSE)

  if(!all(sf::st_geometry_type(points) == "POINT"))
    stop("points must have POINT geometry.", call. = FALSE)

  crs <- sf::st_crs(points)

  if(is.na(crs$epsg))
    stop("points must have a coordinate reference system with a resolvable ",
         "EPSG code.", call. = FALSE)

  if(is.null(t_srs)) t_srs <- crs

  coords <- sf::st_coordinates(points)
  source_id <- as.character(seq_len(nrow(points)))

  req <- data.frame(source_id = source_id,
                    x = coords[, "X"],
                    y = coords[, "Y"],
                    crs = crs$epsg)

  url <- paste0(get_hydroadd_url(), "/v1/address")

  features <- lapply(split_equal_size(req, batch_size), function(b) {
    body <- jsonlite::toJSON(b, dataframe = "rows", auto_unbox = TRUE)
    fc <- hgf_json(url, body = body, content_type = "application/json",
                   simplifyVector = FALSE)
    fc$features
  })

  features <- unlist(features, recursive = FALSE)

  build_3dhp_index_sf(features, source_id, crs, t_srs, convert_missing)
}

#' @noRd
build_3dhp_index_sf <- function(features, source_id, crs, t_srs, convert_missing) {

  pull <- function(name, na) {
    vapply(features, function(f) {
      v <- f$properties[[name]]
      if(is.null(v)) na else v
    }, na)
  }

  df <- data.frame(
    source_id    = pull("source_id", NA_character_),
    zsnap        = pull("zsnap", NA_real_),
    mainstemid   = pull("mainstemid", NA_character_),
    m            = pull("m", NA_real_),
    gnisidlabel  = pull("gnisidlabel", NA_character_),
    gnisid       = pull("gnisid", NA_real_),
    featuredate  = pull("featuredate", NA_character_),
    snapdistance = pull("snapdistance", NA_real_),
    snapdate     = pull("snapdate", NA_character_),
    stringsAsFactors = FALSE)

  geom <- lapply(features, function(f) {
    sf::st_point(as.numeric(unlist(f$geometry$coordinates)), dim = "XYZ")
  })

  out <- sf::st_sf(df, geometry = sf::st_sfc(geom, crs = crs))

  out <- out[match(source_id, out$source_id), , drop = FALSE]

  if(convert_missing) {
    out$snapped <- out$snapdistance != -1
    out$zsnap[out$zsnap == -9999] <- NA_real_
    out$m[out$m == -1] <- NA_real_
    out$snapdistance[out$snapdistance == -1] <- NA_real_
  }

  sf::st_transform(out, t_srs)
}
