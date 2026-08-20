#' Get Raindrop Trace
#' @description Uses a raindrop trace web service to trace the
#' nhdplus digital elevation model to the nearest downslope flowline.
#' @param point sfc POINT including crs as created by:
#' \code{sf::st_sfc(sf::st_point(.. ,..), crs)}
#' @param direction character \code{"up"}, \code{"down"}, or \code{"none"}.
#' Controls the portion of the split flowline that is returned along with
#' the raindrop trace line.
#' @return sf data.frame containing raindrop trace and requested
#' portion of flowline.
#' @export
#' @examples
#' \donttest{
#' point <- sf::st_sfc(sf::st_point(x = c(-89.2158, 42.9561)), crs = 4326)
#'
#' (trace <- get_raindrop_trace(point))
#'
#' if(inherits(trace, "sf")) {
#' bbox <- sf::st_bbox(trace) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' hydrogeofetch::plot_nhdplus(bbox = bbox, cache_data = FALSE)
#'
#' plot(sf::st_transform(sf::st_sfc(point, crs = 4326), 3857), add = TRUE)
#' plot(sf::st_transform(sf::st_geometry(trace)[1], 3857), add = TRUE, col = "red")
#' plot(sf::st_transform(sf::st_geometry(trace)[2], 3857), add = TRUE, col = "black")
#' }
#' }
#'
get_raindrop_trace <- function(point, direction = "down") {

  point <- check_point(point)[[1]]

  url_base <- get_nldi_url(pygeo = TRUE)

  url <- paste0(url_base, "nldi-flowtrace/execution")

  allowed_direction <- c("up", "down", "none")

  if(!direction %in% allowed_direction)
    stop(paste("direction must be in",
               paste(allowed_direction, collapse = ", ")))

  return(hgf_sf(url, body = make_json_input_trace(point, direction = direction),
                content_type = "application/json"))

}

#' Get split catchment
#' @description Uses a catchment splitting web service to retrieve
#' the portion of a catchment upstream of the point provided.
#' @param point scf POINT including crs as created by:
#' \code{sf::st_sfc(sf::st_point(.. ,..), crs)}.
#' @param upstream logical If TRUE, the entire drainage basin upstream
#' of the point provided is returned in addition to the local catchment.
#' @return sf data.frame containing the local catchment, the split portion
#' and optionally the total drainage basin.
#' @details
#' This service works within the conterminous US NHDPlusV2 domain. If the point
#' provided falls on an NHDPlusV2 flowline as retrieved from \link{get_raindrop_trace}
#' the catchment will be split across the flow line. IF the point is not
#' along the flowline a small sub catchment will typically result. As a result,
#' most users of this function will want to use \link{get_raindrop_trace} prior
#' to calls to this function.
#'
#' An attempt is made to eliminate polygon shards if they exist in the output.
#' However, there is a chance that this function will return a multipolygon
#' data.frame.
#'
#' @export
#' @examples
#' \donttest{
#' point <- sf::st_sfc(sf::st_point(x = c(-89.2158, 42.9561)), crs = 4326)
#'
#' trace <- get_raindrop_trace(point)
#'
#' if(inherits(trace, "sf")) {
#'
#' (snap_point <- sf::st_sfc(sf::st_point(trace$intersection_point[[1]]),
#'                           crs = 4326))
#'
#' (catchment <- get_split_catchment(snap_point))
#'
#' if(inherits(catchment, "sf")) {
#' bbox <- sf::st_bbox(catchment) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' hydrogeofetch::plot_nhdplus(bbox = bbox, cache_data = FALSE)
#'
#' if(length(sf::st_geometry(catchment)) > 1) {
#'   plot(sf::st_transform(sf::st_geometry(catchment)[2], 3857), add = TRUE, col = "black")
#' }
#' plot(sf::st_transform(sf::st_geometry(catchment)[1], 3857), add = TRUE, col = "red")
#' plot(sf::st_transform(sf::st_sfc(point, crs = 4326), 3857), add = TRUE, col = "white")
#' }
#'
#' (catchment <- get_split_catchment(snap_point, upstream = FALSE))
#'
#'
#' if(inherits(catchment, "sf")) {
#' bbox <- sf::st_bbox(catchment) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' hydrogeofetch::plot_nhdplus(bbox = bbox, cache_data = FALSE)
#'
#' plot(sf::st_transform(sf::st_geometry(catchment)[1], 3857), add = TRUE, col = "red")
#' if(length(sf::st_geometry(catchment)) > 1) {
#'   plot(sf::st_transform(sf::st_geometry(catchment)[2], 3857), add = TRUE, col = "black")
#' }
#' plot(sf::st_transform(sf::st_sfc(point, crs = 4326), 3857), add = TRUE, col = "white")
#' }
#'
#' pour_point <- sf::st_sfc(sf::st_point(x = c(-89.25619, 42.98646)), crs = 4326)
#'
#' (catchment <- get_split_catchment(pour_point, upstream = FALSE))
#'
#' if(inherits(catchment, "sf")) {
#' bbox <- sf::st_bbox(catchment) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' hydrogeofetch::plot_nhdplus(bbox = bbox, cache_data = FALSE)
#'
#' plot(sf::st_transform(sf::st_geometry(catchment)[1], 3857), add = TRUE, col = "red")
#' if(length(sf::st_geometry(catchment)) > 1) {
#'   plot(sf::st_transform(sf::st_geometry(catchment)[2], 3857), add = TRUE, col = "black")
#' }
#' plot(sf::st_transform(sf::st_sfc(pour_point, crs = 4326), 3857), add = TRUE, col = "white")
#' }
#'
#' }
#'}
#'
get_split_catchment <- function(point, upstream = TRUE) {

  point <- check_point(point)[[1]]

  url_base <- get_nldi_url(pygeo = TRUE)

  url <- paste0(url_base, "nldi-splitcatchment/execution")

  out <- hgf_sf(url, body = make_json_input_split(point, upstream),
                content_type = "application/json")

  if(is.null(out))
    message("Ensure that the point you submitted is within the ",
            "coterminous US and consider trying get_raindrop_trace to ensure ",
            "your point is not too close to a catchment boundary.")

  try({
    if(!is.null(out)) {
      out <- sf::st_make_valid(out)

      sf::st_geometry(out) <- sf::st_sfc(lapply(sf::st_geometry(out), remove_shards),
                                         crs = sf::st_crs(out))

      if(sf::st_geometry_type(out, by_geometry = FALSE) == "GEOMETRY") {
        sf::st_geometry(out) <- sf::st_sfc(lapply(sf::st_geometry(out),
                                                  sf::st_cast, to = "MULTIPOLYGON"),
                                           crs = sf::st_crs(out))
      }
    }
  }, silent = TRUE)

  return(out)
}

remove_shards <- function(g, thresh = 0.01) {

  if(length(g) == 1) return(sf::st_polygon(g[[1]]))

  p <- sf::st_cast(sf::st_sfc(g), "POLYGON")

  a <- sf::st_area(p)

  p <- p[a > max(a) * thresh]

  if(length(p) > 1) return(sf::st_multipolygon(p))

  sf::st_polygon(p[[1]])
}

#' Get Cross Section From Point (experimental)
#' @description Uses a cross section retrieval web services to retrieve a
#' cross section given a point and specified width. Orientation is determined
#' based on direction of the flowline found near point. This function uses
#' a 10m National Elevation Dataset request on the back end.
#' @param point sfc POINT including crs as created by:
#' \code{sf::st_sfc(sf::st_point(.. ,..), crs)}
#' @param width Cross section width in meters.
#' @param num_pts numeric number of points to retrieve along the cross section.
#' @return sf data.frame containing points retrieved.
#' @export
#' @examples
#' \donttest{
#' point <- sf::st_sfc(sf::st_point(x = c(-105.97218, 36.17592)), crs = 4326)
#'
#' (xs <- get_xs_point(point, 300, 100))
#'
#' if(inherits(xs, "sf")) {
#'
#' bbox <- sf::st_bbox(xs) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' hydrogeofetch::plot_nhdplus(bbox = bbox, cache_data = FALSE)
#'
#' plot(sf::st_transform(sf::st_geometry(xs), 3857), pch = ".", add = TRUE, col = "red")
#' plot(sf::st_transform(sf::st_sfc(point, crs = 4326), 3857), add = TRUE)
#'
#' plot(xs$distance_m, xs$elevation_m)
#' }
#' }
#'
get_xs_point <- function(point, width, num_pts) {

  point <- check_point(point)[[1]]

  url_base <- get_nldi_url(pygeo = TRUE)

  url <- paste0(url_base, "nldi-xsatpoint/execution")

  get_xs(url, make_json_input_xspt, point, width, num_pts)

}

#' Get Cross Section Endpoints (experimental)
#' @description Uses a cross section retrieval web services to retrieve a
#' cross section between two endpoints.
#' @param point1 sfc POINT including crs as created by:
#' \code{sf::st_sfc(sf::st_point(.. ,..), crs)}
#' @param point2 sfc POINT including crs.
#' @param num_pts numeric number of points to retrieve along the cross section.
#' @param res integer resolution of 3D Elevation Program data to request.
#' Must be one of: 1, 3, 5, 10, 30, 60.
#' @return sf data.frame containing points retrieved.
#' @export
#' @examples
#' \donttest{
#' point1 <- sf::st_sfc(sf::st_point(x = c(-105.9667, 36.17602)), crs = 4326)
#' point2 <- sf::st_sfc(sf::st_point(x = c(-105.97768, 36.17526)), crs = 4326)
#'
#' (xs <- get_xs_points(point1, point2, 100))
#'
#' if(inherits(xs, "sf")) {
#'
#' bbox <- sf::st_bbox(xs) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' hydrogeofetch::plot_nhdplus(bbox = bbox, cache_data = FALSE)
#'
#' plot(sf::st_transform(sf::st_geometry(xs), 3857), pch = ".", add = TRUE, col = "red")
#' plot(sf::st_transform(sf::st_sfc(point1, crs = 4326), 3857), add = TRUE)
#' plot(sf::st_transform(sf::st_sfc(point2, crs = 4326), 3857), add = TRUE)
#'
#' plot(xs$distance_m, xs$elevation_m)
#' }
#' }
#'
get_xs_points <- function(point1, point2, num_pts, res = 1) {

  point1 <- check_point(point1)[[1]]
  point2 <- check_point(point2)[[1]]

  url_base <- get_nldi_url(pygeo = TRUE)

  url <- paste0(url_base, "nldi-xsatendpts/execution")

  check_res(res)

  get_xs(url, make_json_input_xspts, point1, point2, num_pts, res)

}

check_res <- function(res) {
  if(!res %in% c(1, 3, 5, 10, 30, 60)) {
    stop("res input must be one of 1, 3, 5, 10, 30, 60")
  }
  return(invisible(TRUE))
}

#' Get Elevation Along Path (experimental)
#' @description Uses a cross section retrieval web services to retrieve elevation
#'  along a path.
#' @param points sf data.frame containing a point column.
#' @param num_pts numeric number of points to retrieve along the cross section.
#' @param res integer resolution of 3D Elevation Program data to request.
#' Must be one of: 1, 3, 5, 10, 30, 60.
#' @param status logical
#' @return sf data.frame containing points retrieved. Names include
#' "id", "distance_m", "elevation_m", "spatial_ref", "geometry",
#' and ".group". .group tracks which input point each set of output
#' points belongs to.
#' @export
#' @examples
#' \donttest{
#' point1 <- sf::st_sfc(sf::st_point(x = c(-105.9667, 36.17602)), crs = 4326)
#' point2 <- sf::st_sfc(sf::st_point(x = c(-105.97768, 36.17526)), crs = 4326)
#' point3 <- sf::st_sfc(sf::st_point(x = c(-105.98869, 36.17450)), crs = 4326)
#'
#' points <- sf::st_as_sf(c(point1, point2, point3))
#'
#' (xs <- get_elev_along_path(points, 100))
#'
#' if(inherits(xs, "sf")) {
#'
#' bbox <- sf::st_bbox(xs) + c(-0.005, -0.005, 0.005, 0.005)
#'
#' hydrogeofetch::plot_nhdplus(bbox = bbox, cache_data = FALSE)
#'
#' plot(sf::st_transform(sf::st_geometry(xs), 3857), pch = ".", add = TRUE, col = "red")
#' plot(sf::st_transform(sf::st_sfc(point1, crs = 4326), 3857), add = TRUE)
#' plot(sf::st_transform(sf::st_sfc(point2, crs = 4326), 3857), add = TRUE)
#' plot(sf::st_transform(sf::st_sfc(point3, crs = 4326), 3857), add = TRUE)
#'
#' plot(xs$distance_m, xs$elevation_m)
#' }
#'
#' }
#'
get_elev_along_path <- function(points, num_pts, res = 1, status = TRUE) {

  url_base <- get_nldi_url(pygeo = TRUE)

  url <- paste0(url_base, "nldi-xsatendpts/execution")

  check_res(res)

  get_elev(url, make_json_input_xspts, points, num_pts, res, status)

}


get_elev <- function(url, fun, points, num_pts, res, status) {

  points <- split(points, seq_len(nrow(points)))

  points <- lapply(points, check_point)

  points <- lapply(points, "[[", 1)

  data_elev <- data.frame()
  dist <- vector()

  for(i in 1:(length(points)-1)) {

    if(status)
      message(paste("Requestion segment", i, "of", (length(points)-1)))

    data <- get_xs(url, fun, points[[i]], points[[i+1]], num_pts, res)

    if(is.null(data)) {
      return(NULL)
    }

    data$.group <- i

    if(i == 1){

      if(num_pts %% 2 != 0){
        dist[[i]] <- data[[num_pts, 'distance_m']]
      } else {
        dist[[i]] <- data[[num_pts + 1, 'distance_m']]
      }

    } else {

      data[['distance_m']] <- dist[i-1] + data[['distance_m']]

      if(num_pts %% 2 != 0){
        dist[[i]] <- data[[num_pts, 'distance_m']]
      } else {
        dist[[i]] <- data[[num_pts + 1, 'distance_m']]
      }

    }

    data_elev <- rbind(data_elev, data)

  }

  data_elev
}

#' @importFrom dplyr rename
get_xs <- function(url, fun, ...) {
  sf <- hgf_sf(url, body = fun(...), content_type = "application/json")

  if(is.null(sf)) {
    message("No cross section returned. This is an experimental service that ",
            "may be temporarily unavailable or unable to return a result for ",
            "the requested location; try again later or a different point.")
    return(NULL)
  }

  rename(sf,
         distance_m = "distance",
         elevation_m = "elevation")
}

check_point <- function(p) {
  mess <- "Point must be of type sfc and have a CRS declared."

  if(inherits(p, "sf")) p <- sf::st_geometry(p)

  if(!inherits(p, "sfc")) stop(mess)

  tryCatch({
    sf::st_transform(p, 4326)
  }, error = function(e) {
    stop(paste(mess, "Original error was: \n", e))
  })
}

make_json_input_trace <- function(p, raindrop = TRUE, direction = "down") {

  jsonlite::toJSON(list(inputs = list(list(id = "lat",
                                           type = "text/plain",
                                           value = as.character(p[2])),
                                      list(id = "lon",
                                           type = "text/plain",
                                           value = as.character(p[1])),
                                      list(id = "raindroptrace",
                                           type = "text/plain",
                                           value = ifelse(raindrop,
                                                          "true", "false")),
                                      list(id = "direction",
                                           type = "text/plain",
                                           value = direction))),
                   pretty = TRUE, auto_unbox = TRUE)
}

make_json_input_split <- function(p, upstream = TRUE) {

  jsonlite::toJSON(list(inputs = list(list(id = "lat",
                                           type = "text/plain",
                                           value = as.character(p[2])),
                                      list(id = "lon",
                                           type = "text/plain",
                                           value = as.character(p[1])),
                                      list(id = "upstream",
                                           type = "text/plain",
                                           value = ifelse(upstream,
                                                          "true", "false")),
                                      list(id = "simplified",
                                           type = "text/plain",
                                           value = "false"))),
                   pretty = TRUE, auto_unbox = TRUE)

}

make_json_input_xspt <- function(p, w, n) {

  jsonlite::toJSON(list(inputs = list(list(id = "lat",
                                           type = "text/plain",
                                           value = as.character(p[2])),
                                      list(id = "lon",
                                           type = "text/plain",
                                           value = as.character(p[1])),
                                      list(id = "width",
                                           type = "text/plain",
                                           value = as.character(w)),
                                      list(id = "numpts",
                                           type = "text/plain",
                                           value = as.character(n)))),
                   pretty = TRUE, auto_unbox = TRUE)
}


make_json_input_xspts <- function(p1, p2, n, r) {

  jsonlite::toJSON(list(inputs = list(list(id = "lat",
                                           type = "text/plain",
                                           value = as.character(c(p1[2], p2[2]))),
                                      list(id = "lon",
                                           type = "text/plain",
                                           value = as.character(c(p1[1], p2[1]))),
                                      list(id = "3dep_res",
                                           type = "text/plain",
                                           value = as.character(r)),
                                      list(id = "numpts",
                                           type = "text/plain",
                                           value = as.character(n)))),
                   pretty = TRUE, auto_unbox = TRUE)
}
