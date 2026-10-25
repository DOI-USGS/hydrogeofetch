translate_networks <- c("nhdpv2", "nhdphr", "3dhp")

translate_id_col <- c(nhdpv2 = "comid", nhdphr = "nhdplusid", "3dhp" = "id3dhp")

#' Mainstem uri for each row of x
#' @description The mainstem comes from the row's identifier, never from its
#' geometry. A mainstem_uri column overrides; otherwise the identifier column
#' for the network is looked up (ref_rivers for nhdpv2 and nhdphr, the 3DHP
#' flowline attributes for 3dhp, or a mainstemid column already on x).
#' @returns character vector parallel to the rows of x, NA where no mainstem
#' was found
#' @noRd
resolve_mainstem_uris <- function(x, network) {

  x <- sf::st_drop_geometry(x)

  uri <- if("mainstem_uri" %in% names(x)) as.character(x$mainstem_uri) else
    rep(NA_character_, nrow(x))
  uri[!is.na(uri) & uri == ""] <- NA

  todo <- is.na(uri)

  if(!any(todo)) return(uri)

  id_col <- translate_id_col[[network]]

  if(network == "3dhp" && "mainstemid" %in% names(x)) {
    uri[todo] <- as.character(x$mainstemid[todo])
  } else {

    if(!id_col %in% names(x))
      stop("x needs a mainstem_uri column or a ", id_col, " column for ",
           network, ".", call. = FALSE)

    ids <- x[[id_col]][todo]

    if(network == "3dhp") {
      fl <- get_3dhp(ids = unique(as.character(ids)), type = "flowline")
      if(inherits(fl, "sf"))
        uri[todo] <- fl$mainstemid[match(as.character(ids), fl$id3dhp)]
    } else {
      lk <- read_mainstem_lookup(network, ids = unique(as.numeric(ids)))
      lk_col <- mainstem_lookup_id_col(network)
      if(!is.null(lk))
        uri[todo] <- lk$uri[match(as.character(ids), as.character(lk[[lk_col]]))]
    }
  }

  uri[!is.na(uri) & uri == ""] <- NA

  uri
}

#' TRUE where a mainstem uri has been superseded
#' @description An NA from check_mainstems() (the superseded list could not
#' be read, already warned) is treated as current.
#' @noRd
translate_superseded <- function(uri) {
  out <- rep(FALSE, length(uri))

  u <- unique(uri[!is.na(uri)])

  if(length(u) == 0) return(out)

  sup <- check_mainstems(u)

  out[uri %in% u[!is.na(sup) & sup]] <- TRUE

  out
}

#' Which version of a network a placement holds for
#' @noRd
translate_measured_on <- function(fl, network) {

  if(network == "3dhp") {
    d <- as.Date(as.POSIXct(range(fl$featuredate, na.rm = TRUE) / 1000,
                            origin = "1970-01-01", tz = "UTC"))
    return(paste("3dhp featuredate",
                 paste(unique(format(d)), collapse = "/")))
  }

  paste(network, "ref_rivers",
        get("ref_rivers_release", envir = hydrogeofetch_env))
}

#' Index points on the target flowlines of their own mainstem
#' @noRd
translate_by_geometry <- function(pts, uris, tgt, search_radius, precision) {

  n <- length(uris)

  idx <- hydroloom::index_points_to_lines(
    hydroloom::hy(tgt), sf::st_transform(sf::st_geometry(pts), sf::st_crs(tgt)),
    search_radius = search_radius, precision = precision,
    aggregate_ids = uris)

  idx <- idx[!is.na(idx$id), ]

  m <- match(seq_len(n), idx$point_id)

  data.frame(id = idx$id[m], id_measure = idx$id_measure[m],
             offset = idx$offset[m], past_end_km = NA_real_,
             status = ifelse(is.na(m), "not_placed", "placed"))
}

#' Record points as distances from confluences on the source network and
#' place those distances on the target
#' @noRd
translate_by_hydro_location <- function(pts, uris, from, tgt, search_radius,
                                        precision) {

  n <- length(uris)

  out <- data.frame(id = rep(NA, n), id_measure = NA_real_, offset = NA_real_,
                    past_end_km = NA_real_, status = "not_placed",
                    hydro_location_id = NA_character_, distance_km = NA_real_)

  src <- get_mainstem_flowlines(unique(uris), from, include_confluences = TRUE)

  if(is.null(src)) return(out)

  src <- hydroloom::hy(src)
  tgt <- hydroloom::hy(tgt)

  src_conf <- hydroloom::get_confluences(src)
  tgt_conf <- hydroloom::get_confluences(tgt)

  # only confluences the target also has can carry a distance across
  shared <- src_conf[paste(src_conf$aggregate_id, src_conf$hydro_location_id) %in%
                       paste(tgt_conf$aggregate_id, tgt_conf$hydro_location_id), ]

  on_src <- which(uris %in% src$aggregate_id)

  if(length(on_src) == 0) return(out)

  idx <- hydroloom::index_points_to_lines(
    src, sf::st_transform(sf::st_geometry(pts)[on_src], sf::st_crs(src)),
    search_radius = search_radius, precision = precision,
    aggregate_ids = uris[on_src])

  idx <- idx[!is.na(idx$id), ]

  if(nrow(idx) == 0) return(out)

  rows <- on_src[idx$point_id]

  rec <- hydroloom::add_hydro_location_distance(
    data.frame(id = idx$id, id_measure = idx$id_measure), shared, src)

  put <- hydroloom::index_hydro_location_distance(
    data.frame(aggregate_id = rec$aggregate_id,
               hydro_location_id = rec$hydro_location_id,
               distance_km = rec$distance_km),
    tgt_conf, tgt)

  out$id[rows] <- put$id
  out$id_measure[rows] <- put$id_measure
  out$offset[rows] <- idx$offset
  out$past_end_km[rows] <- put$past_end_km
  out$status[rows] <- put$status
  out$hydro_location_id[rows] <- rec$hydro_location_id
  out$distance_km[rows] <- rec$distance_km

  # no confluence on the mainstem is in both networks, so there is no
  # referent to measure from; the point is known only to be on the mainstem
  none <- rows[is.na(rec$hydro_location_id)]
  out$id[none] <- NA
  out$id_measure[none] <- NA
  out$past_end_km[none] <- NA
  out$status[none] <- "no_shared_hydro_location"

  out
}

#' @title Translate hydro locations between networks
#' @description Places points that are indexed to one network (NHDPlusV2,
#' NHDPlusHR, or 3DHP) on the flowlines of another, keeping each point on the
#' mainstem it is on in the source network. The mainstem comes from the
#' point's source identifier, not from its geometry, so a point near a
#' confluence stays on its own river.
#' @details The mainstem of each point comes from, in order: a
#' \code{mainstem_uri} column on \code{x}; for \code{"3dhp"}, a
#' \code{mainstemid} column; otherwise the identifier column for \code{from}
#' (\code{comid}, \code{nhdplusid} or \code{id3dhp}), looked up in the
#' ref_rivers tables (see \link{add_mainstems}) or the 3DHP flowlines. A
#' superseded mainstem (see \link{check_mainstems}) is reported and not
#' placed; \link{update_mainstems} resolves it.
#'
#' The target flowlines of each mainstem come from
#' \link{get_mainstem_flowlines}. There are two ways to place a point on them.
#'
#' \code{method = "geometry"} indexes the point to the target flowlines of
#' its mainstem with \link[hydroloom]{index_points_to_lines} and
#' \code{aggregate_ids}. \code{offset} is the distance from the point to the
#' target flowline. On NHDPlusV2, whose flowlines are generalized, a
#' \code{precision} of a few tens of meters gives a better measure.
#'
#' \code{method = "hydro_location"} indexes the point on the source flowlines,
#' records it as a distance from the nearest confluence that both networks
#' have on that mainstem (\link[hydroloom]{get_confluences} and
#' \link[hydroloom]{add_hydro_location_distance}), and lays the distance out
#' on the target (\link[hydroloom]{index_hydro_location_distance}). The
#' placement is approximate in absolute terms but stays on the same side of
#' the confluence, which is what matters for upstream drainage. \code{offset}
#' is the distance from the point to the source flowline.
#'
#' \code{status} is one of: \code{placed}; \code{not_placed} (no flowline
#' within \code{search_radius}); \code{no_mainstem}; \code{mainstem_superseded};
#' \code{mainstem_not_in_target} (no target flowlines for the mainstem); and,
#' for \code{"hydro_location"}, \code{no_shared_hydro_location} (no
#' confluence on the mainstem is in both networks, so the point is known only
#' to be on \code{mainstem_uri}), \code{past_end} and
#' \code{hydro_location_not_in_x} as described in
#' \link[hydroloom]{index_hydro_location_distance}. \code{measured_on} names the
#' target network and the ref_rivers release, or for 3DHP the range of
#' \code{featuredate}. 3DHP identifiers are not persistent, so a 3DHP
#' placement holds for that release.
#'
#' The ref_rivers lookup tables are large downloads on first use (see
#' \link{add_mainstems}), so this function has no example.
#'
#' \preformatted{
#' # a point on New Hope River just above the Northeast Creek mouth
#' pt <- sf::st_sf(comid = 8894344,
#'                 geometry = sf::st_sfc(sf::st_point(c(-78.9763, 35.8355)),
#'                                       crs = 4326))
#'
#' translate_hydrolocations(pt, "nhdpv2", "3dhp", method = "hydro_location")}
#'
#' @param x sf POINT data.frame with the source identifier column for
#' \code{from} (\code{comid}, \code{nhdplusid} or \code{id3dhp}) or a
#' \code{mainstem_uri} column.
#' @param from character network \code{x} is indexed to, one of "nhdpv2",
#' "nhdphr", or "3dhp".
#' @param to character network to place \code{x} on, one of "nhdpv2",
#' "nhdphr", or "3dhp".
#' @param method character "geometry" or "hydro_location". See details.
#' @param search_radius units distance passed to
#' \link[hydroloom]{index_points_to_lines}. \code{NULL} uses its default.
#' @param precision numeric meters, passed to
#' \link[hydroloom]{index_points_to_lines}.
#' @return \code{x} with \code{mainstem_uri}, \code{id} (target flowline),
#' \code{id_measure}, \code{offset}, \code{past_end_km}, \code{status},
#' \code{method}, and \code{measured_on} added, and for
#' \code{"hydro_location"} also \code{hydro_location_id} and
#' \code{distance_km}.
#' @export
translate_hydrolocations <- function(x, from, to,
  method = c("geometry", "hydro_location"),
  search_radius = NULL, precision = NA) {

  from <- match.arg(from, translate_networks)
  to <- match.arg(to, translate_networks)
  method <- match.arg(method)

  if(!inherits(x, "sf") || !all(sf::st_geometry_type(x) == "POINT"))
    stop("x must be an sf object of POINT geometries.", call. = FALSE)

  n <- nrow(x)

  uri <- resolve_mainstem_uris(x, from)

  status <- rep(NA_character_, n)
  status[is.na(uri)] <- "no_mainstem"
  status[is.na(status) & translate_superseded(uri)] <- "mainstem_superseded"

  res <- data.frame(id = rep(NA, n), id_measure = NA_real_, offset = NA_real_,
                    past_end_km = NA_real_, status = status,
                    hydro_location_id = NA_character_, distance_km = NA_real_)

  measured_on <- NA_character_

  todo <- unique(uri[is.na(status)])

  if(length(todo) > 0) {

    tgt <- get_mainstem_flowlines(todo, to,
                                  include_confluences = method == "hydro_location")

    in_tgt <- if(is.null(tgt)) rep(FALSE, n) else uri %in% tgt$mainstemid

    res$status[is.na(res$status) & !in_tgt] <- "mainstem_not_in_target"

    rows <- which(is.na(res$status))

    if(length(rows) > 0) {

      measured_on <- translate_measured_on(tgt, to)

      placed <- if(method == "geometry") {
        translate_by_geometry(x[rows, ], uri[rows], tgt, search_radius,
                              precision)
      } else {
        translate_by_hydro_location(x[rows, ], uri[rows], from, tgt,
                                    search_radius, precision)
      }

      for(col in names(placed)) res[[col]][rows] <- placed[[col]]
    }
  }

  x$mainstem_uri <- uri
  x$id <- res$id
  x$id_measure <- res$id_measure
  x$offset <- res$offset
  x$past_end_km <- res$past_end_km
  x$status <- res$status
  x$method <- method
  x$measured_on <- measured_on

  if(method == "hydro_location") {
    x$hydro_location_id <- res$hydro_location_id
    x$distance_km <- res$distance_km
  }

  x
}


#' One unplaced row of the translate_flowlines() piece table
#' @noRd
translate_piece_rows <- function(line_id, status, from_offset = NA_real_,
                                 to_offset = NA_real_) {
  n <- length(line_id)

  data.frame(line_id = line_id, segment_order = rep(NA_integer_, n),
             id = rep(NA, n), id_from_measure = rep(NA_real_, n),
             id_to_measure = rep(NA_real_, n),
             line_from_measure = rep(NA_real_, n),
             line_to_measure = rep(NA_real_, n),
             length_km = rep(NA_real_, n),
             from_offset = rep_len(from_offset, n),
             to_offset = rep_len(to_offset, n), status = status)
}

#' Empty LINESTRINGs, for rows of the piece table with no geometry
#' @noRd
empty_lines <- function(n, crs) {
  sf::st_sfc(rep(list(sf::st_linestring()), n), crs = crs)
}

#' Pieces of the target flowlines covering each source flowline
#' @description Indexes each source flowline's downstream (end) and upstream
#' (start) nodes on the target flowlines of its mainstem and takes the
#' subpath between them.
#' @returns piece table as data.frame, with attribute "geom" holding piece
#' geometry parallel to the rows, or NULL when get_subpath() returned none
#' @noRd
translate_flowline_pieces <- function(src, uris, line_ids, tgt, snap_m,
                                      search_radius, precision) {

  tgt <- hydroloom::hy(tgt)
  src <- sf::st_transform(src, sf::st_crs(tgt))

  ends <- lapply(c(bottom = "end", top = "start"), function(position) {
    nodes <- sf::st_geometry(hydroloom::get_node(src, position))

    idx <- hydroloom::index_points_to_lines(
      tgt, nodes, search_radius = search_radius, precision = precision,
      aggregate_ids = uris)

    idx <- idx[!is.na(idx$id), ]

    idx[match(seq_along(uris), idx$point_id), ]
  })

  bottom <- ends$bottom
  top <- ends$top

  ok <- !is.na(bottom$id) & !is.na(top$id)

  out <- translate_piece_rows(line_ids[!ok], rep("not_placed", sum(!ok)),
                              bottom$offset[!ok], top$offset[!ok])
  geom <- NULL

  if(any(ok)) {
    sp <- hydroloom::get_subpath(
      data.frame(id = bottom$id[ok], id_measure = bottom$id_measure[ok],
                 line_id = line_ids[ok]),
      data.frame(id = top$id[ok], id_measure = top$id_measure[ok]),
      tgt, snap_m)

    # get_subpath() keeps any piece longer than zero; a snapped end at a
    # boundary computed two ways can leave a floating-point sliver
    sliver <- !is.na(sp$id) & sp$length_km < 1e-6

    if(inherits(sp, "sf")) {
      geom <- c(empty_lines(nrow(out), sf::st_crs(sp)),
                sf::st_geometry(sp)[!sliver])
      sp <- sf::st_drop_geometry(sp)
    }

    sp <- as.data.frame(sp)[!sliver, ]
    sp$segment_order <- stats::ave(sp$segment_order, sp$line_id,
                                   FUN = seq_along)

    # measures along the source flowline, 0 at the downstream end of the
    # subpath, from the pieces' lengths
    tot <- stats::ave(sp$length_km, sp$line_id, FUN = sum)
    cum <- stats::ave(sp$length_km, sp$line_id, FUN = cumsum)
    sp$line_from_measure <- 100 * (cum - sp$length_km) / tot
    sp$line_to_measure <- 100 * cum / tot

    k <- match(sp$line_id, line_ids[ok])
    sp$from_offset <- bottom$offset[ok][k]
    sp$to_offset <- top$offset[ok][k]

    sp$status <- ifelse(is.na(sp$id), "collapsed",
                        ifelse(sp$reversed, "inverted", "placed"))

    collapsed <- sp$status == "collapsed"
    sp$line_from_measure[collapsed] <- NA
    sp$line_to_measure[collapsed] <- NA

    out <- rbind(out, sp[names(out)])
  }

  attr(out, "geom") <- geom

  out
}

#' @title Translate flowlines between networks
#' @description Finds the stretch of another network's flowlines (NHDPlusV2,
#' NHDPlusHR, or 3DHP) that covers each given flowline, on the same mainstem.
#' Each source flowline comes back as one or more pieces of target flowlines,
#' each cut by measure.
#' @details The mainstem of each source flowline comes from its identifier,
#' as described in \link{translate_hydrolocations}. The source and target
#' flowlines of that mainstem come from \link{get_mainstem_flowlines}.
#'
#' The downstream (end) and upstream (start) nodes of each source flowline
#' are indexed to the target flowlines of its mainstem with
#' \link[hydroloom]{index_points_to_lines} and \code{aggregate_ids}, and the
#' stretch between them is taken with \link[hydroloom]{get_subpath}. An end
#' within \code{snap_m} meters of a target flowline boundary moves to that
#' boundary, so two source flowlines that share a node share one cut.
#' \code{line_from_measure} and \code{line_to_measure} place each piece along
#' the source flowline, 0 at its downstream end, by the pieces' lengths.
#' \code{from_offset} and \code{to_offset} are the distances from the source
#' flowline's downstream and upstream nodes to the target flowlines, in the
#' units of the target network's coordinate reference system.
#'
#' \code{status} is one of: \code{placed}; \code{inverted} (the source
#' flowline's downstream node is upstream of its upstream node on the
#' target); \code{collapsed} (both ends land on one point, one row with
#' \code{NA} \code{id}); \code{not_placed} (an end has no target flowline
#' within \code{search_radius}); \code{no_mainstem};
#' \code{mainstem_superseded}; \code{source_not_found} (the source flowline is
#' not among the fetched flowlines of its mainstem); and
#' \code{mainstem_not_in_target}. Rows that are not placed have \code{NA}
#' pieces. \code{measured_on} is as described in
#' \link{translate_hydrolocations}. The reverse translation is the same call
#' with \code{from} and \code{to} swapped.
#'
#' The ref_rivers lookup tables are large downloads on first use (see
#' \link{add_mainstems}), so this function has no example.
#'
#' \preformatted{
#' translate_flowlines(8894344, "nhdpv2", "3dhp")}
#'
#' @param ids vector of source flowline identifiers: comids for
#' \code{"nhdpv2"}, NHDPlusIDs for \code{"nhdphr"}, or id3dhp for
#' \code{"3dhp"}.
#' @param snap_m numeric meters, passed to \link[hydroloom]{get_subpath}.
#' @inheritParams translate_hydrolocations
#' @return data.frame with \code{line_id} (the source id),
#' \code{segment_order} (1 at the downstream end), \code{id} (target
#' flowline), \code{id_from_measure}, \code{id_to_measure},
#' \code{line_from_measure}, \code{line_to_measure}, \code{length_km},
#' \code{from_offset}, \code{to_offset}, \code{status}, and
#' \code{measured_on}; an sf data.frame of piece geometry when
#' \pkg{lwgeom} is installed.
#' @export
translate_flowlines <- function(ids, from, to, snap_m = 50,
  search_radius = NULL, precision = NA) {

  from <- match.arg(from, translate_networks)
  to <- match.arg(to, translate_networks)

  id_col <- translate_id_col[[from]]

  src_ids <- unique(ids)
  n <- length(src_ids)

  uri <- resolve_mainstem_uris(stats::setNames(data.frame(src_ids), id_col),
                               from)

  status <- rep(NA_character_, n)
  status[is.na(uri)] <- "no_mainstem"
  status[is.na(status) & translate_superseded(uri)] <- "mainstem_superseded"

  measured_on <- NA_character_
  pieces <- NULL

  todo <- unique(uri[is.na(status)])

  if(length(todo) > 0) {

    src <- get_mainstem_flowlines(todo, from)

    m <- if(is.null(src)) rep(NA_integer_, n) else
      match(as.character(src_ids), as.character(src[[id_col]]))

    status[is.na(status) & is.na(m)] <- "source_not_found"

    tgt <- get_mainstem_flowlines(unique(uri[is.na(status)]), to)

    in_tgt <- if(is.null(tgt)) rep(FALSE, n) else uri %in% tgt$mainstemid

    status[is.na(status) & !in_tgt] <- "mainstem_not_in_target"

    rows <- which(is.na(status))

    if(length(rows) > 0) {
      measured_on <- translate_measured_on(tgt, to)

      pieces <- translate_flowline_pieces(src[m[rows], ], uri[rows],
                                          src_ids[rows], tgt, snap_m,
                                          search_radius, precision)
    }
  }

  done <- !is.na(status)
  out <- translate_piece_rows(src_ids[done], status[done])
  geom <- attr(pieces, "geom")

  if(!is.null(pieces)) out <- rbind(out, pieces)

  ord <- order(match(out$line_id, src_ids), out$segment_order)

  out <- out[ord, ]
  rownames(out) <- NULL
  out$measured_on <- rep(measured_on, nrow(out))

  if(is.null(geom)) return(out)

  geom <- c(empty_lines(sum(done), sf::st_crs(geom)), geom)[ord]

  sf::st_sf(out, geometry = geom)
}
