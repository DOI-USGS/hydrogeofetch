mainstem_lookup_types <- c("nhdpv2", "nhdphr")

mainstem_lookup_id_col <- function(type) {
  if(type == "nhdpv2") "comid" else "nhdplushr_id"
}

#' @title File path to mainstem lookup table cache
#' @description hydrogeofetch will download and cache a parquet file
#' translating geoconnex mainstem identifiers to NHDPlusV2 comids
#' (\code{type = "nhdpv2"}) or NHDPlusHR NHDPlusIDs (\code{type = "nhdphr"}).
#' Will use the user data dir indicated by \link{hydrogeofetch_data_dir}.
#' @param type character one of "nhdpv2" or "nhdphr"
#' @inherit add_mainstems details
#' @return character file path
#' @keywords internal
get_mainstem_lookup_path <- function(type) {
  if(!type %in% mainstem_lookup_types)
    stop("type must be one of: ", paste(mainstem_lookup_types, collapse = ", "))

  release <- get("ref_rivers_release", envir = hydrogeofetch_env)

  file.path(hydrogeofetch_data_dir(),
            paste0("ref_rivers_", release, "_", type, "_lookup.parquet"))
}

#' @title Download mainstem lookup table from ref_rivers
#' @description Downloads and caches a geoconnex mainstem identifier lookup
#' table on your computer.
#' @inherit add_mainstems details
#' @inheritParams get_mainstem_lookup_path
#' @param path character path where the file should be saved. Default is a
#' persistent system data dir as retrieved by \link{hydrogeofetch_data_dir}.
#' Also see: \link{get_mainstem_lookup_path}
#' @param force logical. Force data re-download. Default = FALSE
#' @return character path to cached data
#' @importFrom arrow read_csv_arrow write_parquet
#' @keywords internal

download_mainstem_lookup <- function(type, path = get_mainstem_lookup_path(type), force = FALSE) {

  if(file.exists(path) & !force) {
    message("File already cached")
    return(path)
  }

  release <- get("ref_rivers_release", envir = hydrogeofetch_env)
  base    <- get("ref_rivers_base_url", envir = hydrogeofetch_env)
  url     <- paste0(base, release, "/", type, "_lookup.csv")

  if(nhdplus_debug()) message(url)

  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

  tmp <- tempfile(fileext = ".csv")

  if(is.null(hgf_download(url, tmp))) return(NULL)

  csv <- tryCatch(arrow::read_csv_arrow(tmp), error = function(e) NULL)

  unlink(tmp)

  expect_names <- c("uri", mainstem_lookup_id_col(type))

  if(is.null(csv) || !all(expect_names %in% names(csv))) {
    warning("Mainstem lookup table for ", type,
            " did not have the expected columns.", call. = FALSE)
    return(NULL)
  }

  arrow::write_parquet(csv, path)

  path
}

#' @noRd
read_mainstem_lookup <- function(type, ids = NULL) {
  path <- get_mainstem_lookup_path(type)

  if(!file.exists(path)) {
    if(is.null(download_mainstem_lookup(type, path))) return(NULL)
  }

  id_col <- mainstem_lookup_id_col(type)

  ds <- open_dataset(path)

  if(!is.null(ids)) ds <- filter(ds, .data[[id_col]] %in% ids)

  as.data.frame(collect(ds))
}

#' @noRd
mainstem_id_from_uri <- function(uri) {
  as.integer(sub(".*?/mainstems/([0-9]+).*", "\\1", uri))
}

#' @noRd
mainstem_uri_from_id <- function(id) {
  paste0("https://geoconnex.us/ref/mainstems/", id)
}

#' @noRd
parse_replacement_uris <- function(x) {
  lapply(x, function(s) regmatches(s, gregexpr("https://geoconnex\\.us/[^']+", s))[[1]])
}

#' @title Add mainstem identifiers
#' @description Joins geoconnex mainstem identifiers onto a table that
#' contains NHDPlusV2 (comid/featureid) or NHDPlusHR (nhdplusid)
#' identifiers using a lookup table.
#' @details Source data comes from the ref_rivers GitHub release
#' \href{https://github.com/internetofwater/ref_rivers/releases}{here}. The
#' csv source data is downloaded once, converted to parquet, and cached in
#' the user data dir indicated by \link{hydrogeofetch_data_dir}.
#' @param x data.frame or sf containing an identifier column joinable to
#' NHDPlusV2 or NHDPlusHR.
#' @param join_col character name of the identifier column in \code{x}.
#' Detected automatically from comid, featureid, or nhdplusid (any case)
#' if not provided.
#' @param join_col_type character one of "nhdpv2" or "nhdphr". Detected
#' automatically from \code{join_col} if not provided.
#' @return \code{x} with mainstem_uri and mainstemid columns added.
#' @export
#' @examples
#' \donttest{
#' # the lookup table is cached in hydrogeofetch_data_dir(); point it at a
#' # temporary directory so this example does not write to user space.
#' old_dir <- hydrogeofetch_data_dir()
#' hydrogeofetch_data_dir(tempdir())
#'
#' add_mainstems(data.frame(comid = c(2804607, 2804621)))
#'
#' hydrogeofetch_data_dir(old_dir)
#' }
add_mainstems <- function(x, join_col = NULL, join_col_type = NULL) {

  names_x <- names(x)

  if(is.null(join_col)) {
    hit <- names_x[tolower(names_x) %in% c("comid", "featureid", "nhdplusid")]

    if(length(hit) == 0)
      stop("Could not detect a comid, featureid, or nhdplusid column in x. ",
           "Specify join_col.", call. = FALSE)

    join_col <- hit[1]
  }

  if(!join_col %in% names_x)
    stop("join_col '", join_col, "' not found in x.", call. = FALSE)

  if(is.null(join_col_type)) {
    join_col_type <- if(tolower(join_col) == "nhdplusid") "nhdphr" else "nhdpv2"
  }

  if(!join_col_type %in% mainstem_lookup_types)
    stop("join_col_type must be one of: ",
         paste(mainstem_lookup_types, collapse = ", "), call. = FALSE)

  hy_g <- get_hyg(x, inherits(x, "sf"), join_col)

  x <- st_drop_geometry(x)

  id_col <- mainstem_lookup_id_col(join_col_type)

  lk <- read_mainstem_lookup(join_col_type, ids = unique(x[[join_col]]))

  if(is.null(lk) || nrow(lk) == 0) {

    x$mainstem_uri <- NA_character_
    x$mainstemid <- NA_integer_

  } else {

    lk$mainstemid <- mainstem_id_from_uri(lk$uri)
    lk <- rename(lk, mainstem_uri = "uri")
    lk <- distinct(select(lk, all_of(c(id_col, "mainstem_uri", "mainstemid"))))

    x <- left_join(x, lk, by = stats::setNames(id_col, join_col))

  }

  if(!is.null(hy_g)) x <- st_sf(left_join(x, hy_g, by = join_col))

  x
}

#' @noRd
fetch_superseded_mainstems <- function() {
  base <- get("gocnx_ref_base_url", envir = hydrogeofetch_env)

  base_call <- paste0(base, "collections/mainstems/items?superseded=True")

  out <- get_features_paging(base_call, limit = 500, status = FALSE)

  if(!inherits(out, "sf") || nrow(out) == 0) return(NULL)

  out$new_uris <- parse_replacement_uris(out$new_mainstemid)

  out
}

#' @noRd
get_mainstem_geometry <- function(uris) {
  geoms <- lapply(unique(uris), function(uri) {
    feat <- hgf_sf(paste0(uri, "?f=json"))
    if(inherits(feat, "sf")) rename(feat["uri"], id = "uri") else feat
  })

  geoms <- geoms[vapply(geoms, inherits, logical(1), what = "sf")]

  if(length(geoms) == 0) return(NULL)

  st_sf(bind_rows(geoms))
}

#' @title Check mainstem identifiers for supersession
#' @description Checks whether geoconnex mainstem identifiers have been
#' superseded by a newer reference release. See \link{update_mainstems} to
#' resolve a superseded mainstem id to its replacement.
#' @param x integer or character vector of geoconnex \code{ref/mainstems} ids,
#' or full mainstem uri (any namespace, e.g. \code{ref/mainstems} or
#' \code{usgs/mainstems}) vector of mainstem identifiers to check. Bare
#' integers/character ids are assumed to be \code{ref/mainstems} ids; a full
#' uri is matched literally, so ids minted under a different namespace are
#' never confused with a \code{ref/mainstems} id that happens to share the
#' same trailing number.
#' @return logical vector the same length as \code{x}. \code{TRUE}
#' indicates the mainstem id has been superseded.
#' @export
#' @examples
#' \donttest{
#' old_dir <- hydrogeofetch_data_dir()
#' hydrogeofetch_data_dir(tempdir())
#'
#' check_mainstems(c(2086165, 2086637))
#'
#' hydrogeofetch_data_dir(old_dir)
#' }
check_mainstems <- function(x) {

  x_chr <- as.character(x)
  is_uri <- grepl("^https?://", x_chr)

  uris <- x_chr
  uris[!is_uri] <- mainstem_uri_from_id(x_chr[!is_uri])

  super <- fetch_superseded_mainstems()

  if(is.null(super)) {
    warning("Could not retrieve superseded mainstems list.", call. = FALSE)
    return(rep(NA, length(x)))
  }

  uris %in% super$uri
}

#' @title Update superseded mainstem identifiers
#' @description Given point features with a known geoconnex mainstem
#' identifier, checks whether the mainstem has been superseded and, if so,
#' re-indexes the point to the replacement mainstem using
#' \link[hydroloom]{index_points_to_lines}. Points whose mainstem has no
#' replacement are left unchanged and flagged "unresolved". See
#' \link{check_mainstems} for a lightweight superseded/current check.
#' @param x sf POINT features with a mainstem identifier column.
#' @param mainstem_col character name of the mainstem identifier column in
#' \code{x}. Detected automatically from a "mainstemid" or "mainstem_id"
#' column if not provided. Values may be bare \code{ref/mainstems} ids or
#' full mainstem uris (any namespace); see \link{check_mainstems}.
#' @param search_radius units distance for the nearest neighbor search
#' passed to \link[hydroloom]{index_points_to_lines}. If \code{NULL}, the
#' default in \link[hydroloom]{index_points_to_lines} is used.
#' @return \code{x} with \code{mainstem_col} updated in place and a
#' \code{mainstem_update_status} column added with values "unchanged",
#' "updated", or "unresolved".
#' @export
#' @examples
#' \donttest{
#' old_dir <- hydrogeofetch_data_dir()
#' hydrogeofetch_data_dir(tempdir())
#'
#' pt <- sf::st_sf(mainstemid = 2086165,
#'                 geometry = sf::st_sfc(sf::st_point(c(-75.567, 43.176)),
#'                                       crs = 4326))
#' update_mainstems(pt)
#'
#' hydrogeofetch_data_dir(old_dir)
#' }
update_mainstems <- function(x, mainstem_col = NULL, search_radius = NULL) {

  if(!inherits(x, "sf") || !all(st_geometry_type(x) == "POINT"))
    stop("x must be an sf object of POINT geometries.", call. = FALSE)

  if(is.null(mainstem_col)) {
    hit <- names(x)[tolower(names(x)) %in% c("mainstemid", "mainstem_id")]

    if(length(hit) == 0)
      stop("Could not detect a mainstemid column in x. Specify mainstem_col.",
           call. = FALSE)

    mainstem_col <- hit[1]
  }

  col_chr <- as.character(x[[mainstem_col]])
  is_uri <- grepl("^https?://", col_chr)

  x_uri <- col_chr
  x_uri[!is_uri] <- mainstem_uri_from_id(col_chr[!is_uri])

  x$mainstem_update_status <- "unchanged"

  super <- fetch_superseded_mainstems()

  if(is.null(super)) {
    warning("Could not retrieve superseded mainstems list.", call. = FALSE)
    return(x)
  }

  is_super <- x_uri %in% super$uri

  if(!any(is_super)) return(x)

  super_uris <- unique(x_uri[is_super])

  for(suri in super_uris) {

    cands <- super$new_uris[[match(suri, super$uri)]]

    rows <- which(x_uri == suri)

    if(length(cands) == 0) {
      x$mainstem_update_status[rows] <- "unresolved"
      next
    }

    cand_lines <- get_mainstem_geometry(cands)

    if(is.null(cand_lines)) {
      x$mainstem_update_status[rows] <- "unresolved"
      next
    }

    pts_5070 <- st_transform(x[rows, ], 5070)
    lines_5070 <- st_transform(cand_lines, 5070)

    idx <- index_points_to_lines(lines_5070, pts_5070,
                                 search_radius = search_radius,
                                 max_matches = 1)

    if(is.null(idx) || nrow(idx) == 0) {
      x$mainstem_update_status[rows] <- "unresolved"
      next
    }

    matched <- rows[idx$point_id]
    x[[mainstem_col]][matched] <-
      ifelse(is_uri[matched], idx$id, mainstem_id_from_uri(idx$id))
    x$mainstem_update_status[matched] <- "updated"

    unmatched <- setdiff(rows, matched)
    if(length(unmatched) > 0)
      x$mainstem_update_status[unmatched] <- "unresolved"
  }

  if(any(x$mainstem_update_status == "unresolved"))
    warning("Some mainstem ids could not be resolved to a replacement. ",
            "See mainstem_update_status column.", call. = FALSE)

  x
}
