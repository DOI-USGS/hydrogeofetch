#' @title Prep NHDPlus Data
#' @description Function to prep NHDPlus data for use by hydrogeofetch functions
#' @param flines data.frame NHDPlus flowlines including:
#' COMID, LENGTHKM, FTYPE (or FCODE), TerminalFl, FromNode, ToNode, TotDASqKM,
#' StartFlag, StreamOrde, StreamCalc, TerminalPa, Pathlength,
#' and Divergence variables.
#' @param min_network_size numeric Minimum size (sqkm) of drainage network
#' to include in output.
#' @param  min_path_length numeric Minimum length (km) of terminal level
#' path of a network.
#' @param  min_path_size numeric Minimum size (sqkm) of outlet level
#' path of a drainage basin. Drainage basins with an outlet drainage area
#' smaller than this will be removed.
#' @param purge_non_dendritic logical Should non dendritic paths be removed
#' or not.
#' @param warn logical controls whether warning an status messages are printed
#' @param error logical controls whether to return potentially invalid data with a warning rather than an error
#' @param skip_toCOMID logical if TRUE, toCOMID will not be added to output.
#' @param align_names logical
#' @return data.frame with prepared NHDPlus network attributes.
#' @importFrom dplyr select filter left_join group_split group_by bind_rows all_of
#' @keywords internal
prepare_nhdplus <- function(flines,
                            min_network_size = 0,
                            min_path_length = 0,
                            min_path_size = 0,
                            purge_non_dendritic = TRUE,
                            warn = TRUE,
                            error = TRUE,
                            skip_toCOMID = FALSE,
                            align_names = TRUE) {

  req_names <- c("COMID", "LENGTHKM", "TerminalFl",
                 "Hydroseq", "TotDASqKM", "StartFlag", "TerminalPa",
                 "Pathlength", "Divergence",
                 "LevelPathI")

  dend <- c("StreamOrde", "StreamCalc")
  sz <- c("TotDASqKM", "Hydroseq", "LevelPathI")
  tocomid <- c("ToNode", "FromNode")

  dont_return <- tolower(c("ToNode", "FromNode", "TerminalFl",
                           "StartFlag", "StreamOrde", "StreamCalc",
                           "TerminalPa", "Pathlength", "Divergence"))

  if(purge_non_dendritic) {
    req_names <- c(req_names, dend)
  }

  if(min_path_size > 0 | min_network_size > 0 | min_path_length > 0) {
    req_names <- c(req_names, sz)
  }

  if(!skip_toCOMID) {
    req_names <- c(req_names, tocomid)
  }

  assign("prepare_nhdplus_attributes",
         unique(req_names),
         envir = hydrogeofetch_env)

  flines <- check_names(flines, "prepare_nhdplus", tolower = TRUE)

  if ("sf" %in% class(flines)) {
    if (warn) warning("removing geometry")
    flines <- st_drop_geometry(flines)
  }

  orig_rows <- nrow(flines)

  flines <- filter_coastal(flines)

  flines <- flines[ , names(flines) %in% tolower(req_names)]

  if (!any(flines$terminalfl == 1)) {
    warning("Got NHDPlus data without a Terminal catchment. Attempting to find it.")
    if (all(flines$terminalpa == flines$terminalpa[1])) {
      out_ind <- which(flines$hydroseq == min(flines$hydroseq))
      flines$terminalfl[out_ind] <- 1
    } else {
      stop("Multiple networks without terminal flags found. Can't proceed.")
    }
  }

  if (purge_non_dendritic) {

    flines <- filter(flines, .data$streamorde == .data$streamcalc)

  } else {
    if(!skip_toCOMID) {
      flines[["fromnode"]][which(flines$divergence == 2)] <- NA
    }
  }

  if(min_path_size > 0) {

    remove_paths <- group_by(flines, .data$levelpathi)
    remove_paths <- filter(remove_paths, .data$hydroseq == min(.data$hydroseq))
    remove_paths <- filter(remove_paths, .data$totdasqkm < min_path_size &
                             .data$totdasqkm >= 0)$levelpathi
    flines <- filter(flines, !.data$levelpathi %in% remove_paths)

  }

  if(min_network_size > 0 | min_path_length > 0) {

    terminal_filter <- flines$terminalfl == 1 &
      flines$totdasqkm < min_network_size
    start_filter <- flines$startflag == 1 &
      flines$pathlength < min_path_length

    if (any(terminal_filter, na.rm = TRUE) | any(start_filter, na.rm = TRUE)) {

      tiny_networks <- rbind(filter(flines, terminal_filter),
                             filter(flines, start_filter))

      flines <- filter(flines, !flines$terminalpa %in%
                         unique(tiny_networks$terminalpa))

    }
  }

  if (warn) {
    warning(paste("Removed", orig_rows - nrow(flines),
                  "flowlines that don't apply.\n",
                  "Includes: Coastlines, non-dendritic paths, \nand networks",
                  "with drainage area less than",
                  min_network_size, "sqkm, and drainage basins smaller than",
                  min_path_size))
  }

  if(!skip_toCOMID) {

    if(nrow(flines) > 0) {
      flines <- get_tocomid(flines, remove_coastal = FALSE)
    }

    if (!all(flines[["terminalfl"]][which(flines$tocomid == 0)] == 1)) {
      warn <- paste("FromNode - ToNode imply terminal flowlines that are not\n",
                    "flagged terminal. Can't assume NA toCOMIDs go to the ocean.")
      if(error) {
        stop(warn)
      } else {
        warning(warn)
      }
    }
  }

  out <- flines[ , !names(flines) %in% dont_return]

  if(align_names) {
    out <- align_nhdplus_names(out)
  }

  out
}


filter_coastal <- function(flines) {
  if("fcode" %in% names(flines)) {
    flines <- filter(flines, (.data$fcode != 56600))
  } else if("ftype" %in% names(flines)) {
    flines <- filter(flines, (.data$ftype != "Coastline" | .data$ftype != 566))
  } else {
    stop("must provide fcode and/or ftype to filter coastline.")
  }
}

#' Get tocomid
#' @description Given flowlines with fromnode and tonode attributes,
#' will return a toid attribute that is the result of joining
#' tonode and fromnode attributes. In the case that a terminalpa
#' attribute is included, the join is executed by terminalpa group.
#' This is done grouped by terminalpathID because duplicate node
#' ids have been encountered across basins in some datasets. If
#' `remove_coastal` is `TRUE` (the default) either ftype or fcode are
#' required. Uses the \link[hydroloom]{add_toids} function.
#' @param x data.frame with comid, tonode, fromnode, and (optionally)
#' divergence and terminalpa attributes.
#' @param return_dendritic logical if TRUE, a divergence attribute is required
#' (2 indicates diverted path, 1 is main) and diverted paths will be treated
#' as headwaters. If this is FALSE, the return value is a data.frame including
#' the comid and tocomid attributes.
#' @param missing integer value to use for terminal nodes.
#' @param remove_coastal logical remove coastal features prior to generating
#' tocomid values? ftype or fcode are required if `TRUE`. fcode == 56600 or
#' fcode == "Coastline" will be removed.
#' @param add logical if TRUE, a tocomid column will be added, otherwise
#' a data.frame with two columns will be returned.
#' @return data.frame containing comid and tocomid attributes or all
#' attributes provided with comid and tocomid in the first and second columns..
#' @importFrom hydroloom add_toids
#' @export
#' @examples
#' source(system.file("extdata", "sample_flines.R", package = "hydrogeofetch"))
#'
#' tocomid <- get_tocomid(sample_flines)
#'
#' tocomid <- get_tocomid(sample_flines, return_dendritic = FALSE)
#'
get_tocomid <- function(x, return_dendritic = TRUE, missing = 0,
                        remove_coastal = TRUE, add = TRUE) {

  x <- check_names(x, "get_tocomid", tolower = TRUE)

  hy_g <- get_hyg(x, add, "comid")

  x <- st_drop_geometry(x)

  if(remove_coastal)
    x <- filter_coastal(x)

  x <- rename(x, id = "comid")

  if("terminalpa" %in% names(x)) {

    order <- data.frame(id = x$id)

    x <- group_split(group_by(x, .data$terminalpa))
    # TODO: migrate to hydroloom::to_flownetwork() when return_dendritic = FALSE.
    # add_toids(return_dendritic = FALSE) is deprecated as of hydroloom 1.2.0.
    # Blocker: to_flownetwork() requires levelpath on input, which the
    # test_rebuild_topology.R:67 call path (COMID/FromNode/ToNode only) omits.
    x <- bind_rows(lapply(x, function(g) {
      suppressWarnings(add_toids(g, return_dendritic = return_dendritic))
    }))

    x <- left_join(order, x, by = c("id"))

  } else {

    x <- suppressWarnings(add_toids(x, return_dendritic))

  }

  x <- rename(x, comid = "id", tocomid = "toid")

  if(!is.na(missing) & missing != 0) {
    repl <- x[["tocomid"]] == 0
    x[["tocomid"]][repl] <- rep(missing, sum(repl))
  }

  if(add) {

    if(!is.null(hy_g)) {
      x <- sf::st_sf(left_join(x, hy_g, by = "comid"))
    }

    x[ , c("comid", "tocomid",
           names(x)[!names(x) %in% c("comid", "tocomid")])]

  } else {

    as.data.frame(select(x, "comid", "tocomid"))

  }
}

#' @noRd
get_hyg <- function(x, add, id = "comid") {
  if(add && inherits(x, "sf")) {
    select(x, all_of(id))
  } else {
    NULL
  }
}
