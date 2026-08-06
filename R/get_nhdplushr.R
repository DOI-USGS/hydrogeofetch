#' Get NHDPlus HiRes
#' @param hr_dir character directory with geodatabases (gdb search is recursive)
#' @param out_gpkg character path to write output geopackage
#' @param layers character vector with desired layers to return.
#' c("NHDFlowline", "NHDPlusCatchment") is default.
#' Choose from:
#' c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody", "NHDArea", "NHDLine",
#' "NHDPlusSink", "NHDPlusWall", "NHDPoint", "NHDPlusBurnWaterbody",
#' "NHDPlusBurnLineEvent", "HYDRO_NET_Junctions",
#' "WBDHU2", "WBDHU4","WBDHU6", "WBDHU8" "WBDHU10", "WBDHU12", "WBDLine")
#' Set to NULL to get all available.
#' @param pattern character optional regex to select certain files in hr_dir
#' @param check_terminals boolean if TRUE, run \link{make_standalone} on output.
#' @param overwrite boolean should the output overwrite? If false and the output layer
#' exists, it will be read and returned so this function will always return data even
#' if called a second time for the same output. This is useful for workflows. Note that
#' this will NOT delete the entire Geopackage. It will overwrite on a per layer basis.
#' @param keep_cols character vector of column names to keep in the output. If NULL,
#' all will be kept.
#' @param ... parameters passed along to \link{get_hr_data}
#' for "NHDFlowline" layers.
#' @return sf data.frames containing output that may also be written
#' to a geopackage for later use.
#' @details
#' NHDFlowline is joined to value added attributes prior to being
#' returned.
#' Names are not modified from the NHDPlusHR geodatabase.
#' Set layers to "NULL" to get all layers.
#'
#' @importFrom sf st_layers read_sf st_sf write_sf
#' @export
#' @examples
#' \donttest{
#' # a small NHDPlusHR sample, downloaded to a temporary directory
#' work_dir <- file.path(tempdir(check = TRUE), "hr_example")
#' try(source(system.file("extdata/nhdplushr_data.R", package = "hydrogeofetch")))
#'
#' if(file.exists(file.path(work_dir, "03_sub.gpkg"))) {
#'
#' get_nhdplushr(work_dir, pattern = "03_sub.gpkg")
#'
#' get_nhdplushr(work_dir, pattern = "03_sub.gpkg", layers = NULL)
#'
#' get_nhdplushr(work_dir, pattern = "03_sub.gpkg",
#'               layers = "NHDFlowline",
#'               min_size_sqkm = 10, simp = 10, proj = 5070)
#'
#' }
#'
#' # For real data, download subregions first -- see download_nhdplushr().
#' }
get_nhdplushr <- function(hr_dir, out_gpkg = NULL,
                          layers = c("NHDFlowline", "NHDPlusCatchment"),
                          pattern = ".*GDB.gdb$", check_terminals = TRUE,
                          overwrite = FALSE, keep_cols = NULL, ...) {

  gdbs <- do.call(c, as.list(sapply(hr_dir, list.files,
                                    pattern = pattern,
                                    full.names = TRUE, recursive = TRUE,
                                    include.dirs = TRUE, USE.NAMES = FALSE)))

  if(length(gdbs) == 0) {
    # For testing.
    gdbs <- list.files(hr_dir, pattern = "sub.gpkg", full.names = TRUE)
  }

  dup_list_key <- regmatches(basename(gdbs), regexpr("[0-9][0-9][0-9][0-9]", basename(gdbs)))

  if(any(duplicated(dup_list_key))) {
    remove <- gdbs[duplicated(dup_list_key)]
    warning("Found duplicate HU04s in nhdplushr directory? Will not use: \n",
            paste(remove, collapse = "\n"))
    gdbs <- gdbs[!duplicated(dup_list_key)]
  }

  if(is.null(layers)) {
    layers <- st_layers(gdbs[1])

    layers <- layers$name[!is.na(layers$geomtype) & layers$features > 0]
  }

  out_list <- list()

  layer_names <- c()
  if(!is.null(out_gpkg) && file.exists(out_gpkg))
    layer_names <- st_layers(out_gpkg)$name

  for(layer in layers) {
    if(!is.null(out_gpkg) && layer %in% layer_names & !overwrite) {
      out <- read_sf(out_gpkg, layer, check_ring_dir = FALSE)
    } else {

      layer_set <- lapply(gdbs, get_hr_data, layer = layer, ...)

      out <- dplyr::bind_rows(layer_set)

      try(out <- st_sf(out))
    }

    if(grepl("flowline", layer, ignore.case = TRUE) & check_terminals) {
      expect_names <- get(paste0("make_standalone_tonode_attributes"),
                          envir = hydrogeofetch_env)
      if(!all(expect_names %in% names(out))) {
        warning("check_terminals is true but attributes selected do not support the checks.")
      } else {
        tryCatch(out <- make_standalone(out),
                 error = function(e) {
                   warning("Failed to execute 'make_standalone' on output.\n",
                           "Error was:\n", e)
                 })
      }
    }

    if(!is.null(out_gpkg) && (!layer %in% layer_names | overwrite)) {
      write_sf(out, layer = layer, dsn = out_gpkg)
    }

    out_list[layer] <- list(cull_cols(out, keep_cols))

  }
  return(out_list)
}

#' Get NHDPlus HiRes Data
#' @description Use to remove unwanted detail NHDPlusHR data
#' See \link{get_nhdplushr} for examples.
#' @param gdb character path to geodatabase to get data from.
#' @param layer character layer name from geodatabase found with \link[sf]{st_layers}
#' @param min_size_sqkm numeric minimum basin size to be included in the output
#' @param proj a projection specification compatible with \link[sf]{st_crs}
#' @param simp numeric simplification tolerance in units of projection
#' @param rename boolean if TRUE, hydrogeofetch standard attribute values will
#' be applied.
#' @export
#' @return sf data.frame containing requested data
#' @importFrom sf st_transform st_simplify st_crs st_drop_geometry st_geometry
#' @importFrom sf st_cast st_multilinestring st_zm st_geometry<-
#' @importFrom dplyr select group_by filter ungroup distinct
get_hr_data <- function(gdb, layer = NULL, min_size_sqkm = NULL,
                        simp = NULL, proj = NULL, rename = TRUE) {
  if(layer == "NHDFlowline") {
    hr_data <- suppressWarnings(read_sf(gdb, "NHDPlusFlowlineVAA"))
    hr_data <- select(hr_data, -dplyr::matches(c("ReachCode", "VPUID"), ignore.case = TRUE))

    join_col <- names(hr_data)[grepl("nhdplusid", names(hr_data), ignore.case = TRUE)]

    hr_data <- left_join(st_zm(read_sf(gdb, layer)), hr_data, by = join_col)

    fix <- which( # In the case things come in as non-linestring geometries
      !sapply(st_geometry(hr_data),
              function(x) class(x)[2]) %in% c("LINESTRING", "MULTILINESTRING"))

    for(f in fix) {
      st_geometry(hr_data)[[f]] <- st_multilinestring(lapply(st_geometry(hr_data)[[f]][[1]],
                                                             st_cast, to = "LINESTRING"),
                                                      dim = "XY")
    }

    hr_data <- st_zm(hr_data)

    if(!is.null(min_size_sqkm)) {

      orig_names <- names(hr_data)
      hr_data <- align_nhdplus_names(hr_data)

      filter_data <- select(st_drop_geometry(hr_data), LevelPathI, TotDASqKM)
      filter_data <- ungroup(filter(group_by(filter_data, LevelPathI),
                                    TotDASqKM == max(TotDASqKM)))
      filter_data <- distinct(filter(filter_data, TotDASqKM > min_size_sqkm))
      hr_data <- hr_data[hr_data$LevelPathI %in% filter_data$LevelPathI, ]


      names(hr_data) <- orig_names
    }

  } else {
    hr_data <- read_sf(gdb, layer)
  }

  if(rename) hr_data <- align_nhdplus_names(hr_data)

  if(!is.null(proj) && st_crs(proj) != st_crs(hr_data))
    hr_data <- st_transform(hr_data, proj)

  if(!is.null(simp) && simp > 0)
    hr_data <- st_simplify(hr_data, dTolerance = simp)

  return(hr_data)
}

cull_cols <- function(x, keep_cols) {

  if(is.null(keep_cols)) return(x)

  keep_cols <- names(x)[sapply(names(x), \(x) any(grepl(x, keep_cols, ignore.case = TRUE)))]

  geom_name <- attr(x, "sf_column")
  if(!geom_name %in% keep_cols) keep_cols <- c(keep_cols, geom_name)

  x[, keep_cols]
}

#' Make isolated NHDPlusHR region a standalone dataset
#' @description Cleans up and prepares NHDPlusHR regional data for use as complete NHDPlus
#' data. The primary modification applied is to ensure that any flowpath that exits the
#' domain is labeled as a terminal path and attributes are propagated upstream such that
#' the domain is independently complete.
#' @param flowlines sf data.frame of NHDPlusHR flowlines.
#' @importFrom sf st_zm write_sf st_drop_geometry
#' @importFrom dplyr group_by filter select
#' @return sf data.frame containing standalone network
#' @export
#' @examples
#' \donttest{
#' library(dplyr)
#' library(sf)
#' source(system.file("extdata/nhdplushr_data.R", package = "hydrogeofetch"))
#'
#' (outlet <- filter(hr_data$NHDFlowline, Hydroseq == min(Hydroseq)))
#' nrow(filter(hr_data$NHDFlowline, TerminalPa == outlet$Hydroseq))
#'
#' hr_data$NHDFlowline <- make_standalone(hr_data$NHDFlowline)
#'
#' (outlet <- filter(hr_data$NHDFlowline, Hydroseq == min(Hydroseq)))
#' nrow(filter(hr_data$NHDFlowline, TerminalPa == outlet$Hydroseq))
#'
#' source(system.file("extdata/nhdplushr_data.R", package = "hydrogeofetch"))
#'
#' # Remove mainstem and non-dendritic stuff.
#' subset <- filter(hr_data$NHDFlowline,
#'                         StreamLeve > min(hr_data$NHDFlowline$StreamLeve) &
#'                           StreamOrde == StreamCalc)
#'
#' subset <- subset_nhdplus(subset$COMID, nhdplus_data = hr_gpkg)$NHDFlowline
#'
#' plot(sf::st_geometry(hr_data$NHDFlowline))
#'
#' flowline_mod <- make_standalone(subset)
#'
#' terminals <- unique(flowline_mod$TerminalPa)
#'
#' colors <- sample(hcl.colors(length(terminals), palette = "Zissou 1"))
#'
#' for(i in 1:length(terminals)) {
#'   fl <- flowline_mod[flowline_mod$TerminalPa == terminals[i], ]
#'   plot(st_geometry(fl), col = colors[i], lwd = 2, add = TRUE)
#' }
#'
#' ol <- filter(flowline_mod, TerminalFl == 1 & TerminalPa %in% terminals)
#'
#' plot(st_geometry(ol), lwd = 2, add = TRUE)
#'}
make_standalone <- function(flowlines) {

  if(any(grepl("tocomid", names(flowlines), ignore.case = TRUE))) {
    flowlines <- check_names(flowlines, "make_standalone_tocomid")

    # Remove non-terminal coastal flowlines
    flowlines <- flowlines[!(flowlines$FCODE == 566 &
                               flowlines$Hydroseq != flowlines$TerminalPa), ]

    outlets <- select(st_drop_geometry(flowlines),
                      "COMID", "toCOMID",
                      "Hydroseq", "TerminalPa",
                      "LevelPathI")

    outlets <- filter(outlets,
                      (is.na(.data$toCOMID) | .data$toCOMID == 0 | !.data$toCOMID %in% .data$COMID)
                      & .data$Hydroseq != .data$TerminalPa)
  } else {
    flowlines <- check_names(flowlines, "make_standalone_tonode")

    # Remove non-terminal coastal flowlines
    flowlines <- filter(flowlines,!(FCODE == 566 & TerminalFl != 1))

    outlets <- select(st_drop_geometry(flowlines),
                      "COMID", "ToNode",
                      "FromNode", "TerminalFl",
                      "Hydroseq", "TerminalPa",
                      "LevelPathI")

    outlets <- left_join(outlets,
                         select(outlets,
                                toCOMID = "COMID", "FromNode"),
                         by = c("ToNode" = "FromNode"),
                         relationship = "many-to-many", na_matches = "never")

    outlets <- filter(outlets,
                      is.na(.data$toCOMID) & .data$TerminalFl == 0)
  }
  outlets <- select(outlets, "Hydroseq", "LevelPathI", "COMID")

  for(i in seq_len(nrow(outlets))) {
    flowlines = fix_term(outlets[i, ], flowlines)
  }

  return(flowlines)
}

#' @importFrom hydroloom navigate_network_dfs
fix_term <- function(term, flowlines) {
  term_hydroseq <- term$Hydroseq
  term_comid <- term$COMID

  if("toCOMID" %in% names(term)) {
    flowlines$toCOMID[flowlines$COMID == term_comid] <- 0
  }

  # old_term_levelpath is the levelpath of the mainstem of the basin.
  old_term_levelpath <- flowlines$LevelPathI[flowlines$Hydroseq == term_hydroseq]
  old_term_levelpath <- old_term_levelpath[!is.na(old_term_levelpath)]

  if("TerminalFl" %in% names(flowlines)) {
    # Set the terminal flag of the new basin outlet.
    flowlines$TerminalFl[flowlines$Hydroseq == term_hydroseq] <- 1
  }
  # Change all terminal path IDs to match the new Termal ID of the basin.
  ut <- navigate_network_dfs(select(flowlines, -any_of(c("Permanent_Identifier"))), term_comid, direction = "up")
  flowlines$TerminalPa[flowlines$COMID %in% unlist(ut)] <- term_hydroseq

  # Change the mainstem levelpath ID to match the new Terminal ID of the basin.
  flowlines$LevelPathI[flowlines$LevelPathI == old_term_levelpath] <- term_hydroseq

  if("DnLevelPat" %in% names(flowlines)) {
    # Change the old Down Level Paths so they point to the new mainstem levelpath ID
    flowlines$DnLevelPat[flowlines$DnLevelPat == old_term_levelpath] <- term_hydroseq

    flowlines$DnLevelPat[flowlines$Hydroseq == term_hydroseq] <- 0
  }

  if("UpLevelPat" %in% names(flowlines)) {
    # Change olf Up Level Path to point to the new mainstem levelpath ID
    flowlines$UpLevelPat[flowlines$UpLevelPat == old_term_levelpath] <- term_hydroseq
  }

  # Make the Down Hydrosequence and Down Level and Path look like an outlet.
  if("DnLevel" %in% names(flowlines)) {
    flowlines$DnLevel[flowlines$Hydroseq == term_hydroseq] <- 0
  }

  if("DnHydroseq" %in% names(flowlines)) {
    flowlines$DnHydroseq[flowlines$Hydroseq == term_hydroseq] <- 0
  }

  return(flowlines)
}
