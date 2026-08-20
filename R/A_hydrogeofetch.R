# Primary HydroShare Data Resource
vaa_hydroshare <-
  'https://www.hydroshare.org/resource/6092c8a62fac45be97a09bfd0b0bf726/data/contents/nhdplusVAA.parquet'

vaa_sciencebase <-
  'https://www.sciencebase.gov/catalog/file/get/63cb311ed34e06fef14f40a3?name=enhd_nhdplusatts.parquet'

hydrogeofetch_env <- new.env()

# NHDPlus Attributes
COMID <- "COMID"
FEATUREID <- "FEATUREID"
Permanent_Identifier <- "Permanent_Identifier"
Hydroseq <- "Hydroseq"
UpHydroseq <- "UpHydroseq"
DnHydroseq <- "DnHydroseq"
DnMinorHyd <- "DnMinorHyd"
UpLevelPat <- "UpLevelPat"
LevelPathI <- "LevelPathI"
LevelPathID <- "LevelPathID"
DnLevelPat <- "DnLevelPat"
DnLevel <- "DnLevel"
ToNode <- "ToNode"
FromNode <- "FromNode"
TotDASqKM <- "TotDASqKM"
AreaSqKM <- "AreaSqKM"
LENGTHKM <- "LENGTHKM"
Pathlength <- "Pathlength"
ArbolateSu <- "ArbolateSu"
StreamCalc <- "StreamCalc"
StreamOrde <- "StreamOrde"
TerminalFl <- "TerminalFl"
Divergence <- "Divergence"
TerminalPa <- "TerminalPa"
StartFlag <- "StartFlag"
FTYPE <- "FTYPE"
FCODE <- "FCODE"
FromMeas <- "FromMeas"
ToMeas <- "ToMeas"
REACHCODE <- "REACHCODE"
REACH_meas <- "REACH_meas"
HUC12 <- "HUC12"
TOHUC <- "TOHUC"
ReachCode <- "ReachCode"
VPUID <- "VPUID"
RPUID <- "RPUID"
toCOMID <- "toCOMID"
WBAREACOMI <- "WBAREACOMI"


# List of input names that should be changed to replacement names
nhdplus_attributes <- list(
  COMID = COMID, NHDPlusID = COMID, nhdplusid = COMID,
  Permanent_Identifier = Permanent_Identifier,
  RPUID = RPUID,
  VPUID = VPUID,
  FEATUREID = FEATUREID,
  WBAREACOMI = WBAREACOMI,
  Hydroseq = Hydroseq, HydroSeq = Hydroseq,
  UpHydroseq = UpHydroseq, UpHydroSeq = UpHydroseq,
  DnHydroseq = DnHydroseq, DnHydroSeq = DnHydroseq,
  DnMinorHyd = DnMinorHyd,
  LevelPathI = LevelPathI,
  LevelPathID = LevelPathID,
  UpLevelPat = UpLevelPat,
  DnLevelPat = DnLevelPat,
  DnLevel = DnLevel,
  ToNode = ToNode,
  FromNode = FromNode,
  TotDASqKM = TotDASqKM, TotDASqKm = TotDASqKM,
  AreaSqKM = AreaSqKM, AreaSqKm = AreaSqKM,
  LENGTHKM = LENGTHKM, LengthKM = LENGTHKM,
  ArbolateSu = ArbolateSu,
  Pathlength = Pathlength, PathLength = Pathlength,
  StreamCalc = StreamCalc,
  StreamOrde = StreamOrde,
  TerminalFl = TerminalFl,
  Divergence = Divergence,
  TerminalPa = TerminalPa,
  StartFlag = StartFlag,
  FTYPE = FTYPE, FType = FTYPE,
  FCODE = FCODE, FCode = FCODE,
  FromMeas = FromMeas,
  ToMeas = ToMeas,
  REACHCODE = REACHCODE, ReachCode = REACHCODE,
  REACH_meas = REACH_meas,
  HUC12 = HUC12,
  TOHUC = TOHUC,
  toCOMID = toCOMID)

.data <- . <- NULL

assign("nhdplus_attributes", nhdplus_attributes, envir = hydrogeofetch_env)

assign("arcrest_root", "https://hydro.nationalmap.gov/arcgis/rest/services/",
       envir = hydrogeofetch_env)

assign("arcrest_3dhp_root", "https://3dhp.nationalmap.gov/arcgis/rest/services/",
       envir = hydrogeofetch_env)

assign("gocnx_ref_base_url", "https://reference.geoconnex.us/",
       envir = hydrogeofetch_env)

assign("ref_rivers_release", "v3.2", envir = hydrogeofetch_env)

assign("ref_rivers_base_url",
       "https://github.com/internetofwater/ref_rivers/releases/download/",
       envir = hydrogeofetch_env)

assign("usgs_water_root", "https://api.water.usgs.gov/fabric/pygeoapi/",
       envir = hydrogeofetch_env)

assign("hydroadd_root", "https://apps.usgs.gov/hydroadd3d/api",
       envir = hydrogeofetch_env)

assign("split_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM"),
       envir = hydrogeofetch_env)

assign("collapse_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM", "LevelPathI", "Hydroseq"),
       envir = hydrogeofetch_env)

assign("reconcile_collapsed_flowlines_attributes",
       c("COMID", "toCOMID", "LENGTHKM", "LevelPathI", "Hydroseq"),
       envir = hydrogeofetch_env)

assign("make_standalone_tonode_attributes",
       c("COMID", "ToNode", "FromNode", "TerminalFl", "Hydroseq", "TerminalPa",
         "LevelPathI", "FCODE"),
       envir = hydrogeofetch_env)

assign("make_standalone_tocomid_attributes",
       c("COMID", "toCOMID", "Hydroseq", "TerminalPa",
         "LevelPathI", "FCODE"), envir = hydrogeofetch_env)

assign("subset_rpu_attributes",
       c("COMID", "Pathlength", "LENGTHKM",
         "Hydroseq", "LevelPathI", "DnLevelPat",
         "RPUID", "ArbolateSu", "TerminalPa"),
       envir = hydrogeofetch_env)

assign("subset_vpu_attributes",
       c(get("subset_rpu_attributes",
             envir = hydrogeofetch_env),
         "VPUID"),
       envir = hydrogeofetch_env)

assign("get_hydro_location_attributes",
       c("COMID", "ToMeas", "FromMeas"),
       envir = hydrogeofetch_env)

assign("get_wb_outlet_mres_attributes",
       c("COMID", "Hydroseq", "WBAREACOMI"),
       envir = hydrogeofetch_env)

assign("get_wb_outlet_hires_attributes",
       c("WBArea_Permanent_Identifier", "Hydroseq"),
       envir = hydrogeofetch_env)

assign("on_off_network_attributes",
       c("COMID", "WBAREACOMI"),
       envir = hydrogeofetch_env)

assign("get_tocomid_attributes",
       c("COMID", "ToNode", "FromNode"),
       envir = hydrogeofetch_env)

# assigned here for record keeping. Used as a status counter in apply functions.
assign("cur_count", 0, envir = hydrogeofetch_env)

check_names <- function(x, function_name, align = TRUE, tolower = FALSE) {
  if(align) {
    x <- align_nhdplus_names(x)
  }
  names_x <- names(x)
  expect_names <- get(paste0(function_name, "_attributes"),
                      envir = hydrogeofetch_env)
  if ( !all(expect_names %in% names_x)) {
    stop(paste0("Missing some required attributes in call to: ",
                function_name, ". Expected: ",
                paste(expect_names[which(!(expect_names %in%
                                             names_x))],
                      collapse = ", "), " or NHDPlusHR equivalents."))
  }

  if(tolower) {
    names(x) <- tolower(names(x))
    if(inherits(x, "sf")) {
      attr(x, "sf_column") <- tolower(attr(x, "sf_column"))
    }
  }

  return(x)
}

default_nhdplus_path <- "../NHDPlusV21_National_Seamless.gdb"

assign("default_nhdplus_path", default_nhdplus_path, envir = hydrogeofetch_env)

nhd_bucket <- "https://prd-tnm.s3.amazonaws.com/"
nhdhr_file_list <- "?prefix=StagedProducts/Hydrography/NHDPlusHR/VPU/Current/GDB/"
archive_nhdhr_file_list <- "?prefix=StagedProducts/Hydrography/NHDPlusHR/VPU/Archive/GDB/"
nhd_file_list <- "?prefix=StagedProducts/Hydrography/NHD/HU4/GDB/"

assign("nhd_bucket", nhd_bucket, envir = hydrogeofetch_env)
assign("nhdhr_file_list", nhdhr_file_list, envir = hydrogeofetch_env)
assign("archive_nhdhr_file_list", archive_nhdhr_file_list, envir = hydrogeofetch_env)

assign("api_water_tier", "prod",
       envir = hydrogeofetch_env)

nhdplus_debug <- function() {
  Sys.getenv("debug_hydrogeofetch") == "true"
}

#' @noRd
get_api_water_tier <- function() {
  tier_env <- Sys.getenv("API_WATER_TIER")
  if(tier_env != "") tier_env
  else get("api_water_tier", envir = hydrogeofetch_env)
}

#' @noRd
get_nldi_url <- function(pygeo = FALSE) {

  tier <- get_api_water_tier()

  if(pygeo) {
    if (tier == "prod") {
      "https://api.water.usgs.gov/nldi/pygeoapi/processes/"
    } else if (tier == "test") {
      "https://labs-beta.waterdata.usgs.gov/api/nldi/pygeoapi/processes/"
    } else {
      stop("only prod or test allowed.")
    }
  } else {
    if (tier == "prod") {
      "https://api.water.usgs.gov/nldi"
    } else if (tier == "test") {
      "https://labs-beta.waterdata.usgs.gov/api/nldi"
    } else {
      stop("only prod or test allowed.")
    }
  }
}

#' @noRd
get_water_url <- function() {
  tier <- get_api_water_tier()
  if (tier == "prod") {
    "https://api.water.usgs.gov/fabric/pygeoapi/"
  } else if (tier == "test") {
    "https://labs-beta.waterdata.usgs.gov/api/fabric/pygeoapi/"
  } else {
    stop("only prod or test allowed.")
  }
}

#' @noRd
get_hydroadd_url <- function() {
  get("hydroadd_root", envir = hydrogeofetch_env)
}

#' Get or set hydrogeofetch data directory
#' @description if left unset, will return the user data dir
#' as returned by `tools::R_user_dir` for this package.
#' @param dir path of desired data directory
#' @return character path of data directory (silent when setting)
#' @importFrom tools R_user_dir
#' @export
#' @examples
#' hydrogeofetch_data_dir()
#'
#' # set it somewhere else, then put it back
#' old_dir <- hydrogeofetch_data_dir()
#'
#' hydrogeofetch_data_dir(file.path(tempdir(check = TRUE), "demo"))
#'
#' hydrogeofetch_data_dir()
#'
#' hydrogeofetch_data_dir(old_dir)
#'
hydrogeofetch_data_dir <- function(dir = NULL) {

  if(is.null(dir)) {

    nhdpt_dat_dir <- try(get("nhdpt_dat_dir", envir = hydrogeofetch_env), silent = TRUE)

    if(inherits(nhdpt_dat_dir, "try-error") || grepl("CRAN", nhdpt_dat_dir)) {
      assign("nhdpt_dat_dir",
             tools::R_user_dir("hydrogeofetch"),
             envir = hydrogeofetch_env)
    }

    return(get("nhdpt_dat_dir", envir = hydrogeofetch_env))


  } else {
    assign("nhdpt_dat_dir",
           dir,
           envir = hydrogeofetch_env)
    return(invisible(get("nhdpt_dat_dir", envir = hydrogeofetch_env)))
  }
}

.onAttach <- function(libname, pkgname) {
  nhdplus_path(default_nhdplus_path, warn = FALSE)
}

#' @title NHDPlus Data Path
#' @description Allows specification of a custom path to a source dataset.
#' Typically this will be the national seamless dataset in
#' geodatabase or geopackage format.
#' @param path character path ending in .gdb or .gpkg
#' @param warn boolean controls whether warning an status messages are printed
#' @return 0 (invisibly) if set successfully, character path if no input.
#' @export
#' @examples
#' nhdplus_path("/data/NHDPlusV21_National_Seamless.gdb")
#'
#' nhdplus_path("/data/NHDPlusV21_National_Seamless.gdb", warn=FALSE)
#'
#' nhdplus_path()
#'
nhdplus_path <- function(path = NULL, warn = FALSE) {
  if (!is.null(path)) {

    assign("nhdplus_data", path, envir = hydrogeofetch_env)

    if (warn) {
      warning("Path does not exist.")
    }

    if (nhdplus_path() == path) {
      invisible(0)
    }
  } else {
      return(get("nhdplus_data", envir = hydrogeofetch_env))
  }
}

#' @title hydrogeofetch cache settings
#' @description
#' Provides an interface to adjust hydrogeofetch `memoise` cache.
#'
#' Mode and timeout can also be set using environment variables.
#' `HYDROGEOFETCH_MEMOISE_CACHE` and `HYDROGEOFETCH_MEMOISE_TIMEOUT` are
#' used unless overridden with this function. The old `NHDPLUSTOOLS_*`
#' names are still recognized as a fallback.
#'
#' The default mode is 'memory', so cached responses live only as long as the
#' R session. Set 'filesystem' to persist them in \link{hydrogeofetch_data_dir},
#' and see \link{hydrogeofetch_cache_clear} to remove them again.
#'
#' @param mode character 'memory' (default) or 'filesystem'
#' @param timeout numeric number of seconds until caches invalidate
#' @return list containing settings at time of calling. If inputs are
#' NULL, current settings. If settings are altered, previous setting values.
#' @export
#'
hydrogeofetch_cache_settings <- function(mode = NULL, timeout = NULL) {
  current_mode <- tryCatch(get("nhdpt_mem_cache", envir = hydrogeofetch_env),
                           error = \(e) "memory") # default to memory
  current_timeout <- tryCatch(get("nhdpt_cache_timeout", envir = hydrogeofetch_env),
                              error = \(e) hydrogeofetch_memoise_timeout())

  if(!is.null(mode) && mode %in% c("memory", "filesystem")) {
    assign("nhdpt_mem_cache", mode, envir = hydrogeofetch_env)
  }

  if(!is.null(timeout) && is.numeric(timeout)) {
    assign("nhdpt_cache_timeout", timeout, envir = hydrogeofetch_env)
  }

  return(invisible(list(mode = current_mode, timeout = current_timeout)))
}

#' @title Clear cached hydrogeofetch data
#' @description Removes files that hydrogeofetch has written to
#' \link{hydrogeofetch_data_dir}. Use this to reclaim space or to force
#' downloaded data to be refreshed.
#'
#' Downloaded datasets are kept until removed with this function -- nothing
#' expires them automatically -- so it is worth calling periodically if you use
#' \link{get_vaa} or \link{add_mainstems} on many different areas.
#'
#' @param what character which cached data to remove. One or more of
#' 'responses' (the `memoise` filesystem cache of web service responses),
#' 'tiles' (basemap tiles cached by \link{plot_nhdplus}), 'vaa' (value added
#' attribute parquet files), 'mainstems' (mainstem lookup tables), or 'all'
#' (the default) for everything in the data directory.
#' @param ask logical if TRUE, the default in an interactive session, asks for
#' confirmation before deleting.
#' @return character vector of paths removed, invisibly.
#' @export
#' @examples
#' # pointed at an empty temporary directory here so the example does not
#' # remove anything you have actually cached
#' old_dir <- hydrogeofetch_data_dir()
#' hydrogeofetch_data_dir(file.path(tempdir(), "hgf_cache_demo"))
#'
#' hydrogeofetch_cache_clear(ask = FALSE)
#'
#' hydrogeofetch_data_dir(old_dir)
#'
hydrogeofetch_cache_clear <- function(what = "all", ask = interactive()) {

  what <- match.arg(what, c("all", "responses", "tiles", "vaa", "mainstems"),
                    several.ok = TRUE)

  dir <- hydrogeofetch_data_dir()

  if(!dir.exists(dir)) return(invisible(character(0)))

  targets <- if("all" %in% what) {
    list.files(dir, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  } else {
    unlist(lapply(what, function(x) {
      switch(x,
             tiles = list.files(dir, pattern = "^tile_cache_dir$",
                                full.names = TRUE),
             vaa = list.files(dir, full.names = TRUE,
                              pattern = paste0("^(nhdplusVAA\\.parquet|",
                                               "enhd_nhdplusatts\\.parquet|",
                                               "streamcat_metadata\\.rds)$")),
             mainstems = list.files(dir, pattern = "_lookup\\.parquet$",
                                    full.names = TRUE),
             # memoise writes files named for the hash of their key, so they
             # are the hex-only names with no extension
             responses = {
               f <- list.files(dir, full.names = TRUE)
               f[grepl("^[0-9a-f]{16,}$", basename(f))]
             })
    }))
  }

  targets <- unique(targets)

  if(length(targets) == 0) {
    message("Nothing cached in ", dir)
    return(invisible(character(0)))
  }

  size <- sum(file.size(targets), na.rm = TRUE)

  if(ask) {
    message("Remove ", length(targets), " item(s) (",
            round(size / 1e6, 1), " MB) from ", dir, "?")
    if(!identical(tolower(readline("Type 'yes' to confirm: ")), "yes")) {
      message("Nothing removed.")
      return(invisible(character(0)))
    }
  }

  unlink(targets, recursive = TRUE, force = TRUE)

  message("Removed ", length(targets), " item(s) (",
          round(size / 1e6, 1), " MB) from ", dir)

  invisible(targets)
}

#' @description resolves the cache mode from the session setting, then the
#' environment variable, then the default.
#' @noRd
hydrogeofetch_memoise_mode <- function() {
  ses_memo_cache <- try(get("nhdpt_mem_cache", envir = hydrogeofetch_env), silent = TRUE)

  if(!inherits(ses_memo_cache, "try-error")) {
    return(ses_memo_cache)
  }

  sys_memo_cache <- Sys.getenv("HYDROGEOFETCH_MEMOISE_CACHE")
  if(sys_memo_cache == "") {
    old <- Sys.getenv("NHDPLUSTOOLS_MEMOISE_CACHE")
    if(old != "") {
      warned <- try(get("warned_cache_env", envir = hydrogeofetch_env), silent = TRUE)
      if(inherits(warned, "try-error")) {
        packageStartupMessage("NHDPLUSTOOLS_MEMOISE_CACHE is deprecated. ",
                              "Set HYDROGEOFETCH_MEMOISE_CACHE instead.")
        assign("warned_cache_env", TRUE, envir = hydrogeofetch_env)
      }
      sys_memo_cache <- old
    }
  }

  # memory is the default: a filesystem cache persists in the user data dir,
  # so it is only used when explicitly asked for.
  if(sys_memo_cache == "filesystem") "filesystem" else "memory"
}

#' @description returns the cache backing the current mode, building it on
#' first use. The filesystem cache is only created once it is asked for, so
#' nothing is written to the user data dir under the default memory mode.
#' @noRd
hydrogeofetch_memoise_backend <- function() {
  mode <- hydrogeofetch_memoise_mode()
  key <- paste0("memo_cache_", mode)

  existing <- try(get(key, envir = hydrogeofetch_env), silent = TRUE)
  if(!inherits(existing, "try-error")) return(existing)

  cache <- if(mode == "filesystem") {
    dir.create(hydrogeofetch_data_dir(), showWarnings = FALSE, recursive = TRUE)
    memoise::cache_filesystem(hydrogeofetch_data_dir())
  } else {
    memoise::cache_memory()
  }

  assign(key, cache, envir = hydrogeofetch_env)

  cache
}

#' @description a memoise cache that forwards every operation to whichever
#' backend the current mode selects. The query functions are memoised once at
#' load and their namespace bindings are then locked, so the indirection is
#' what lets \link{hydrogeofetch_cache_settings} change modes mid-session.
#' @importFrom memoise memoise cache_memory cache_filesystem
#' @importFrom digest digest
#' @noRd
hydrogeofetch_memoise_cache <- function() {
  list(
    digest   = function(...) hydrogeofetch_memoise_backend()$digest(...),
    reset    = function(...) hydrogeofetch_memoise_backend()$reset(...),
    set      = function(...) hydrogeofetch_memoise_backend()$set(...),
    get      = function(...) hydrogeofetch_memoise_backend()$get(...),
    has_key  = function(...) hydrogeofetch_memoise_backend()$has_key(...),
    keys     = function(...) hydrogeofetch_memoise_backend()$keys(...),
    drop_key = function(...) hydrogeofetch_memoise_backend()$drop_key(...)
  )
}

hydrogeofetch_memoise_timeout <- function() {
  ses_timeout <- try(get("nhdpt_cache_timeout", envir = hydrogeofetch_env), silent = TRUE)

  if(!inherits(ses_timeout, "try-error")) {
    return(ses_timeout)
  }

  sys_timeout <- Sys.getenv("HYDROGEOFETCH_MEMOISE_TIMEOUT")
  if(sys_timeout == "") {
    old <- Sys.getenv("NHDPLUSTOOLS_MEMOISE_TIMEOUT")
    if(old != "") {
      warned <- try(get("warned_timeout_env", envir = hydrogeofetch_env), silent = TRUE)
      if(inherits(warned, "try-error")) {
        packageStartupMessage("NHDPLUSTOOLS_MEMOISE_TIMEOUT is deprecated. ",
                              "Set HYDROGEOFETCH_MEMOISE_TIMEOUT instead.")
        assign("warned_timeout_env", TRUE, envir = hydrogeofetch_env)
      }
      sys_timeout <- old
    }
  }

  if(sys_timeout != "") {
    as.numeric(sys_timeout)
  } else {
    60 * 60 * 24
  }
}

.onLoad <- function(libname, pkgname) {
  query_usgs_arcrest <<- memoise::memoise(query_usgs_arcrest,
                                          ~memoise::timeout(hydrogeofetch_memoise_timeout()),
                                          cache = hydrogeofetch_memoise_cache())
  query_usgs_oafeat <<- memoise::memoise(query_usgs_oafeat,
                                            ~memoise::timeout(hydrogeofetch_memoise_timeout()),
                                            cache = hydrogeofetch_memoise_cache())
  query_nldi <<- memoise::memoise(query_nldi,
                                  ~memoise::timeout(hydrogeofetch_memoise_timeout()),
                                  cache = hydrogeofetch_memoise_cache())
}

#' @title Align NHD Dataset Names
#' @description this function takes any NHDPlus dataset and aligns the attribute names with those used in hydrogeofetch.
#' @param x a \code{sf} object of nhdplus flowlines
#' @return data.frame renamed \code{sf} object
#' @export
#' @examples
#' source(system.file("extdata/new_hope_data.R", package = "hydrogeofetch"))
#'
#' names(new_hope_flowline)
#'
#' names(new_hope_flowline) <- tolower(names(new_hope_flowline))
#'
#' new_hope_flowline <- align_nhdplus_names(new_hope_flowline)
#'
#' names(new_hope_flowline)
#'
align_nhdplus_names <- function(x){

  attribute_names <- get("nhdplus_attributes", envir = hydrogeofetch_env)

  # get into correct case
  good_names <- unique(unlist(do.call(rbind, attribute_names))[,1])

  new_names <- old_names <- names(x)

  matched <- match(toupper(names(x)), toupper(good_names))
  replacement_names <- as.character(good_names[matched[which(!is.na(matched))]])

  new_names[which(toupper(old_names) %in% toupper(good_names))] <- replacement_names
  names(x) <- new_names

  # rename to match package
  new_names <- old_names <- names(x)

  matched <- match(names(x), names(attribute_names))
  replacement_names <- as.character(attribute_names[matched[which(!is.na(matched))]])

  new_names[which(old_names %in% names(attribute_names))] <- replacement_names

  names(x) <- new_names

  if("GridCode" %in% names(x) & !"FeatureID" %in% names(x) & !"FEATUREID" %in% names(x))
    names(x)[which(names(x) == "COMID")] <- "FEATUREID"

  return(x)

}

filter_list_kvp <- \(l, key, val, type = NULL, n = NULL) {
  ret <- l[vapply(l, \(x) x[[key]] == val, TRUE)]


  if(!is.null(type)) {
    ret <- ret[vapply(ret, \(x) x[["type"]] == type, TRUE)]
  }

  if(!is.null(n)) {
    ret <- ret[[n]]
  }

  ret
}

extract <- `[[`

split_equal_size <- function (x, n)
{
  nr <- try(nrow(x), silent = TRUE)
  if (inherits(nr, "try-error") | is.null(nr))
    nr <- try(length(x), silent = TRUE)
  if (!inherits(nr, "numeric") & length(nr) != 1)
    stop("x can't be interpreted as a data.frame or list")
  split(x, rep(1:ceiling(nr/n), each = n, length.out = nr))
}

#' @title Trim and Cull NULLs
#' @description Remove NULL arguments from a list
#' @param x a list
#' @keywords internal
#' @return a list
#' @noRd

tc <- function(x) {
  Filter(Negate(is.null), x)
}

#' @importFrom hydroloom get_node get_hydro_location rescale_measures rename_geometry
NULL
