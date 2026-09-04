#' @title File path to value added attribute (vaa) Cache
#' @description hydrogeofetch will download and cache a parquet file with
#' NHDPlusV2 attribute data sans geometry. This function returns the
#' file path to the cached file. Will use the user data dir indicated
#' by \link{hydrogeofetch_data_dir}.
#' @param updated_network logical default FALSE. If TRUE, returns path to updated
#' network parameters. See \link{get_vaa} for more.
#' @inherit download_vaa details
#' @return character file path
#' @export
#' @examples
#' get_vaa_path()
#'
#' get_vaa_path(updated_network = TRUE)
#'

get_vaa_path <- function(updated_network = FALSE) {
  if(updated_network) {
    file.path(hydrogeofetch_data_dir(), "enhd_nhdplusatts.parquet")
  } else {
    file.path(hydrogeofetch_data_dir(), "nhdplusVAA.parquet")
  }
}

#' @title Available NHDPlusV2 Attributes
#' @description Find variables available from the NHDPlusV2 attribute data.frame
#'
#' Calling this function downloads the attribute table to \link{get_vaa_path}
#' if it is not already cached there. The table is roughly 260 MB, so this
#' function has no example. 
#' 
#' \preformatted{
#' old_dir <- hydrogeofetch_data_dir()
#' hydrogeofetch_data_dir(file.path(tempdir(), "vaa"))
#'
#' get_vaa_names()
#'
#' hydrogeofetch_data_dir(old_dir)}
#' 
#' @inherit download_vaa details
#' @inheritParams get_vaa
#' @return character vector
#' @export
get_vaa_names <- function(updated_network = FALSE) {
  path <- get_vaa_path(updated_network = updated_network)

  check_vaa_path(path, TRUE)

  arrow::open_dataset(path)$schema$names
}

#' @title NHDPlusV2 Attribute Subset
#' @description Return requested NHDPlusV2 Attributes.
#'
#' Calling this function downloads the attribute table to \link{get_vaa_path}
#' if it is not already cached there. The base table is roughly 260 MB and the
#' updated network table another 170 MB, so this function has no example. To
#' try it without writing to your user data directory, point
#' \link{hydrogeofetch_data_dir} at a temporary path first:
#' \preformatted{
#' old_dir <- hydrogeofetch_data_dir()
#' hydrogeofetch_data_dir(file.path(tempdir(), "vaa"))
#'
#' get_vaa("slope")
#' get_vaa(c("slope", "lengthkm"))
#' get_vaa("reachcode", updated_network = TRUE)
#'
#' hydrogeofetch_data_dir(old_dir)}
#' @inherit download_vaa details
#' @param atts character The variable names you would like, always includes comid
#' @param path character path where the file should be saved. Default is a
#' persistent system data as retrieved by \link{hydrogeofetch_data_dir}.
#' Also see: \link{get_vaa_path}
#' @param download logical if TRUE, the default, will download VAA table if not
#' found at path.
#' @param updated_network logical default FALSE. If TRUE, updated network attributes
#' from E2NHD and National Water Model retrieved from
#' \doi{10.5066/P976XCVT}.
#' @return data.frame containing requested VAA data
#' @export

get_vaa <- function(atts = NULL,
                    path = get_vaa_path(),
                    download = TRUE,
                    updated_network = FALSE) {

  check_vaa_path(path, download, FALSE)

  if(updated_network) {
    updated_net_path <- file.path(dirname(path), "enhd_nhdplusatts.parquet")

    check_vaa_path(updated_net_path, download, TRUE)

    new_names <- arrow::open_dataset(updated_net_path)$schema$names
  }

  available_names = get_vaa_names(updated_network)

  if(is.null(atts)) {

    atts <- available_names

  } else {

    bad_atts = atts[!atts %in% available_names]
    atts      = atts[atts %in% available_names]
    if(length(bad_atts) > 0){

      message(paste(bad_atts, collapse = ", "), " not in vaa data. Ignoring...")

    }

  }

  if(updated_network) {

    message("Caution: updated attributes drop some catchments and attributes")

    deprecated_names <- c("streamcalc", "fromnode", "tonode",
                          "dnlevel", "uplevelpat", "uphydroseq",
                          "dnminorhyd", "divdasqkm")

    include_names <- c("comid",
                       atts[!atts %in%
                              c(deprecated_names, new_names)])

    replace_names <- atts[atts %in% new_names & !atts %in% deprecated_names]

    # as.data.frame strips any data.table class arrow may attach on metadata
    # roundtrip failure, keeping the data.frame `[` semantics downstream.
    out <- as.data.frame(arrow::read_parquet(path, col_select = include_names))

    new_comid <- arrow::read_parquet(updated_net_path, col_select = "comid")

    out <- out[match(new_comid$comid, out$comid), , drop = FALSE]

    cols_to_add <- replace_names[replace_names != "comid"]
    if(length(cols_to_add) > 0) {
      out <- cbind(out, as.data.frame(arrow::read_parquet(updated_net_path,
                                                          col_select = cols_to_add)))
    }

    reorder <- match(get_vaa_names(updated_network), names(out))

    reorder <- reorder[!is.na(reorder)]

    return(out[, reorder])

  } else {

    return(as.data.frame(arrow::read_parquet(path, col_select = c('comid', atts[atts != 'comid']))))
  }

}

check_vaa_path <- function(path, download, updated_network = FALSE) {
  if(!file.exists(path)){
    if(download) {
      message("didn't find data, downloading.")
      path <- download_vaa(path, updated_network = updated_network)
    } else {
      stop("need to download data: run `download_vaa()`")
    }
  }
}

#' @title Download NHDPlusVAA data from HydroShare
#' @description downloads and caches NHDPlusVAA data on your computer
#' @details The VAA data is a aggregate table of information from the NHDPlusV2
#' elevslope.dbf(s), PlusFlowlineVAA.dbf(s); and NHDFlowlines. All data
#' originates from the EPA NHDPlus Homepage
#' \href{https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data}{here}.
#' To see the location of cached data on your machine use
#' \code{\link{get_vaa_path}}.
#' To view aggregate data and documentation, see
#' \href{https://www.hydroshare.org/resource/6092c8a62fac45be97a09bfd0b0bf726/}{here}
#' @inheritParams get_vaa
#' @param force logical. Force data re-download. Default = FALSE
#' @return character path to cached data
#' @export

download_vaa <- function(path = get_vaa_path(updated_network), force = FALSE, updated_network = FALSE) {

  if (file.exists(path) & !force) {
    message("File already cached")
  } else {
    url <- ifelse(updated_network, vaa_sciencebase, vaa_hydroshare)

    if(nhdplus_debug()) {
      message(url)
    }

    dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)

    if(is.null(hgf_download(url, path))) return(NULL)
  }
  # return file path
  return(path)
}

#' Get catchment characteristics metadata table
#' @description Download and cache table of catchment characteristics.
#'
#' When \code{source = "usgs"} (the default), returns metadata from:
#' Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes
#' for NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream
#' Watersheds for the Conterminous United States (ver. 3.0, January 2021): U.S.
#' Geological Survey data release, \doi{10.5066/F7765D7V}.
#'
#' When \code{source = "streamcat"}, returns metric metadata from the EPA
#' StreamCat dataset accessed via the \code{StreamCatTools} package (must be
#' installed separately):
#' Weber, Marc H, Hill, Ryan A., Brookes, Allen F. 2024, StreamCatTools: Tools to 
#' work with the StreamCat API within R and access the full suite of StreamCat and 
#' LakeCat metrics, https://usepa.github.io/StreamCatTools
#' @param search character string of length 1 to free search the metadata table.
#' If no search term is provided the entire table is returned.
#' @param source character \code{"usgs"} (default) or \code{"streamcat"}.
#' @param cache logical should cached metadata be used?
#' @return data.frame of characteristic metadata with columns ID, description,
#' units, datasetLabel, datasetURL, themeLabel, themeURL, watershedType, sbid,
#' end, s3_url, and http_url. NULL if the metadata service is unavailable.
#' @importFrom utils read.delim
#' @export
#' @examples
#' \donttest{
#' # the metadata table is cached in hydrogeofetch_data_dir(); point it at a
#' # temporary directory so this example does not write to user space.
#' old_dir <- hydrogeofetch_data_dir()
#' hydrogeofetch_data_dir(file.path(tempdir(check = TRUE), "meta_demo"))
#'
#' get_characteristics_metadata()
#'
#' hydrogeofetch_data_dir(old_dir)
#' }
get_characteristics_metadata <- function(search, source = "usgs", cache = TRUE) {

  source <- match.arg(source, c("usgs", "streamcat"))

  # StreamCat metadata via EPA StreamCat API (StreamCatTools package)
  if(source == "streamcat") {
    check_pkg("StreamCatTools")

    r <- file.path(hydrogeofetch_data_dir(), "streamcat_metadata.rds")

    if(!cache) unlink(r, force = TRUE)

    out <- tryCatch({
      if(file.exists(r) && cache) {
        readRDS(r)
      } else {
        params <- StreamCatTools::sc_get_params(param = "variable_info")

        if(!dir.exists(hydrogeofetch_data_dir()))
          dir.create(hydrogeofetch_data_dir(), recursive = TRUE)

        saveRDS(params, r)
        params
      }
    }, error = function(e) {
      NULL
    })

    if(is.null(out)) warning("Problem getting StreamCat metadata. Is the API available?")

    if(!missing(search) && !is.null(out)) {
      return(out[unique(unlist(
        mapply(grep, search, out, ignore.case = TRUE)
      )), ])
    }
    return(out)
  }

  # USGS ScienceBase metadata (default)
  out <- tryCatch({
    u <- "https://prod-is-usgs-sb-prod-publish.s3.amazonaws.com/5669a79ee4b08895842a1d47/metadata_table.tsv"

    f <- file.path(hydrogeofetch_data_dir(), "metadata_table.tsv")
    r <- file.path(hydrogeofetch_data_dir(), "metadata_table.rds")

    if(!cache) {
      unlink(r, force = TRUE)
      unlink(f, force = TRUE)
    }

    if(file.exists(r)) {
      out <- readRDS(r)
    } else {

      if(!dir.exists(dirname(f))) dir.create(dirname(f), recursive = TRUE)

      if(!file.exists(f)) hgf_download(u, f, progress = FALSE)

      out <- read.delim(f, sep = "\t")

      saveRDS(out, r)

      unlink(f)

      out
    }
  }, error = function(e) {
    NULL
  })

  if(is.null(out)) warning("Problem getting metadata, no internet?")

  if(!missing(search)) {
    return(out[unique(unlist(
      mapply(grep, search, out, ignore.case = TRUE)
    )), ])
  }
  out
}

#' Get Catchment Characteristics
#' @description Downloads (subsets of) catchment characteristics from a cloud data
#' store. See \link{get_characteristics_metadata} for available characteristics.
#'
#' When \code{source = "usgs"} (the default), data is retrieved from:
#' Wieczorek, M.E., Jackson, S.E., and Schwarz, G.E., 2018, Select Attributes
#' for NHDPlus Version 2.1 Reach Catchments and Modified Network Routed Upstream
#' Watersheds for the Conterminous United States (ver. 3.0, January 2021): U.S.
#' Geological Survey data release, \doi{10.5066/F7765D7V}.
#'
#' When \code{source = "streamcat"}, data is retrieved from the EPA StreamCat
#' dataset via the \code{StreamCatTools} package (must be installed separately).
#' The \code{aoi} parameter controls the area of interest for StreamCat queries.
#'
#' @param varname character vector of desired variables. If repeated varnames
#' are provided, they will be downloaded once but duplicated in the output.
#' For \code{source = "streamcat"}, use StreamCat metric names (see
#' \code{get_characteristics_metadata(source = "streamcat")}).
#' @param ids numeric vector of identifiers (comids) from the specified fabric
#' @param reference_fabric (not used) will be used to allow future specification
#' of alternate reference fabrics
#' @param source character \code{"usgs"} (default) or \code{"streamcat"}.
#' @param aoi character area of interest for StreamCat queries. One of
#' \code{"cat"} (local catchment, default), \code{"ws"} (total upstream
#' watershed), \code{"catrp100"} (catchment riparian 100m buffer),
#' \code{"wsrp100"} (watershed riparian 100m buffer), or \code{"other"}
#' (for metrics like BankfullDepth, IWI, etc.). Ignored when
#' \code{source = "usgs"} where the area of interest is encoded in the
#' variable name prefix (e.g. CAT_, TOT_, ACC_).
#' @return data.frame with columns characteristic_id, comid,
#' characteristic_value, and percent_nodata. NULL if no requested variables
#' were found or the data store is unavailable.
#' @importFrom dplyr bind_rows filter select everything collect all_of
#' @importFrom arrow s3_bucket open_dataset
#' @export
#' @examples
#' \donttest{
#'   get_catchment_characteristics("CAT_BFI", c(5329343, 5329427))
#' }
get_catchment_characteristics <- function(varname, ids,
                                          reference_fabric = "nhdplusv2",
                                          source = "usgs",
                                          aoi = "cat"){

  source <- match.arg(source, c("usgs", "streamcat"))

  # dispatch to StreamCat API via StreamCatTools package
  if(source == "streamcat") {
    aoi <- match.arg(aoi, c("cat", "ws", "catrp100", "wsrp100", "other"))
    return(get_catchment_characteristics_streamcat(varname, ids, aoi))
  }

  # USGS ScienceBase S3 parquet data (default path)
  metadata <- get_characteristics_metadata()

  uvar <- unique(varname)

  # validate all variables upfront
  missing_vars <- setdiff(uvar, metadata$ID)
  if(length(missing_vars) > 0) {
    warning(paste("Variable(s)", paste(missing_vars, collapse = ", "),
                  "not found in metadata."))
    return(NULL)
  }

  # get metadata for requested variables (first match per ID)
  var_meta <- metadata[metadata$ID %in% uvar, ]
  var_meta <- var_meta[!duplicated(var_meta$ID), ]

  # Batch variables by shared s3_url to avoid redundant S3 round-trips.
  # Multiple variables often share the same parquet dataset (e.g. CAT_BFI,
  # ACC_BFI, TOT_BFI). Opening each dataset once and selecting all needed
  # columns in a single collect() call is much faster than per-variable access.
  # See: https://github.com/DOI-USGS/hydrogeofetch/issues/449
  url_groups <- split(var_meta$ID, var_meta$s3_url)

  # The arrow S3 metadata roundtrip emits a benign "discarded from R metadata"
  # (externalptr) warning; muffle only that one so other warnings still surface.
  out <- withCallingHandlers(tryCatch({
    unlist(lapply(names(url_groups), function(url) {
      vars_in_group <- url_groups[[url]]

      # open the shared S3 parquet dataset once for all variables in this group
      ds <- open_dataset(url)

      cols_to_select <- c("COMID", vars_in_group, "percent_nodata")

      sub <- filter(select(ds, any_of(cols_to_select)),
                    .data$COMID %in% ids)

      # as.data.frame strips the data.table class arrow may attach when its
      # R metadata roundtrip fails (e.g. externalptr columns) so downstream
      # `att[, cols, drop = FALSE]` keeps the data.frame semantics it expects.
      att <- as.data.frame(collect(sub))

      att <- dplyr::mutate_all(att, \(x) ifelse(x == -9999, NA, x))

      has_pct_nodata <- "percent_nodata" %in% names(att)

      # pivot each variable into the standard 4-column long format
      lapply(vars_in_group, function(x) {
        cols <- c("COMID", x)
        if(has_pct_nodata) cols <- c(cols, "percent_nodata")
        out_df <- att[, cols, drop = FALSE]

        out_df$characteristic_id <- x

        if(!has_pct_nodata) {
          out_df$percent_nodata <- 0
        }

        out_df <- mutate(out_df, percent_nodata =
                           ifelse(is.na(.data[[x]]), 100, .data$percent_nodata))

        distinct(
          select(out_df, all_of(c(characteristic_id = "characteristic_id",
                                  comid = "COMID",
                                  characteristic_value = x,
                                  percent_nodata = "percent_nodata"))))
      })
    }), recursive = FALSE)
  }, error = function(e) {
    e
  }), warning = function(w) {
    if(grepl("discarded from R metadata", conditionMessage(w)))
      invokeRestart("muffleWarning")
  })

  df <- sapply(out, is.data.frame)

  if(!all(df)) {
    warning(paste0("Issue getting characteristics. Error was: \n",
                   out[!df]))
    return(NULL)
  }

  # name list elements by their characteristic_id
  names(out) <- sapply(out, function(x) x$characteristic_id[1])

  varname <- stats::setNames(make.unique(varname, sep = "||"), varname)

  out <- lapply(1:length(varname), function(i) {
    out <- out[names(varname)[i]]

    out[[1]]$characteristic_id <- varname[[i]]

    out[[1]]
  })

  bind_rows(out)
}

# Retrieve catchment characteristics from EPA StreamCat API.
# Returns the same 4-column format as the USGS path.
#' @noRd
get_catchment_characteristics_streamcat <- function(varname, ids, aoi = "cat") {

  check_pkg("StreamCatTools")

  out <- tryCatch({
    metrics <- paste(unique(varname), collapse = ",")
    comids <- paste(ids, collapse = ",")

    # TODO: fix up showPctFull when this is fixed
    # https://github.com/USEPA/StreamCatTools/issues/88
    # sc_get_data returns a wide data.frame with metric columns (e.g. fertcat,
    # superfunddensws) and area columns
    result <- StreamCatTools::sc_get_data(
      metric = metrics,
      aoi = aoi,
      comid = comids, 
      showPctFull = NULL
    )

    if(is.null(result) || nrow(result) == 0) {
      warning("No data returned from StreamCat API.")
      return(NULL)
    }

    # separate metric value columns from area and completeness columns
    area_cols <- grep("AreaSqKm$", names(result), value = TRUE, ignore.case = TRUE)
    pctfull_cols <- grep("pctfull$", names(result), value = TRUE, ignore.case = TRUE)
    id_col <- intersect(c("COMID", "comid"), names(result))

    value_cols <- setdiff(names(result), c(id_col, area_cols, pctfull_cols))

    comid_vals <- result[[id_col[1]]]

    # pivot each metric column into the standard long format
    rows <- lapply(value_cols, function(col) {
      # StreamCat PctFull = percent with data; convert to percent_nodata
      # PctFull column names don't follow a predictable pattern relative
      # to metric names (e.g. fertcat -> agriculturalnitrogen_catpctfull),
      # so we use NA-based fallback for percent_nodata
      pct_nodata <- ifelse(is.na(result[[col]]), 100, 0)

      data.frame(
        characteristic_id = col,
        comid = comid_vals,
        characteristic_value = result[[col]],
        percent_nodata = pct_nodata,
        stringsAsFactors = FALSE
      )
    })

    bind_rows(rows)
  }, error = function(e) {
    warning(paste0("Issue getting StreamCat characteristics. Error was: \n",
                   e$message))
    NULL
  })

  out
}
