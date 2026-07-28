#####################################################################
# File contains general downloading tools and API utility functions #
#####################################################################

#' Download NHDPlus HiRes
#' @param nhd_dir character directory to save output into
#' @param hu_list character vector of hydrologic region(s) to download.
#' Use \link{get_huc} to find HU codes of interest. Accepts two digit
#' and four digit codes.
#' @param download_files boolean if FALSE, only URLs to files will be returned
#' can be hu02s and/or hu04s
#' @param archive pull data from the "archive" folder rather than "current".
#' The archive contains the original releases of NHDPlusHR data that were updated
#' in subsequent processing. Not all subsets of NHDPlusHR were updated. See:
#' https://www.usgs.gov/national-hydrography/access-national-hydrography-products
#' for more details.
#'
#' @return character Paths to geodatabases created.
#' @export
#' @examples
#' \donttest{
#' hu <- get_huc(sf::st_sfc(sf::st_point(c(-73, 42)), crs = 4326),
#'                             type = "huc08")
#' if(inherits(hu, "sf")) {
#' (hu <- substr(hu$huc8, 1, 2))
#'
#' download_nhdplushr(tempdir(), c(hu, "0203"), download_files = FALSE)
#'
#' download_nhdplushr(tempdir(), c(hu, "0203"), download_files = FALSE, archive = TRUE)
#' }
#' }
download_nhdplushr <- function(nhd_dir, hu_list, download_files = TRUE, archive = FALSE) {

  list_source <- get("nhdhr_file_list", envir = hydrogeofetch_env)

  if(archive) list_source <- get("archive_nhdhr_file_list", envir = hydrogeofetch_env)

  download_nhd_internal(get("nhd_bucket", envir = hydrogeofetch_env),
               list_source,
               "NHDPLUS_H_", nhd_dir, hu_list, download_files)
}

#' Download NHD
#' @inheritParams download_nhdplushr
#'
#' @return character Paths to geodatabases created.
#' @export
#' @examples
#' \donttest{
#' hu <- get_huc(sf::st_sfc(sf::st_point(c(-73, 42)), crs = 4326),
#'                             type = "huc08")
#'
#' (hu <- substr(hu$huc8, 1, 2))
#'
#' download_nhd(tempdir(), c(hu, "0203"), download_files = FALSE)
#' }
download_nhd <- function(nhd_dir, hu_list, download_files = TRUE) {

  download_nhd_internal(get("nhd_bucket", envir = hydrogeofetch_env),
               get("nhd_file_list", envir = hydrogeofetch_env),
               "NHD_H_",
               nhd_dir, hu_list, download_files)
}

#' @importFrom xml2 read_xml xml_ns_strip xml_find_all xml_text
#' @importFrom utils download.file
#' @importFrom zip unzip
download_nhd_internal <- function(bucket, file_list_snip, prefix, nhd_dir, hu_list, download_files = TRUE) {
  hu02_list <- unique(substr(hu_list, 1, 2))
  hu04_list <- hu_list[which(nchar(hu_list) == 4)]
  subset_hu02 <- sapply(hu02_list, function(x)
    sapply(x, function(y) any(grepl(y, hu04_list))))

  out <- c()

  for(h in 1:length(hu02_list)) {
    hu02 <- hu02_list[h]

    if(download_files) {
      out <- c(out, file.path(nhd_dir, hu02))
    }

    if(download_files) {
      dir.create(out[length(out)], recursive = TRUE, showWarnings = FALSE)
    }

    file_list <- tryCatch({
      read_xml(paste0(bucket, file_list_snip,
                      prefix, hu02)) |>
        xml_ns_strip() |>
        xml_find_all(xpath = "//Key") |>
        xml_text()
    }, error= function(e) {
      NULL
    })

    if(is.null(file_list)) {
      warning("Something went wrong retrieving the nhdhr file list.")
      return(NULL)
    }

    file_list <- file_list[grepl("_GDB.zip", file_list)]

    if(subset_hu02[h]) {
      file_list <- file_list[sapply(file_list, function(f)
        any(sapply(hu04_list, grepl, x = f)))]
    }

    for(key in file_list) {
      dir_out <- ifelse(is.null(out[length(out)]), "", out[length(out)])
      out_file <- file.path(dir_out, basename(key))
      url <- paste0(bucket, key)

      hu04 <- regexec("[0-9][0-9][0-9][0-9]", out_file)[[1]]
      hu04 <- substr(out_file, hu04, hu04 + 3)

      gdb_in_dir <- list.files(dirname(out_file), full.names = TRUE)
      gdb_in_dir <- gdb_in_dir[grepl(paste0(".*", hu04, "_.*\\.gdb"), gdb_in_dir, ignore.case = TRUE)]

      if(download_files & !dir.exists(gsub(".zip", ".gdb", out_file)) &
         !(length(gdb_in_dir) > 0 && !dir.exists(gdb_in_dir))) {

        if(file.exists(out_file)) {
          unlink(out_file)
        }

        if(is.null(hgf_download(url, out_file))) return(NULL)

        tryCatch({zip::unzip(out_file, exdir = out[length(out)])},
                 error = function(e) {
                   warning("error unzipping with zip::unzip \n",
                           out_file, "\n", e, "\ntrying a different way", immediate. = TRUE)
                   files <- try(utils::unzip(out_file, exdir = out[length(out)]))
                   if(!inherits(files, "try-error")) {
                     warning("Success with utils::unzip", immediate. = TRUE)
                   } else {
                     warning("unzip of\n", out_file,
                             "\nfailed with utils and zip packages.\n",
                             "Try manually unzipping?", immediate. = TRUE)}
                 })

        unlink(out_file)
      } else if(!download_files) {
        out <- c(out, url)
      }
    }
  }
  return(out)
}

#' @title Download seamless National Hydrography Dataset Version 2 (NHDPlusV2)
#' @description This function downloads and decompresses staged seamless NHDPlusV2 data.
#' The following requirements are needed: p7zip (MacOS), 7zip (windows) Please see:
#' https://www.epa.gov/waterdata/get-nhdplus-national-hydrography-dataset-plus-data
#' for more information and metadata about this data.
#'
#' Default downloads lower-48 only. See examples for islands. No Alaska data
#' are available.
#'
#' @param outdir The folder path where data should be downloaded and extracted
#' @param url the location of the online resource
#' @param progress boolean display download progress?
#' @return character path to the local geodatabase
#' @export
#' @examples
#' \dontrun{
#'   download_nhdplusv2("./data/nhd/")
#'
#'   download_nhdplusv2(outdir = "./inst/",
#'       url = paste0("https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/",
#'                    "Data/NationalData/NHDPlusV21_NationalData_Seamless",
#'                    "_Geodatabase_HI_PR_VI_PI_03.7z"))
#' }

download_nhdplusv2 <- function(outdir,
                               url = paste0("https://dmap-data-commons-ow.s3.amazonaws.com/NHDPlusV21/",
                                            "Data/NationalData/NHDPlusV21_NationalData_Seamless",
                                            "_Geodatabase_Lower48_07.7z"),
                               progress = TRUE) {

  tryCatch({
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  file <- file.path(outdir, basename(url))
  if(!file.exists(file)) {
    message("Downloading ", basename(url))
    if(is.null(hgf_download(url, file, progress))) return(NULL)
  }

  if(!any(grepl("gdb", list.dirs(outdir)))) {

    try_7z <- try(check7z())
    if(inherits(try_7z, "try-error")) {
      message("couldn't find 7zip, won't try to extract data")
      message("check for data in: ", outdir)
      return(outdir)
    }

    message("Extracting data ...")

    system(paste0(shQuote(try_7z), " -o", path.expand(outdir), " x ", file),
           intern = TRUE)

  }

  path <- list.dirs(outdir)[grepl("gdb", list.dirs(outdir))]
  path <- path[grepl("NHDPlus", path)]

  message(paste("NHDPlusV2 data available at:", path))

  return(invisible(path))
  }, error = function(e) {
    warning("Something went wrong downloading nhd data.")
    return(NULL)
  })
}

#' @title Download the seamless Watershed Boundary Dataset (WBD)
#' @description This function downloads and decompresses staged seamless WBD data.
#' Please see:
#' https://prd-tnm.s3.amazonaws.com/StagedProducts/Hydrography/WBD/National/GDB/WBD_National_GDB.xml
#' for metadata.
#' @inheritParams download_nhdplusv2
#' @return character path to the local geodatabase
#' @export
#' @importFrom zip unzip
#' @examples
#' \dontrun{
#'   download_wbd("./data/wbd/")
#' }

download_wbd <- function(outdir,
                         url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/",
                                      "Hydrography/WBD/National/GDB/WBD_National_GDB.zip"),
                         progress = TRUE) {

  tryCatch({
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  file <- file.path(outdir, basename(url))
  if(!file.exists(file)) {
    message("Downloading ", basename(url))
    if(is.null(hgf_download(url, file, progress))) return(NULL)
  }

  message("Extracting data ...")

  try(suppressWarnings(zip::unzip(file, exdir = outdir, overwrite = FALSE)))

  path <- list.dirs(outdir)[grepl("gdb", list.dirs(outdir))]
  path <- path[grepl("WBD", path)]

  message(paste("WBD data extracted to:", path))

  return(invisible(path))
  }, error = function(e) {
    warning("Something went wrong trying to download WBD data.")
    return(NULL)
  })
}

#' @noRd
#' @description gunzip a file in place. Leaves the .gz file intact. Skips if
#' the destination already exists. Replaces R.utils::gunzip(remove = FALSE,
#' skip = TRUE) so we don't carry R.utils as a dependency for one call.
gunzip_keep <- function(file) {
  out <- sub("\\.gz$", "", file)
  if(file.exists(out)) return(invisible(out))
  con_in <- gzfile(file, "rb")
  on.exit(close(con_in), add = TRUE)
  con_out <- file(out, "wb")
  on.exit(close(con_out), add = TRUE)
  repeat {
    chunk <- readBin(con_in, what = "raw", n = 1e6)
    if(length(chunk) == 0) break
    writeBin(chunk, con_out)
  }
  invisible(out)
}

#' @title Download the seamless Reach File (RF1) Database
#' @description This function downloads and decompresses staged RF1 data.
#' See: https://water.usgs.gov/GIS/metadata/usgswrd/XML/erf1_2.xml for metadata.
#' @inheritParams download_nhdplusv2
#' @return character path to the local e00 file
#' @export
#' @examples
#' \dontrun{
#'   download_rf1("./data/rf1/")
#' }

download_rf1 <- function(outdir,
                         url = "https://water.usgs.gov/GIS/dsdl/erf1_2.e00.gz",
                         progress = TRUE){
  tryCatch({
  if(!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)
  file <- file.path(outdir, basename(url))
  if(!file.exists(file)) {
    message("Downloading ", basename(url))
    if(is.null(hgf_download(url, file, progress))) return(NULL)
  }

  message("Extracting data ...")

  gunzip_keep(file)

  path     <- list.files(outdir, full.names = TRUE)[!grepl("gz", list.files(outdir))]
  path     <- path[grepl("rf1", path)]

  message(paste("RF1 data extracted to:", path))

  return(invisible(path))
  }, error = function(e) {
    warning("Something went wrong trying to download RF1 data.")
    return(NULL)
  })

}

#' @title Utility to see in 7z is local
#' @description Checks if 7z is on system. If not, provides an informative error
#' @return character path to the 7z executable to call
#' @noRd
check7z <- function() {

  tryCatch({
    system("7z", intern = TRUE)
    "7z"
  }, error = function(e) {
    # 7z not on PATH; check default Windows install location
    win_path <- "C:/Program Files/7-Zip/7z.exe"
    if(.Platform$OS.type == "windows" && file.exists(win_path)) {
      return(win_path)
    }
    stop(simpleError(
      "Please Install 7zip (Windows) or p7zip (MacOS/Unix). Choose accordingly:
        Windows: https://www.7-zip.org/download.html
        Mac: 'brew install p7zip' or 'sudo port install p7zip'
        Linux: https://sourceforge.net/projects/p7zip/"
    ))
  })

}

#################################################################
# httr2 helpers — all HTTP in the package flows through these  #
#################################################################

#' @description builds a request with a bounded lifetime so a hung service
#' cannot stall a call indefinitely. `timeout` caps the whole request and is
#' used for the API calls; bulk downloads pass NULL so a slow-but-progressing
#' transfer is not cut off, and rely on the connect timeout alone.
#' @importFrom httr2 request req_perform
#' @noRd
build_hgf_req <- function(url, body = NULL, content_type = NULL, encode = NULL,
                          timeout = 300) {
  req <- httr2::request(url) |>
    httr2::req_user_agent(
      paste0("hydrogeofetch/", utils::packageVersion("hydrogeofetch"))
    ) |>
    httr2::req_retry(max_tries = 3)

  # req_timeout() covers connection setup too, and zeroes connecttimeout, so
  # the two are set as alternatives rather than together.
  req <- if(is.null(timeout)) {
    httr2::req_options(req, connecttimeout = 30)
  } else {
    httr2::req_timeout(req, timeout)
  }

  if(nhdplus_debug()) {
    message(if(is.null(body)) "GET " else "POST ", url)
    if(!is.null(body) && is.character(body)) message(body)
  }

  if(!is.null(body)) {
    if(identical(encode, "form")) {
      req <- do.call(httr2::req_body_form, c(list(req), body))
    } else {
      ct <- content_type %||% "application/octet-stream"
      req <- httr2::req_body_raw(req, charToRaw(body), type = ct)
    }
  }

  req
}

#' @noRd
hgf_json <- function(url, body = NULL, content_type = NULL, encode = NULL,
                      simplifyVector = TRUE, ...) {
  tryCatch({
    resp <- httr2::req_perform(build_hgf_req(url, body, content_type, encode))
    httr2::resp_body_json(resp, simplifyVector = simplifyVector, ...)
  }, error = function(e) {
    warning("Failed to get JSON from ", url, ": ", conditionMessage(e),
            call. = FALSE)
    NULL
  })
}

#' @noRd
hgf_sf <- function(url, body = NULL, content_type = NULL, encode = NULL) {
  tryCatch({
    resp <- httr2::req_perform(build_hgf_req(url, body, content_type, encode))
    sf::st_zm(sf::read_sf(httr2::resp_body_string(resp)))
  }, error = function(e) {
    warning("Failed to get features from ", url, ": ", conditionMessage(e),
            call. = FALSE)
    NULL
  })
}

#' @noRd
hgf_download <- function(url, path, progress = TRUE) {
  tryCatch({
    req <- build_hgf_req(url, timeout = NULL)
    if(progress && interactive()) req <- httr2::req_progress(req)
    httr2::req_perform(req, path = path)
    invisible(path)
  }, error = function(e) {
    warning("Failed to download from ", url, ": ", conditionMessage(e),
            call. = FALSE)
    NULL
  })
}

#' @noRd
mem_get_json <- memoise::memoise(\(url) hgf_json(url, simplifyVector = FALSE))

#' @importFrom sf st_make_valid st_as_sfc st_bbox st_buffer st_transform st_crs
check_query_params <- function(AOI, ids, type, where, source, t_srs, buffer) {
  # If t_src is not provided set to AOI CRS
  if(is.null(t_srs)){ t_srs  <- st_crs(AOI) }
  # If AOI CRS is NA (e.g st_crs(NULL)) then set to 4326
  if(is.na(t_srs))  { t_srs  <- 4326 }

  if(!is.null(AOI) & !is.null(ids)) {
    # Check if AOI and IDs are both given
    stop("Either IDs or a spatial AOI can be passed.", call. = FALSE)
  } else if(is.null(AOI) & is.null(ids) & !(!is.null(where) && grepl("IN", where))) {
    # Check if AOI and IDs are both NULL
    stop("IDs or a spatial AOI must be passed.", call. = FALSE)
  } else if(!(type %in% source$user_call)) {
    # Check that "type" is valid
    stop(paste("Type not available must be one of:",
               paste(source$user_call, collapse = ", ")),
         call. = FALSE)
  }

  if(!is.null(AOI)){

    if(length(st_geometry(AOI)) > 1) {
      stop("AOI must be one an only one feature.")
    }

    if(st_geometry_type(AOI) == "POINT"){
      # If input is a POINT, buffer by 1/2 meter (in equal area projection)
      AOI = st_transform(AOI, 5070) |>
        st_buffer(buffer) |>
        st_bbox() |>
        st_as_sfc() |>
        st_make_valid() |>
        st_transform(st_crs(AOI))
    }
  }

  return(list(AOI = AOI, t_srs = t_srs))
}
