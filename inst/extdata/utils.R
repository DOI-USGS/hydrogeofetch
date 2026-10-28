download_pkg_data <- function(f, u, work_dir) {
  dir.create(work_dir, showWarnings = FALSE, recursive = TRUE)

  d <- file.path(work_dir, f)

  check_cache <- file.path(hydrogeofetch::hydrogeofetch_data_dir(), f)

  if(file.exists(check_cache)) {

    project_file <- check_cache

  } else {

    project_file <- c(paste0("../../docs/data/", f),
                      paste0("docs/data/", f))

    project_file <- project_file[file.exists(project_file)][1]

  }

  if(length(project_file) > 0 &&
     file.exists(project_file)) {

    if(!dir.exists(dirname(d))) {
      dir.create(dirname(d), showWarnings = TRUE, recursive = TRUE)
    }


    file.copy(project_file, d, overwrite = TRUE)
  } else {
    url <- u
    resp <- tryCatch(httr2::request(url) |>
                       httr2::req_retry(max_tries = 3) |>
                       httr2::req_perform(path = d),
                     error = function(e) {
                       message("could not download ", url, ": ", conditionMessage(e))
                       NULL
                     })

    # a failed request can leave the error body or a partial file at d
    if(is.null(resp)) {
      unlink(d)
      return(invisible(NULL))
    }
  }

  if(grepl(".zip$", d)) {
    zip::unzip(d, exdir = work_dir)
    unlink(d)
  }

  return(invisible(work_dir))

}
