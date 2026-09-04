
library("sf")
library("dplyr")

hydrogeofetch_cache_settings(mode = "memory", timeout = 1)

sf::sf_use_s2(TRUE)

unlink(file.path(tempdir(check = TRUE), "*"), recursive = TRUE)

get_test_file <- function(temp_dir) {
  check_locations <- c("../../docs/data/03_sub.zip", "docs/data/03_sub.zip")
  check_location <- check_locations[file.exists(check_locations)]
  if(length(check_location) == 0) {
    temp_file <- file.path(temp_dir, "temp.zip")
    hgf_download("https://doi-usgs.github.io/hydrogeofetch/data/03_sub.zip",
                  temp_file)
  } else {
    temp_file <- check_location[1]
  }
  unzip(temp_file, exdir = temp_dir)
}


get_test_dir <- function() {
  f <- list.files(pattern = "data$", recursive = TRUE, full.names = TRUE, include.dirs = TRUE)
  f[grep("testthat\\/data", f)]
}


check_layers <- function(out_file) {
  expect_equal(nrow(sf::read_sf(out_file, "CatchmentSP")), 4)
  expect_equal(nrow(sf::read_sf(out_file, "NHDWaterbody")), 1)
  expect_true(sf::st_crs(sf::read_sf(out_file, "CatchmentSP")) ==
                sf::st_crs(4269))
  expect_true(sf::st_crs(sf::read_sf(out_file, "NHDWaterbody")) ==
                sf::st_crs(4269))
  expect_true(sf::st_crs(sf::read_sf(out_file, "NHDFlowline_Network")) ==
                sf::st_crs(4269))
}

setup_workdir <- function() {
  work_dir <- file.path(tempdir(), "test_hr")
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)
  out_gpkg <- file.path(work_dir, "temp.gpkg")
  list(wd = work_dir, og = out_gpkg)
}

teardown_workdir <- function(work_dir) {
  unlink(work_dir, recursive = TRUE, force = TRUE)
}

skip_if_no_integration <- function() {
  if(!identical(Sys.getenv("HYDROGEOFETCH_INTEGRATION"), "true"))
    skip("Set HYDROGEOFETCH_INTEGRATION=true for integration tests")
}

# Fixtures live in a tarball (tests/testthat/fixtures.tar.gz) so individual
# paths stay under R CMD check's 100-char portable-path limit. The loose
# tests/testthat/fixtures/ tree is the source of truth (committed, .Rbuildignored);
# the tarball is the shipping artifact. When the loose tree is present (dev,
# including re-record), the helper auto-regenerates the tarball if any fixture
# is newer than it, or if the two hold different files -- so a dev who edits,
# re-records, or renames fixtures never has to remember to repack before
# committing. The name check matters because file.rename carries the old mtime
# over, which would leave a rename invisible to the mtime check.
fixtures_root <- local({
  if(dir.exists("fixtures")) {
    files <- list.files("fixtures", recursive = TRUE, full.names = TRUE,
                        all.files = TRUE, no.. = TRUE)
    tarball <- "fixtures.tar.gz"
    packed <- if(file.exists(tarball)) {
      grep("/$", utils::untar(tarball, list = TRUE), value = TRUE,
           invert = TRUE)
    } else character(0)
    stale <- length(files) > 0 && (!file.exists(tarball) ||
      file.info(tarball)$mtime < max(file.info(files)$mtime) ||
      !setequal(files, packed))
    if(stale) {
      message("Repacking ", tarball, " (loose fixtures/ tree is newer)")
      utils::tar(tarball, files = "fixtures", compression = "gzip",
                 tar = "internal")
    }
    return(".")
  }
  td <- file.path(tempdir(), "hgf_fixtures")
  if(!dir.exists(file.path(td, "fixtures"))) {
    archive <- if(file.exists("fixtures.tar.gz")) {
      "fixtures.tar.gz"
    } else if(file.exists("tests/testthat/fixtures.tar.gz")) {
      "tests/testthat/fixtures.tar.gz"
    } else {
      stop("Cannot locate fixtures.tar.gz for httptest2 mocks", call. = FALSE)
    }
    dir.create(td, showWarnings = FALSE, recursive = TRUE)
    utils::untar(archive, exdir = td)
  }
  td
})

# httptest2 names each fixture by hashing the request's query string and body,
# so every coordinate in a request is part of that fixture's identity. Those
# coordinates come out of projection round trips, and their last digits differ
# between PROJ builds -- a couple of centimeters was enough to send CRAN's
# Fedora checks looking for fixtures that do not exist. Coarsening coordinates
# to three decimals (about 100 m) before the hash puts four orders of magnitude
# between that platform noise and the nearest name change. Only the copy
# httptest2 hashes is coarsened; the request that goes over the wire keeps full
# precision, and recording applies the same transform, so record and replay
# agree on the name.
coarsen_coords <- function(txt, digits = 3) {
  if(is.null(txt) || !nzchar(txt)) return(txt)
  m <- gregexpr("-?[0-9]+[.][0-9]{4,}", txt)
  regmatches(txt, m) <- lapply(regmatches(txt, m), function(v)
    formatC(round(as.numeric(v), digits), format = "f", digits = digits))
  txt
}

httptest2::set_redactor(function(x) {
  x$url <- coarsen_coords(x$url)
  if(is.raw(x$body$data))
    x$body$data <- charToRaw(coarsen_coords(rawToChar(x$body$data)))
  x
})

with_mock_hgf <- function(fixture, expr,
    live = identical(Sys.getenv("HYDROGEOFETCH_LIVE"), "true")) {
  if(live) return(expr)
  # httptest2 aborts an unmatched request, hgf_sf() catches that and warns, and
  # the caller turns it into "No features found" -- indistinguishable from a
  # legitimately empty response. Promote the warning back to an error so a
  # stale fixture name fails where it happens instead of somewhere downstream.
  # A call wrapped in capture_warnings()/expect_warning() muffles the warning
  # before this handler sees it, so those sites stay on their own assertions.
  withCallingHandlers(
    httptest2::with_mock_dir(file.path(fixtures_root, "fixtures", fixture),
                             expr),
    warning = function(w) {
      if(grepl("unexpected request was made", conditionMessage(w)))
        stop("Missing fixture in '", fixture, "': ", conditionMessage(w),
             call. = FALSE)
    })
}
