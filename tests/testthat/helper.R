
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
    hgf_download("https://doi-usgs.github.io/nhdplusTools/data/03_sub.zip",
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
# is newer than it -- so a dev who edits or re-records fixtures never has to
# remember to repack before committing.
fixtures_root <- local({
  if(dir.exists("fixtures")) {
    files <- list.files("fixtures", recursive = TRUE, full.names = TRUE,
                        all.files = TRUE, no.. = TRUE)
    tarball <- "fixtures.tar.gz"
    stale <- length(files) > 0 && (!file.exists(tarball) ||
      file.info(tarball)$mtime < max(file.info(files)$mtime))
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

with_mock_hgf <- function(fixture, expr,
    live = identical(Sys.getenv("HYDROGEOFETCH_LIVE"), "true")) {
  if(live) {
    expr
  } else {
    httptest2::with_mock_dir(
      file.path(fixtures_root, "fixtures", fixture), expr)
  }
}
