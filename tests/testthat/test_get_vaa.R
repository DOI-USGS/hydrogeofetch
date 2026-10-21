

test_that("vaa examples", {
  skip_if_no_integration()
  skip_on_ci()

  vaa_names <- get_vaa_names()

  expect_type(vaa_names, "character")

  vaa_path <- get_vaa_path()

  expect_type(vaa_path, "character")

  vaa <- get_vaa()

  expect_s3_class(vaa, "data.frame")

  expect_message(empty <- get_vaa(atts = "baddies"), "baddies not in vaa data. Ignoring...")

  expect_equal(nrow(empty), 2691339)

  expect_message(vaa_path_2 <- download_vaa(), "File already cached")

  expect_equal(vaa_path, vaa_path_2)

  expect_message(update <- get_vaa("reachcode", updated_network = TRUE))

  expect_equal(names(update), c("comid", "reachcode"))

  expect_message(get_vaa("bad", updated_network = TRUE),
                 "bad not in vaa data")

  expect_true("tocomid" %in% get_vaa_names(updated_network = TRUE))
})

test_that("catchment chars", {

  skip_if_no_integration()
  skip_on_os("linux")
  skip_on_os("mac")

  old_opts <- options(arrow.unsafe_metadata = TRUE)

  httptest2::without_internet({
    suppressMessages(expect_warning(w <- get_characteristics_metadata(cache = FALSE)))
    expect_null(w)
  })

  meta <- hydrogeofetch::get_characteristics_metadata(cache = FALSE)

  expect_true(inherits(meta, "data.frame"))
  expect_true(nrow(meta) > 1000)

  meta <- hydrogeofetch::get_characteristics_metadata()

  expect_true(inherits(meta, "data.frame"))
  expect_true(nrow(meta) > 1000)

  meta <- hydrogeofetch::get_characteristics_metadata("BFI")
  expect_equal(nrow(meta), 3)

  expect_equal(names(meta), c("ID", "description", "units", "datasetLabel", "datasetURL",
                              "themeLabel", "watershedType",
                              "sbid", "end", "s3_url", "http_url", "source"))

  source(system.file("extdata", "walker_data.R", package = "hydrogeofetch"))

  suppressWarnings(
    dat <- get_catchment_characteristics(c("CAT_BFI", "ACC_BFI", "TOT_BFI"), walker_catchment$FEATUREID)
  )

  expect_equal(names(dat), c("characteristic_id", "comid",
                             "characteristic_value", "percent_nodata"))

  expect_true(all(c("CAT_BFI", "ACC_BFI", "TOT_BFI") %in% dat$characteristic_id))

  expect_true(all(walker_catchment$FEATUREID %in% dat$comid))

  expect_warning(w <- get_catchment_characteristics("CAT_CFI",
                                                    walker_catchment$FEATUREID),
                 "CAT_CFI not found in metadata")
  expect_null(w)

  options(old_opts)
})

test_that("characteristics metadata falls back to ScienceBase when S3 fails (#487)", {
  d <- tempfile()

  local_mocked_bindings(
    hydrogeofetch_data_dir = function() d,
    hgf_download = function(url, path, ...) {
      if(grepl("amazonaws", url)) return(NULL)
      writeLines(c("ID", "CAT_BFI"), path)
      path
    }
  )

  expect_equal(get_characteristics_metadata(cache = FALSE)$ID, "CAT_BFI")
})

test_that("streamcat catchment chars", {

  skip_if_no_integration()
  skip_if_not_installed("StreamCatTools")

  source(system.file("extdata", "walker_data.R", package = "hydrogeofetch"))
  test_comids <- walker_catchment$FEATUREID[1:2]

  meta <- suppressWarnings(
    get_characteristics_metadata(source = "streamcat")
  )
  if(is.null(meta)) skip("StreamCat API unavailable")
  expect_true(inherits(meta, "data.frame"))
  expect_true(nrow(meta) > 100)

  meta_search <- suppressWarnings(
    get_characteristics_metadata("fert", source = "streamcat")
  )
  if(is.null(meta_search)) skip("StreamCat API unavailable")
  expect_true(nrow(meta_search) > 0)

  expected_names <- c("characteristic_id", "comid",
                      "characteristic_value", "percent_nodata")

  check_streamcat_result <- function(dat, ids) {
    if(is.null(dat)) skip("StreamCat API unavailable (rate limited)")
    expect_equal(names(dat), expected_names)
    expect_true(nrow(dat) > 0)
    expect_true(all(ids %in% dat$comid))
  }

  # test default (cat) AOI
  dat <- suppressWarnings(get_catchment_characteristics(
    varname = "fert",
    ids = test_comids,
    source = "streamcat"))

  check_streamcat_result(dat, test_comids)

  # test watershed AOI
  dat_ws <- suppressWarnings(get_catchment_characteristics(
    varname = "fert",
    ids = test_comids,
    source = "streamcat",
    aoi = "ws"))

  check_streamcat_result(dat_ws, test_comids)

  # test catchment riparian 100m buffer AOI (rddens has riparian data)
  dat_catrp <- suppressWarnings(get_catchment_characteristics(
    varname = "superfunddens",
    ids = test_comids,
    source = "streamcat",
    aoi = "catrp100"))

  check_streamcat_result(dat_catrp, test_comids)

  # test watershed riparian 100m buffer AOI
  dat_wsrp <- suppressWarnings(get_catchment_characteristics(
    varname = "rddens",
    ids = test_comids,
    source = "streamcat",
    aoi = "wsrp100"))

  check_streamcat_result(dat_wsrp, test_comids)

  # test "other" AOI (metrics not tied to cat/ws like IWI)
  dat_other <- suppressWarnings(get_catchment_characteristics(
    varname = "iwi",
    ids = test_comids,
    source = "streamcat",
    aoi = "other"))

  check_streamcat_result(dat_other, test_comids)
})
