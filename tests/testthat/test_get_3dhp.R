test_that("get_3dhp", {

  expect_error(get_3dhp(universalreferenceid = "01234", type = "test"),
               "only be specified for hydrolocation")

  expect_error(get_3dhp(universalreferenceid = "01234", type = "hydrolocation",
                        ids = "1"),
               "can not specify both")

  expect_error(get_3dhp(ids = "workunitid:NHD", type = "flowline"),
               "not a useful filter")

  with_mock_hgf("get_3dhp", {
    expect_type(get_3dhp(), "list")

    ms <- get_3dhp(ids = "https://geoconnex.us/ref/mainstems/377002",
                   type = "flowline")

    expect_equal(unique(ms$mainstemid), "https://geoconnex.us/ref/mainstems/377002")

    expect_s3_class(ms, "sf")

    suppressWarnings({
      hl <- get_3dhp(ids = "https://geoconnex.us/ref/mainstems/377002",
                     type = "hydrolocation - reach code, external connection")
    })
    expect_equal(unique(hl$mainstemid), "https://geoconnex.us/ref/mainstems/377002")
  })

  skip_if_no_integration()

  wufl <- get_3dhp(ids = "workunitid:300585", type = "flowline")

  expect_s3_class(wufl, "sf")
  expect_true(all(wufl$workunitid == "300585"))
})

test_that("a failed page does not break a multi page request", {

  # a page that 504s comes back NULL and used to reach bind_rows as-is,
  # erroring with "Argument 3 must be a data frame or a named atomic vector."
  page <- sf::st_sf(id3dhp = "a", workunitid = "300585",
                    geometry = sf::st_sfc(sf::st_point(c(-89.4, 43.1)),
                                          crs = 4326))

  calls <- 0

  local_mocked_bindings(
    hgf_json = function(...) list(objectIds = list(1, 2, 3)),
    hgf_sf = function(...) {
      calls <<- calls + 1
      if(calls == 2) return(NULL)
      page$id3dhp <- as.character(calls)
      page
    },
    .package = "hydrogeofetch")

  suppressMessages(
    expect_warning(out <- get_3dhp(ids = "workunitid:300585",
                                   type = "flowline", page_size = 1),
                   "1 of 3 feature requests failed"))

  expect_s3_class(out, "sf")
  expect_equal(nrow(out), 2)
})
