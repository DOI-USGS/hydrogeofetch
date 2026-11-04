AOI_esri <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
                                   xmax = -89.24681, ymax = 43.17192),
                                 crs = "+proj=longlat +datum=WGS84 +no_defs"))

test_that("spatial_filter", {

  expect_equal(as.character(hydrogeofetch:::spatial_filter_esri(AOI_esri)[[1]]),
               '{"xmin":-89.5668,"ymin":42.9982,"xmax":-89.2468,"ymax":43.1719,"spatialReference":{"wkid":4326}}')
})

test_that("check_query_params", {

  ids <- NULL
  type <- "test"
  source <- data.frame(user_call = c("test", "test2"))
  t_srs <- NULL
  buffer <- 0.5
  where <- NULL

  expect_equal(
    hydrogeofetch:::check_query_params(AOI_esri, ids, type, where, source, t_srs, buffer),
    list(AOI = AOI_esri, t_srs = sf::st_crs(AOI_esri)))

  expect_error(hydrogeofetch:::check_query_params(AOI_esri, c(1,2), type, where, source, t_srs, buffer),
               "Either IDs or")

  expect_error(hydrogeofetch:::check_query_params(NULL, NULL, type, where, source, t_srs, buffer),
               "IDs or a spatial AOI must be passed.")

  expect_error(hydrogeofetch:::check_query_params(AOI_esri, ids, "test3", where, source, t_srs, buffer),
               "test, test2")

  expect_error(hydrogeofetch:::check_query_params(c(AOI_esri, AOI_esri), ids, type, where, source, t_srs, buffer),
               "AOI must be one an only one feature.")

  AOI_new <- sf::st_sfc(sf::st_point(c(-89.56684, 42.99816)), crs = 4326)

  expect_equal(
    hydrogeofetch:::check_query_params(AOI_new, ids, type, where, source, t_srs, buffer)$AOI[[1]],
    structure(list(structure(c(-89.5668465687776, -89.5668457346386,
                               -89.5668334312216, -89.5668342653621,
                               -89.5668465687776, 42.9981558371707,
                               42.9981647683392, 42.9981641628291,
                               42.9981552316606, 42.9981558371707),
                             dim = c(5L, 2L))),
              class = c("XY", "POLYGON", "sfg")), tolerance = 0.1)

})

test_that("basic 3dhp service requests", {

  AOI_new <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.5, ymin = 43.0,
                                     xmax = -89.4, ymax = 43.1),
                                   crs = "+proj=longlat +datum=WGS84 +no_defs"))

  with_mock_hgf("arcrest_3dhp", {
    expect_message(expect_s3_class(hydrogeofetch:::query_usgs_arcrest(AOI_new, service = "3DHP_all"),
                                   "data.frame"))

    expect_warning(hydrogeofetch:::query_usgs_arcrest(AOI_new, service = "3DHP_all",
                                                     type = "hydrolocation"))

    test_data <- hydrogeofetch:::query_usgs_arcrest(AOI_new, , service = "3DHP_all",
                                                   type = "hydrolocation - external connection")

    expect_s3_class(test_data, "sf")
  })
})

test_that("query_usgs_arcrest degrades gracefully when the feature fetch fails", {

  AOI_new <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.5, ymin = 43.0,
                                     xmax = -89.4, ymax = 43.1),
                                   crs = "+proj=longlat +datum=WGS84 +no_defs"))

  # fully mocked at the internal-helper level (no fixtures/network) so a
  # fresh, never-memoised call signature isn't required
  testthat::local_mocked_bindings(
    get_arcrest_service_info = function(...) list(
      url_base = "https://example.com/MapServer/",
      all_layers = list(layers = list(list(id = 0, name = "Flowline",
                                           type = "Feature Layer"))),
      id = "id3dhp"),
    hgf_json = function(...) list(objectIds = list(1, 2, 3)),
    # simulates the feature-fetch request failing (e.g. an HTTP 504) after
    # the objectIds lookup already succeeded
    hgf_sf = function(...) NULL,
    .package = "hydrogeofetch")

  expect_warning(
    out <- hydrogeofetch:::query_usgs_arcrest(
      AOI_new, service = "3DHP_all", type = "flowline"),
    "features found")

  expect_null(out)
})
