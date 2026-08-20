

test_that("nldi basics work", {

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")

  expect_error(hydrogeofetch:::check_nldi_feature(nldi_nwis[1]),
                 "Missing some required input for NLDI. Expected length 2 character vector or list with optional names: featureID")

  with_mock_hgf("nldi_basics", {
    nldi_sources <- dataRetrieval::get_nldi_sources()

    expect_equal(class(nldi_sources), "data.frame")

    expect_true(all(c("comid", "huc12pp", "nwissite") %in% nldi_sources$source))

    expect_true(all(names(nldi_sources) %in% c("source", "sourceName", "features")))

    idx <- get_nldi_index(c(-89.276, 42.988))
    expect_equal(nrow(idx), 2)
  })
})

test_that("navigation works", {

  with_mock_hgf("nldi_navigation", {
    nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")

    nav <- navigate_nldi(nldi_feature = nldi_nwis,
                         mode = "UM",
                         data_source = "nwissite",
                         distance_km = 1)

    expect_true("sf" %in% class(nav$UM_nwissite))

    expect_true("sfc_POINT" %in% class(sf::st_geometry(nav$UM_nwissite)),
           "expected point response")

    nav2 <- navigate_nldi(nldi_feature = nldi_nwis,
                         mode = "upstreamMain",
                         data_source = "nwissite",
                         distance_km = 100)

    expect_true(nrow(nav2$UM_nwissite) > nrow(nav$UM_nwissite))

    nldi_nwis <- as.character(nldi_nwis)

    nav3 <- navigate_nldi(nldi_feature = nldi_nwis,
                          mode = "upstreamMain",
                          data_source = "flowlines",
                          distance_km = 10)

    expect_s3_class(sf::st_geometry(nav3$UM), "sfc_LINESTRING")

    expect_warning(nav3 <- navigate_nldi(nldi_feature = nldi_nwis,
                                         mode = "upstreamMain",
                                         data_source = "flowline",
                                         distance_km = 10),
                   "data source specified as flowline or '' is deprecated")

    nav <- navigate_nldi(nldi_feature = nldi_nwis,
                         mode = "https://api.water.usgs.gov/api/nldi/linked-data/nwissite/USGS-08279500/navigation/UM",
                         data_source = "dumb",
                         distance_km = 1)

    expect_equal(nav$origin$sourceName, "NWIS Surface Water Sites")

    expect_equal(class(as.integer(nav$origin$comid)), "integer")

    nav <- navigate_nldi(nldi_feature = nldi_nwis,
                         mode = "https://api.water.usgs.gov/api/nldi/linked-data/nwissite/USGS-08279500/navigation/UM",
                         data_source = "nwissite",
                         distance_km = 1)

    expect_true("sf" %in% sapply(nav, class))
  })
})

test_that("basin works", {

  skip_if_no_integration()
  skip_on_ci()

  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-05428500")

  site <- get_nldi_feature(nldi_nwis)

  nav <- get_nldi_basin(nldi_feature = nldi_nwis)

  expect_true("sf" %in% class(nav))

  expect_true("sfc_POLYGON" %in% class(sf::st_geometry(nav)),
         "expected polygon response")

  basin2 <- get_nldi_basin(nldi_feature = nldi_nwis,
                           simplify = FALSE, split = TRUE)

  if(length(sf::st_geometry(basin2)[[1]]) > 1) {
    lens <- sapply(sf::st_geometry(basin2)[[1]], \(x) nrow(x[[1]]))
    sf::st_geometry(basin2) <- sf::st_sfc(
      sf::st_polygon(sf::st_geometry(basin2)[[1]][[which(lens == max(lens))]]), crs = sf::st_crs(basin2))
    basin2 <- sf::st_cast(basin2, "POLYGON")
  }

  expect_true(length(sf::st_coordinates(nav)) < length(sf::st_coordinates(basin2)))

  expect_true(!sf::st_crosses(sf::st_cast(nav, "LINESTRING"),
                              sf::st_buffer(site, units::set_units(50, "m")),
                                 sparse = FALSE))

  expect_true(sf::st_crosses(sf::st_cast(basin2, "LINESTRING"),
                             sf::st_buffer(site, units::set_units(50, "m")),
                             sparse = FALSE))
})

test_that("get feature works", {
  skip_if_no_integration()

  # NLDI adds and drops optional columns (mainstem, X, Y) without notice, so
  # assert on the stable core rather than an exact column count.
  core_atts <- c("sourceName", "identifier", "comid", "name",
                 "reachcode", "measure", "geometry")

  f <- get_nldi_feature(list(featureSource = "nwissite", featureID = "USGS-05428500"))

  expect_equal(nrow(f), 1)
  expect_true(all(core_atts %in% names(f)))
  expect_equal(f$identifier, "USGS-05428500")

  f <- get_nldi_feature(list("nwissite", "USGS-05428500"))

  expect_equal(nrow(f), 1)
  expect_true(all(core_atts %in% names(f)))
  expect_equal(f$identifier, "USGS-05428500")

})

test_that("request coordinates are rounded", {
  # a coordinate that has been through a transform carries digits that differ
  # between PROJ builds; the request body has to come out the same either way
  expect_equal(hydrogeofetch:::coord_chr(-89.21579999999998), "-89.2158")
  expect_equal(hydrogeofetch:::coord_chr(42.9561), "42.9561")
})

test_that("raindrop", {

  with_mock_hgf("raindrop", {
    point <- sf::st_sfc(sf::st_point(x = c(-89.2158, 42.9561)), crs = 4326)

    trace <- get_raindrop_trace(point, direction = "up")

    expect_equal(trace$id[1], 'upstreamFlowline')

    expect_equal(trace$id[2], "raindropPath")

    expect_equal(nrow(trace), 2)

    expect_true(inherits(trace, "sf"))

    expect_type(trace$intersection_point, "list")
  })

  expect_error(get_raindrop_trace(
    sf::st_sfc(sf::st_point(x = c(-89.2158, 42.9561)), crs = 4326),
    direction = "borked"),
    "direction must be in up, down, none")

})

test_that("split", {

  with_mock_hgf("split_catchment", {
    snap_point <- sf::st_sfc(sf::st_point(c(-89.213274, 42.956989)),
                             crs = 4326)

    catchment <- get_split_catchment(snap_point, upstream = TRUE)

    area <- sf::st_area(catchment)

    expect_true(area[1] < units::set_units(7000000, "m^2"))

    expect_true(area[2] > units::set_units(900000000, "m^2"))

    point <- sf::st_sfc(sf::st_point(c(-20.213274, 42.956989)),
                        crs = 4326)

    expect_message(suppressWarnings(get_split_catchment(point, upstream = TRUE)),
                   "Ensure that the point")
  })

  # Doesn't improve coverage
  # catchment2 <- get_split_catchment(snap_point, upstream = FALSE)
  #
  # area <- sf::st_area(catchment2)
  #
  # expect_true(area[1] < units::set_units(7000000, "m^2"))
  #
  # expect_true(area[2] < units::set_units(900000000, "m^2"))
  #
  # pour_point <- sf::st_sfc(sf::st_point(x = c(-89.25619, 42.98646)), crs = 4326)
  #
  # catchment3 <- get_split_catchment(pour_point, upstream = FALSE)
  #
  # area <- sf::st_area(catchment3)
  #
  # expect_true(area[1] < units::set_units(7000000, "m^2"))
  #
  # expect_true(area[2] < units::set_units(40000, "m^2"))

})

test_that("xs", {

  skip_if_no_integration()
  skip_on_ci()

  point <- sf::st_sfc(sf::st_point(x = c(-105.97218, 36.17592)), crs = 4326)

  xs <- get_xs_point(point, 300, 100)

  skip_if(is.null(xs), "xs service broken?")
  
  expect_true(inherits(xs, "sf"))
  expect_equal(nrow(xs), 101)

  expect_true(all(c("distance_m", "elevation_m") %in% names(xs)))

  point1 <- sf::st_sfc(sf::st_point(x = c(-105.9667, 36.17602)), crs = 4326)
  point2 <- sf::st_sfc(sf::st_point(x = c(-105.97768, 36.17526)), crs = 4326)

  xs <- get_xs_points(point1, point2, 100)

  expect_true(inherits(xs, "sf"))
  expect_equal(nrow(xs), 101)

  expect_true(all(c("distance_m", "elevation_m") %in% names(xs)))

  expect_error(get_xs_points(point1, point2, 100, 2),
               "res input must be one of 1, 3, 5, 10, 30, 60")

  point1 <- sf::st_sfc(sf::st_point(x = c(-105.9667, 36.17602)), crs = 4326)
  point2 <- sf::st_sfc(sf::st_point(x = c(-105.97768, 36.17526)), crs = 4326)
  point3 <- sf::st_sfc(sf::st_point(x = c(-105.98869, 36.17450)), crs = 4326)

  points <- sf::st_as_sf(c(point1, point2, point3))

  suppressMessages(xs <- get_elev_along_path(points, 100))

  expect_true(all(names(xs) %in% c("id", "distance_m", "elevation_m", "spatial_ref", "geometry",
                                   ".group")))

  expect_equal(nrow(xs), 202)

  expect_true(mean(xs$elevation_m) - 1822 < 1)

  suppressMessages(xs <- get_elev_along_path(points, 33))

  expect_equal(nrow(xs), 66)
})

test_that("coverage", {
  assign("nldi_tier", "borked",
         envir = hydrogeofetch:::hydrogeofetch_env)

  # may bring back but not relevant now
  # expect_error(hydrogeofetch:::get_nldi_url(),
  #              "only prod or test allowed.")

  tier_env <- Sys.getenv("API_WATER_TIER")

  Sys.unsetenv("API_WATER_TIER")

  assign("api_water_tier", "test",
         envir = hydrogeofetch:::hydrogeofetch_env)

  test <- hydrogeofetch:::get_nldi_url()

  expect_true(grepl("https", test))

  test <- hydrogeofetch:::get_nldi_url(pygeo = TRUE)

  expect_true(grepl("pygeoapi", test))

  test <- hydrogeofetch:::get_water_url()

  expect_true(grepl("fabric/pygeoapi", test))

  Sys.setenv("API_WATER_TIER" = tier_env)

  assign("api_water_tier", "prod",
         envir = hydrogeofetch:::hydrogeofetch_env)

})
