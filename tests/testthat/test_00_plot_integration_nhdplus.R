source(system.file("extdata/sample_data.R", package = "hydrogeofetch"))

test_that("basics work", {

  skip_if_no_integration()
  skip_on_os("mac")
  tempd <- tempdir(check = TRUE)

  site <- "USGS-05428500"
  g_temp <- file.path(tempd, "foo.gpkg")

  d <-  hydrogeofetch:::get_plot_data(site, gpkg = g_temp, flowline_only = FALSE)
  expect_equal(names(d), c("plot_bbox", "outlets", "flowline", "basin",
                           "catchment","network_wtbd","off_network_wtbd"))

  expect_true(all(c("comid", "type") %in% names(d$outlets)))
  l <- sf::st_layers(g_temp)
  expect_equal(l$name,
               c("CatchmentSP", "NHDFlowline_Network", "NHDArea", "NHDWaterbody", "NHDFlowline_NonNetwork"))
  expect_equal(l$features, c(431, 84, 1, 90, 45))

  pdf(NULL)
  tempf <- file.path(tempd, "temp.png")

  png(file.path(tempd, "temp.png"))
  plot_nhdplus("USGS-05428500")
  dev.off()

  expect_true(file.exists(tempf))

  unlink(tempf)
  unlink(g_temp)

  # For test performance
  Sys.setenv(MAKE_BASIN="FALSE")

  png(file.path(tempd, "temp.png"))

  plot_nhdplus(list(list("comid", "13293970"),
                    list("nwissite", "USGS-05428500"),
                    list("huc12pp", "070900020603"),
                    list("huc12pp", "070900020602")),
               streamorder = 2,
               nhdplus_data = sample_data,
               gpkg = g_temp)
  dev.off()

  l <- sf::st_layers(g_temp)
  expect_equal(l$name,
               c("NHDFlowline_Network", "CatchmentSP", "NHDArea", "NHDWaterbody",
                 "NHDFlowline_NonNetwork", "Gage"))
  expect_equal(l$features, c(251, 250, 3, 117, 48, 44))

  expect_true(file.exists(tempf))
  unlink(tempf)

  unlink(g_temp)

  test_cache <- file.path(get_test_dir(), "plot_test_2.rds")

  png(file.path(tempd, "temp.png"))
  plot_nhdplus(sf::st_as_sf(data.frame(x = -89.36083,
                                       y = 43.08944),
                            coords = c("x", "y"), crs = 4326),
               streamorder = 2,
               nhdplus_data = sample_data,
               gpkg = g_temp)
  dev.off()

  l <- sf::st_layers(g_temp)
  expect_equal(l$name,
               c("NHDFlowline_Network", "CatchmentSP",
                 "NHDArea", "NHDWaterbody",
                 "NHDFlowline_NonNetwork", "Gage"))
  expect_equal(l$features, c(168, 167, 1, 90, 45, 33))

  expect_true(file.exists(tempf))
  unlink(tempf)

  # Test caching
  tmp_cache <- file.path(tempd, "temp.rds")
  unlink(tmp_cache, force = TRUE)

  png(file.path(tempd, "temp.png"))
  plot_nhdplus("USGS-05428500", streamorder = 3, cache_data = tmp_cache)
  dev.off()

  unlink(tempf)
  expect_true(file.exists(tmp_cache))

  png(file.path(tempd, "temp.png"))
  plot_nhdplus("USGS-05428500", streamorder = 3, cache_data = tmp_cache)
  dev.off()

  expect_true(file.exists(tempf))
  unlink(tempf)
  unlink(tmp_cache, force = TRUE)
})
