source(system.file("extdata/sample_data.R", package = "hydrogeofetch"))

test_that("local data 2", {

  # For test performance
  Sys.setenv(MAKE_BASIN="FALSE")

  skip_if_no_integration()

  gage <- sf::read_sf(sample_data, "Gage")

  start <- gage$FLComID[which(gage$SOURCE_FEA == "05429500")]

  plot_data <- hydrogeofetch:::get_plot_data(outlets = list("05427850",
                                                           "05427718",
                                                           c("comid", start),
                                                           c("nwissite", "USGS-05428500"),
                                                           c("huc12pp", "070900020603"),
                                                           c("huc12pp", "070900020602")),
                                            nhdplus_data = sample_data, streamorder = 3)

  expect_equal(nrow(plot_data$outlets), 6)

  # Also works with remote data.
  plot_data <- hydrogeofetch:::get_plot_data(outlets = list("05427718",
                                                           "05427850",
                                                           c("comid", start),
                                                           c("nwissite", "USGS-05428500"),
                                                           c("huc12pp", "070900020603"),
                                                           c("huc12pp", "070900020602")))
  expect_equal(nrow(plot_data$outlets), 6)
  expect_true("sfc_POINT" %in% class(sf::st_geometry(plot_data$outlets)))

  plot_data <- hydrogeofetch:::get_plot_data(sf::st_as_sf(data.frame(x = -89.36083,
                                                                    y = 43.08944),
                                                         coords = c("x", "y"), crs = 4326),
                                            streamorder = 2,
                                            nhdplus_data = sample_data)
  expect_equal(nrow(plot_data$outlets), 1)

})
