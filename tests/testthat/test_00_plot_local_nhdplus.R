source(system.file("extdata/sample_data.R", package = "hydrogeofetch"))

test_that("local data", {
  testthat::skip_on_ci()
  # For test performance
  Sys.setenv(MAKE_BASIN="FALSE")

  skip_if_no_integration()

  fline <- sf::read_sf(sample_data, "NHDFlowline_Network")
  gage <- sf::read_sf(sample_data, "Gage")

  # site_data <- dataRetrieval::whatNWISdata(siteNumber = gage$SOURCE_FEA,
  #                                          parameterCd = c("00060"), statCd = c("00003"))
  #
  # site <- filter(site_data, stat_cd == "00003") |>
  #   filter(count_nu == max(count_nu)) |>
  #   sf::st_as_sf(coords = c("dec_long_va", "dec_lat_va"), crs = sf::st_crs(4326))
  #
  # start <- hydrogeofetch::get_flowline_index(fline, sf::st_transform(site, sf::st_crs(fline)))$COMID
  # or in this case they are the same
  start <- gage$FLComID[which(gage$SOURCE_FEA == "05429500")]

  outlet <- c("comid", start)

  expect_error(plot_nhdplus(outlets = outlet, nhdplus_data = "borked", gpkg = "borky"),
               "couldn't find nhdplus_data and output data not requested by the same path.")

  expect_error(plot_nhdplus(outlets = outlet, nhdplus_data = "borked", gpkg = "borked"),
               "output_file must end in '.gpkg'")

  plot_data <- hydrogeofetch:::get_plot_data(outlets = outlet, nhdplus_data = sample_data)

  expect_equal(names(plot_data), c("plot_bbox", "outlets", "flowline", "basin", "catchment",
                                   "network_wtbd","off_network_wtbd"))
  expect_equal(nrow(plot_data$flowline), 251)
  expect_equal(plot_data$outlets$type, "comid")

  plot_data <- hydrogeofetch:::get_plot_data(outlets = outlet, streamorder = 3, nhdplus_data = sample_data)
  expect_equal(nrow(plot_data$flowline), 251)
  expect_true(all(c("comid", "type") %in% names(plot_data$outlets)))

  outlet <- c(list(outlet), list(c("nwissite", "USGS-05428500")))
  plot_data <- hydrogeofetch:::get_plot_data(outlets = outlet, streamorder = 3, nhdplus_data = sample_data)

  expect_true(all(names(plot_data$outlets) %in% c("comid", "geom", "type")))
  expect_equal(as.integer(plot_data$outlets$comid), as.integer(c("13293970", "13293750")))

  expect_s3_class(sf::st_geometry(plot_data$flowline)[[1]], "XY")

  plot_data <- hydrogeofetch:::get_plot_data(outlets = c(outlet, "USGS-05428500"),
                                            streamorder = 3, nhdplus_data = sample_data)

  expect_equal(nrow(plot_data$outlets), 3)
  expect_equal(plot_data$outlets$type, c("comid", "nwissite", "nwissite"))

  # plot_nhdplus(outlets = outlet, nhdplus_data = sample_data)
  #
  # plot_nhdplus(outlets = outlet, nhdplus_data = sample_data, streamorder = 3)
})
