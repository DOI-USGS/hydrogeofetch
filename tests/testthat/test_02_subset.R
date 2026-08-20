source(list.files(pattern = "helper.R", recursive = TRUE, full.names = TRUE))

test_that("subset errors", {

  temp_dir <- tempdir()

  nhdplus_path(file.path(temp_dir, "borked"))

  expect_error(subset_nhdplus(1234567,
                              output_file = tempfile(fileext = ".gpkg")),
               "couldn't find nhdplus data")

  expect_error(subset_nhdplus(comids = "dud",
                              output_file = "test",
                              "output_file must end in '.gpkg'"))

  expect_error(subset_nhdplus(output_file = "test",
                              "must provide comids or bounding box"))

  nhdplus_path("../NHDPlusV21_National_Seamless.gdb")

  expect_error(subset_nhdplus(nhdplus_data = "download"), "must provide comids or bounding box")

  expect_error(subset_nhdplus(comids = integer(0), nhdplus_data = "download"), "comids must be NULL or non-empty")
})

test_that("subset runs as expected", {

  temp_dir <- tempdir()
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)
  unlink(file.path(temp_dir, "*"))

  source(system.file("extdata/sample_data.R", package = "hydrogeofetch"))

  fl <- sf::read_sf(sample_data, "NHDFlowline_Network")

  fl <- sf::st_zm(fl)
  fl$geom[2] <- sf::st_cast(fl$geom[2], "LINESTRING")

  fl <- hydroloom::check_valid(fl)

  expect_s3_class(sf::st_geometry(fl), "sfc_LINESTRING")

  expect_null(hydroloom::check_valid(NULL))

  expect_equal(hydrogeofetch:::get_catchment_layer_name(TRUE, sample_data), "CatchmentSP")
  expect_equal(hydrogeofetch:::get_catchment_layer_name(TRUE, "download"), "CatchmentSP")
  expect_equal(hydrogeofetch:::get_catchment_layer_name(FALSE, "download"), "CatchmentSP")

  nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))
  tempf <- file.path(temp_dir, "temp.geojson")
  sf::write_sf(nc, tempf, "Catchment")
  expect_equal(hydrogeofetch:::get_catchment_layer_name(FALSE, tempf), "Catchment")

  nhdplus_path(sample_data)

  if (!dir.exists(temp_dir)) dir.create(temp_dir)

  fline <- sf::read_sf(nhdplus_path(), "NHDFlowline_Network")

  all_comids <- fline$COMID

  comids <- hydroloom::navigate_hydro_network(fline, 13293392, "UM")

  out_file <- tempfile(fileext = ".gpkg")

  # No output file
  fi <- subset_nhdplus(comids = comids,
                       output_file = NULL,
                       nhdplus_data = sample_data,
                       status = FALSE)

  expect_equal(names(fi), c("NHDFlowline_Network", "CatchmentSP", "NHDArea",
                            "NHDWaterbody", "NHDFlowline_NonNetwork", "Gage", "Sink"))

  expect_equal(nrow(fi$CatchmentSP), 4)
  expect_equal(nrow(fi$NHDWaterbody), 1)

  # flowline only
  fi <- subset_nhdplus(comids = comids,
                       output_file = NULL,
                       nhdplus_data = sample_data,
                       status = FALSE,
                       flowline_only = TRUE)

  expect_equal(names(fi), "NHDFlowline_Network")

  # write to output file
  fi <- subset_nhdplus(comids = comids,
                 output_file = out_file,
                 nhdplus_data = sample_data,
                 return_data = FALSE,
                 status = FALSE)

  expect_equal(fi, out_file)

  # don't overwrite
  expect_error(
    subset_nhdplus(comids = comids,
                   output_file = out_file,
                   nhdplus_data = sample_data,
                   overwrite = FALSE,
                   status = FALSE),
    "output_file exists and overwrite is false.")

  # check that status works
  messages <- capture_messages(
    fi <- subset_nhdplus(comids = comids,
                         output_file = out_file,
                         overwrite = TRUE,
                         status = TRUE))

  if(interactive()) {
    expect_equal(length(messages), 17)
  }

  check_layers(out_file)

  unlink(file.path(temp_dir, "*"))

  # Make sure passing data directly works
  fi <- subset_nhdplus(comids = comids,
                       output_file = out_file,
                       nhdplus_data = sample_data,
                       overwrite = FALSE,
                       status = FALSE)

  check_layers(out_file)

  unlink(file.path(temp_dir, "*"))

  nhdplus_path("../NHDPlusV21_National_Seamless.gdb")
})

test_that("subset download", {

  skip_if_no_integration()

  temp_dir <- tempdir()
  dir.create(temp_dir, recursive = TRUE, showWarnings = FALSE)

  out_file <- tempfile(fileext = ".gpkg")

  source(system.file("extdata/sample_data.R", package = "hydrogeofetch"))

  fl <-  sf::read_sf(sample_data, "NHDFlowline_Network")

  comids <- hydroloom::navigate_hydro_network(fl, 13293392, "UM")

  all_comids <- fl$COMID

  # download option
  expect_true("No nhdarea features found" %in%
                capture_warnings(fi <- subset_nhdplus(comids = comids,
                                                      output_file = out_file,
                                                      nhdplus_data = "download",
                                                      overwrite = FALSE,
                                                      status = FALSE,
                                                      flowline_only = FALSE)))

  check_layers(out_file)

  unlink(file.path(temp_dir, "*"))

  fi <- subset_nhdplus(comids = comids,
                       output_file = out_file,
                       nhdplus_data = "download",
                       overwrite = FALSE,
                       status = FALSE)

  expect_equal(length(names(fi)), 1)

  unlink(file.path(temp_dir, "*"))

  fi <- subset_nhdplus(comids = all_comids,
                       output_file = out_file,
                       nhdplus_data = "download",
                       overwrite = FALSE,
                       status = FALSE)

  expect_equal(nrow(fi$NHDFlowline_Network), length(all_comids))

  })

test_that("subset works with HR", {
  work_dir <- tempdir()

  skip_if_no_integration()
  get_test_file(work_dir)

  out_gpkg <- file.path(work_dir, "temp.gpkg")

  hr <- get_nhdplushr(work_dir, out_gpkg = out_gpkg,
                      layers = c("NHDFlowline", "NHDPlusCatchment", "NHDWaterbody",
                                 "NHDArea", "NHDPlusSink"))

  expect_equal(hydrogeofetch:::get_catchment_layer_name(TRUE, out_gpkg), "NHDPlusCatchment")
  expect_equal(hydrogeofetch:::get_catchment_layer_name(FALSE, out_gpkg), "NHDPlusCatchment")

  flowlines <- sf::read_sf(out_gpkg, "NHDFlowline")

  up_ids <- hydroloom::navigate_hydro_network(flowlines, 15000500028335, "UT")

  suppressWarnings(sub <- subset_nhdplus(up_ids,
                                         file.path(work_dir,
                                                   "sub.gpkg"),
                                         out_gpkg,
                                         return_data = FALSE))

  layers <- sf::st_layers(sub)

  expect_equal(length(layers$name), 4)
  expect_equal(layers$features[which(layers$name == "NHDFlowline")], 1427)
})

