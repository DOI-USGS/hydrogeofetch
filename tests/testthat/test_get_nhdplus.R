

source(system.file("extdata", "sample_flines.R", package = "hydrogeofetch"))

pt_data <- sample_flines

test_that("get_nhdplus_byid", {
  comid_set <- hydroloom::navigate_hydro_network(pt_data, 11687180, "UT")

  expect_error(hydrogeofetch:::get_nhdplus_byid(comid_set, "testest"),
               "Layer must be one of catchmentsp, nhdflowline_network")

  with_mock_hgf("nhdplus_byid", {
    catchmentsp <- hydrogeofetch:::get_nhdplus_byid(comid_set, "catchmentsp")
    expect_true("sf" %in% class(catchmentsp))
    expect_equal(nrow(catchmentsp), 5)

    nhdflowline_network <- hydrogeofetch:::get_nhdplus_byid(comid_set, "nhdflowline_network")
    expect_true("sf" %in% class(nhdflowline_network))
    expect_equal(nrow(nhdflowline_network), 5)
  })
})

test_that("get_nhdplus_bybox", {
  bbox <- pt_data |>
    sf::st_transform(4326) |>
    sf::st_bbox() |>
    sf::st_as_sfc()

  expect_error(hydrogeofetch:::get_nhdplus_bybox(bbox, "borked"),
               "Layer must be one of nhdarea, nhdwaterbody, nhdflowline_network, nhdflowline_nonnetwork, catchmentsp.")

  with_mock_hgf("nhdplus_bybox", {
    for (layer in c("nhdarea", "nhdwaterbody")) {
      l <- hydrogeofetch:::get_nhdplus_bybox(bbox, layer)
      expect_true(nrow(l) > 1)
      expect_true("sf" %in% class(l))
    }
  })
})

test_that("downloaders run", {
  skip_if_no_integration()
  dir <- tempdir()
  mess <- capture_messages(
    out <- download_nhdplusv2(outdir = dir,
                              url = "https://doi-usgs.github.io/hydrogeofetch/data/NHDPlus_test.gdb.7z",
                              progress = FALSE))
  unlink(dir, recursive = T)
  expect_true(grepl("NHDPlus_test.gdb", out))

  dir <- tempdir()
  mess <- capture_messages(
    out <- download_rf1(outdir = dir,
                        url = "https://doi-usgs.github.io/hydrogeofetch/data/rf1_test.e00.gz",
                        progress = FALSE))
  unlink(dir, recursive = T)
  expect_true(grepl("rf1_test.e00", out))

  dir <- tempdir()
  temp <- capture.output(mess <- capture_messages(out <- download_wbd(outdir = dir,
                                               url = "https://doi-usgs.github.io/hydrogeofetch/data/WBD_test.gdb.zip",
                                               progress = TRUE)))
  unlink(dir, recursive = T)
  expect_true(grepl("WBD_test.gdb", out))
})
