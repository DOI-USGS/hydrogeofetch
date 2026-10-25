test_that("mainstem_id_from_uri parses geoconnex mainstem uris", {
  expect_equal(hydrogeofetch:::mainstem_id_from_uri(
    "https://geoconnex.us/ref/mainstems/47068"), 47068L)

  expect_equal(hydrogeofetch:::mainstem_id_from_uri(
    c("https://geoconnex.us/ref/mainstems/1",
      "https://geoconnex.us/ref/mainstems/2086165")),
    c(1L, 2086165L))
})

test_that("parse_replacement_uris handles empty, single, and multiple replacements", {
  out <- hydrogeofetch:::parse_replacement_uris(c(
    "",
    "['https://geoconnex.us/ref/mainstems/47068']",
    "['https://geoconnex.us/ref/mainstems/2547827', 'https://geoconnex.us/ref/mainstems/2547826']"
  ))

  expect_equal(out[[1]], character(0))
  expect_equal(out[[2]], "https://geoconnex.us/ref/mainstems/47068")
  expect_equal(out[[3]], c("https://geoconnex.us/ref/mainstems/2547827",
                          "https://geoconnex.us/ref/mainstems/2547826"))
})

test_that("add_mainstems joins comids to mainstem ids using a cached lookup", {
  tmp_dir <- file.path(tempdir(), "mainstems_test_cache")
  dir.create(tmp_dir, showWarnings = FALSE)

  old_dir <- hydrogeofetch_data_dir()
  hydrogeofetch_data_dir(tmp_dir)
  on.exit(hydrogeofetch_data_dir(old_dir), add = TRUE)

  arrow::write_parquet(
    data.frame(uri = c("https://geoconnex.us/ref/mainstems/1826652",
                       "https://geoconnex.us/ref/mainstems/1826652"),
              comid = c(2804607, 2804621)),
    get_mainstem_lookup_path("nhdpv2"))

  out <- add_mainstems(data.frame(comid = c(2804607, 999999999)))

  expect_equal(out$mainstemid, c(1826652, NA_integer_))
  expect_equal(out$mainstem_uri[1],
              "https://geoconnex.us/ref/mainstems/1826652")
})

test_that("add_mainstems downloads and joins the real nhdpv2 lookup table", {
  skip_if_no_integration()

  out <- add_mainstems(data.frame(comid = c(2804607, 2804621)))

  expect_equal(out$mainstemid, c(1826652, 1826652))
})

test_that("check_mainstems flags superseded ids and leaves current ones alone", {
  with_mock_hgf("mainstems_check", {
    res <- check_mainstems(c(2086165, 2086637, 359842))

    expect_equal(res, c(TRUE, TRUE, FALSE))
  })
})

test_that("check_mainstems does not confuse a different mainstem namespace", {
  with_mock_hgf("mainstems_check", {
    # 2086165 (bare id) is a real, superseded ref/mainstems id. A uri with
    # the same trailing number minted under a different authority must not
    # be treated as that ref/mainstems id.
    res <- check_mainstems(c(2086165, "https://geoconnex.us/usgs/mainstems/2086165"))

    expect_equal(res, c(TRUE, FALSE))
  })
})

mainstem_points <- function(id) {
  feature <- hydrogeofetch:::hgf_sf(paste0(
    "https://reference.geoconnex.us/collections/mainstems/items/", id, "?f=json"))

  coords <- sf::st_coordinates(feature)

  pts <- sf::st_sfc(lapply(round(seq(1, nrow(coords), length.out = 3)), function(i) {
    sf::st_point(coords[i, 1:2])
  }), crs = sf::st_crs(feature))

  sf::st_sf(mainstemid = id, geometry = pts)
}

test_that("update_mainstems resolves a superseded mainstem with replacements", {
  with_mock_hgf("mainstems_update_wood", {
    x <- mainstem_points(2086165)

    out <- update_mainstems(x)

    expect_true(all(out$mainstem_update_status == "updated"))
    expect_true(all(out$mainstemid %in% c(2479116, 2479066)))
  })
})

test_that("update_mainstems flags an unresolvable supersession", {
  with_mock_hgf("mainstems_update_stony", {
    x <- mainstem_points(2086637)

    expect_warning(out <- update_mainstems(x), "could not be resolved")

    expect_true(all(out$mainstem_update_status == "unresolved"))
    expect_equal(out$mainstemid, rep(2086637, 3))
  })
})

# Point hydrogeofetch_data_dir() at a temp dir holding small extracts of the
# nhdpv2 and nhdphr lookups and the updated network attributes:
# ref/mainstems/1003325 (two comids) with 726411 flowing into it, and
# NHDPlusHR ref/mainstems/607430 (four flowlines) with 483254 flowing into
# it. Returns the old data dir.
setup_mainstem_data <- function() {
  tmp_dir <- file.path(tempdir(), "mainstem_flowlines_cache")
  dir.create(tmp_dir, showWarnings = FALSE)

  old_dir <- hydrogeofetch_data_dir()
  hydrogeofetch_data_dir(tmp_dir)

  arrow::write_parquet(
    utils::read.csv(test_path("data", "mainstems_nhdpv2_lookup.csv")),
    get_mainstem_lookup_path("nhdpv2"))

  arrow::write_parquet(
    utils::read.csv(test_path("data", "mainstems_enhd.csv")),
    get_vaa_path(updated_network = TRUE))

  # the real table carries nhdplushr_id as int64
  hr <- arrow::arrow_table(utils::read.csv(
    test_path("data", "mainstems_nhdphr_lookup.csv"),
    colClasses = "character"))
  hr$nhdplushr_id <- hr$nhdplushr_id$cast(arrow::int64())
  arrow::write_parquet(hr, get_mainstem_lookup_path("nhdphr"))

  old_dir
}

ms <- function(id) paste0("https://geoconnex.us/ref/mainstems/", id)

test_that("read_mainstem_lookup filters by uri", {
  old_dir <- setup_mainstem_data()
  on.exit(hydrogeofetch_data_dir(old_dir), add = TRUE)

  out <- hydrogeofetch:::read_mainstem_lookup("nhdpv2", uris = ms(1003325))

  expect_equal(nrow(out), 2)
  expect_true(all(out$uri == ms(1003325)))

  expect_error(hydrogeofetch:::read_mainstem_lookup("nhdpv2", ids = 1,
                                                    uris = ms(1003325)),
               "not both")
})

test_that("get_mainstem_flowlines nhdpv2 gives hy_topo with confluences", {
  old_dir <- setup_mainstem_data()
  on.exit(hydrogeofetch_data_dir(old_dir), add = TRUE)

  with_mock_hgf("mainstem_flowlines_nhdpv2", {
    fl <- get_mainstem_flowlines(1003325, "nhdpv2",
                                 include_confluences = TRUE)
  })

  expect_s3_class(fl, "sf")
  expect_equal(names(fl), c("comid", "tocomid", "lengthkm", "hydroseq",
                            "mainstemid", "geometry"))
  expect_true(all(sf::st_geometry_type(fl) == "LINESTRING"))
  expect_equal(sum(fl$mainstemid == ms(1003325)), 2)

  expect_s3_class(hydroloom::hy(fl), "hy_topo")

  conf <- hydroloom::get_confluences(fl)

  expect_equal(conf$hydro_location_id, ms(726411))
})

test_that("get_mainstem_flowlines 3dhp gives hy_topo with confluences", {
  u <- "https://geoconnex.us/usgs/mainstems/10126436"

  with_mock_hgf("mainstem_flowlines_3dhp", {
    fl <- get_mainstem_flowlines(u, "3dhp", include_confluences = TRUE)
  })

  expect_s3_class(fl, "sf")
  expect_equal(names(fl), c("id3dhp", "toid", "hydroseq", "lengthkm",
                            "divergence", "featuredate", "mainstemid",
                            "geometry"))
  expect_true(all(sf::st_geometry_type(fl) == "LINESTRING"))
  expect_equal(sum(fl$mainstemid == u), 2)

  expect_s3_class(hydroloom::hy(fl), "hy_topo")

  conf <- hydroloom::get_confluences(fl)

  expect_equal(conf$hydro_location_id,
               "https://geoconnex.us/usgs/mainstems/6591895")
})

test_that("get_mainstem_flowlines nhdphr gives hy_node with confluences", {
  old_dir <- setup_mainstem_data()
  on.exit(hydrogeofetch_data_dir(old_dir), add = TRUE)

  with_mock_hgf("mainstem_flowlines_nhdphr", {
    fl <- get_mainstem_flowlines(607430, "nhdphr",
                                 include_confluences = TRUE)
  })

  expect_s3_class(fl, "sf")
  expect_equal(names(fl), c("nhdplusid", "fromnode", "tonode", "hydroseq",
                            "lengthkm", "divergence", "mainstemid",
                            "geometry"))
  expect_type(fl$nhdplusid, "character")
  expect_equal(sum(fl$mainstemid == ms(607430)), 4)

  expect_s3_class(hydroloom::hy(fl), "hy_node")

  conf <- hydroloom::get_confluences(fl)

  expect_equal(conf$hydro_location_id, ms(483254))
})

test_that("get_mainstem_flowlines finds New Hope tributaries live", {
  skip_if_no_integration()

  tribs <- ms(c(2408627, 2408909))

  for(network in c("nhdpv2", "3dhp")) {
    fl <- get_mainstem_flowlines(2408056, network, include_confluences = TRUE)

    expect_true(all(tribs %in%
                      hydroloom::get_confluences(fl)$hydro_location_id),
                info = network)
  }
})

test_that("get_mainstem_flowlines degrades to NULL with a warning", {
  old_dir <- setup_mainstem_data()
  on.exit(hydrogeofetch_data_dir(old_dir), add = TRUE)

  local_mocked_bindings(get_nhdplus = function(...) NULL)

  expect_warning(out <- get_mainstem_flowlines(ms(1003325), "nhdpv2"),
                 "No nhdpv2 flowlines found for mainstem\\(s\\): .*1003325")

  expect_null(out)

  # a uri with no lookup rows is reported the same way
  expect_warning(out <- get_mainstem_flowlines(1, "nhdpv2"),
                 "mainstems/1$")

  expect_null(out)
})
