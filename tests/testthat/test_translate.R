ms <- function(id) paste0("https://geoconnex.us/ref/mainstems/", id)

# Small networks around the Northeast Creek (2408627) mouth on New Hope River
# (2408056), in the shape get_mainstem_flowlines() returns: seven New Hope
# flowlines and the outlet flowline of each identified tributary into them.
# V2 is built from hydroloom's shipped New Hope sample; 3DHP is an extract of
# docs/data/new_hope_3dhp.gpkg with a fixed featuredate.
translate_test_networks <- function() {
  g <- sf::read_sf(system.file("extdata/new_hope.gpkg", package = "hydroloom"))
  csv <- utils::read.csv(system.file("extdata/new_hope_mainstems.csv",
                                     package = "hydroloom"))

  g$mainstemid <- csv$mainstem_uri[match(g$COMID, csv$comid)]
  g$REACHCODE <- NULL

  v2 <- hydroloom::add_toids(hydroloom::hy(g))
  v2 <- v2[v2$id %in% c(8894360, 8894356, 8894354, 8894352, 8894344, 8894332,
                        8894324, 8894318, 8894322, 8894330, 8894364, 8894358,
                        8894350, 8894342, 8894326), ]

  v2 <- sf::st_sf(data.frame(comid = v2$id, tocomid = v2$toid,
                             lengthkm = v2$length_km, hydroseq = v2$topo_sort,
                             mainstemid = v2$aggregate_id),
                  geometry = sf::st_geometry(v2))

  v2 <- sf::st_zm(sf::st_transform(v2, 4326))
  v2 <- sf::st_set_geometry(v2, sf::st_cast(sf::st_geometry(v2), "LINESTRING"))

  list(nhdpv2 = v2,
       "3dhp" = sf::read_sf(test_path("data", "new_hope_3dhp_window.geojson")))
}

# Mocks get_mainstem_flowlines() and the superseded list (ref/mainstems/2 is
# superseded) and points hydrogeofetch_data_dir() at a temp dir holding a V2
# lookup for the test networks. The old data dir is returned as attribute
# old_dir for the caller to restore.
local_translate_mocks <- function(env = parent.frame()) {
  nets <- translate_test_networks()

  tmp_dir <- file.path(tempdir(), "translate_test_cache")
  dir.create(tmp_dir, showWarnings = FALSE)
  attr(nets, "old_dir") <- hydrogeofetch_data_dir()
  hydrogeofetch_data_dir(tmp_dir)

  arrow::write_parquet(data.frame(uri = nets$nhdpv2$mainstemid,
                                  comid = nets$nhdpv2$comid),
                       get_mainstem_lookup_path("nhdpv2"))

  local_mocked_bindings(
    get_mainstem_flowlines = function(mainstems, network,
                                      include_confluences = FALSE) {
      out <- nets[[network]]
      if(!include_confluences) out <- out[out$mainstemid %in% mainstems, ]
      if(nrow(out) == 0) NULL else out
    },
    fetch_superseded_mainstems = function() data.frame(uri = ms(2)),
    get_3dhp = function(ids, type, ...) {
      fl <- nets[["3dhp"]]
      fl[fl$id3dhp %in% ids, ]
    },
    .env = env)

  nets
}

# midpoints of V2 flowlines, as source points
v2_points <- function(nets, comids) {
  fl <- sf::st_transform(nets$nhdpv2[match(comids, nets$nhdpv2$comid), ], 5070)

  pts <- sf::st_cast(sf::st_line_sample(sf::st_geometry(fl), sample = 0.5),
                     "POINT")

  sf::st_sf(comid = comids, geometry = sf::st_transform(pts, 4326))
}

test_that("hydro_location keeps points on their side of a confluence", {
  nets <- local_translate_mocks()
  on.exit(hydrogeofetch_data_dir(attr(nets, "old_dir")), add = TRUE)

  # 8894344 is above the Northeast Creek mouth, 8894352 below it. Each also
  # sits the same distance from a V2-only confluence (536579, 1093187), which
  # 3DHP does not carry and so is not used.
  pts <- v2_points(nets, c(8894344, 8894352))

  out <- translate_hydrolocations(pts, "nhdpv2", "3dhp",
                                  method = "hydro_location")

  expect_s3_class(out, "sf")
  expect_equal(out$status, c("placed", "placed"))
  expect_equal(out$mainstem_uri, rep(ms(2408056), 2))
  expect_equal(out$hydro_location_id, rep(ms(2408627), 2))
  expect_equal(sign(out$distance_km), c(1, -1))

  nh_3dhp <- nets[["3dhp"]]$id3dhp[nets[["3dhp"]]$mainstemid == ms(2408056)]
  expect_true(all(out$id %in% nh_3dhp))

  expect_equal(out$measured_on, rep("3dhp featuredate 2025-01-01", 2))
})

test_that("geometry places points on their own mainstem in the target", {
  nets <- local_translate_mocks()
  on.exit(hydrogeofetch_data_dir(attr(nets, "old_dir")), add = TRUE)

  pts <- v2_points(nets, c(8894344, 8894352))

  out <- translate_hydrolocations(pts, "nhdpv2", "3dhp")

  expect_equal(out$status, c("placed", "placed"))
  expect_equal(out$method, c("geometry", "geometry"))

  # offsets are meters from indexing in EPSG:5070
  expect_true(all(out$offset > 0 & out$offset < 1000))

  nh_3dhp <- nets[["3dhp"]]$id3dhp[nets[["3dhp"]]$mainstemid == ms(2408056)]
  expect_true(all(out$id %in% nh_3dhp))
  expect_false("distance_km" %in% names(out))

  # and back: 3DHP points carry mainstemid, so no lookup is needed
  back <- sf::st_sf(id3dhp = out$id, mainstemid = out$mainstem_uri,
                    geometry = sf::st_geometry(out))

  back <- translate_hydrolocations(back, "3dhp", "nhdpv2",
                                   method = "hydro_location")

  expect_equal(back$status, c("placed", "placed"))
  expect_equal(sign(back$distance_km), c(1, -1))
  expect_equal(back$measured_on, rep("nhdpv2 ref_rivers v3.2", 2))
})

test_that("rows that cannot be placed say why", {
  nets <- local_translate_mocks()
  on.exit(hydrogeofetch_data_dir(attr(nets, "old_dir")), add = TRUE)

  pts <- v2_points(nets, rep(8894344, 4))
  pts$comid[1] <- 1
  pts$mainstem_uri <- c(NA, ms(2), ms(1), NA)

  expect_warning(
    out <- translate_hydrolocations(pts, "nhdpv2", "3dhp"),
    NA)

  expect_equal(out$status, c("no_mainstem", "mainstem_superseded",
                             "mainstem_not_in_target", "placed"))

  # a point far from its mainstem is not placed
  far <- sf::st_sf(comid = 8894344,
                   geometry = sf::st_sfc(sf::st_point(c(-79.5, 35.2)),
                                         crs = 4326))

  out <- translate_hydrolocations(far, "nhdpv2", "3dhp")

  expect_equal(out$status, "not_placed")
  expect_true(is.na(out$id))

  # Northeast Creek has no confluence of its own in either network, so a
  # point on it has no referent and is left on its mainstem only
  ne <- v2_points(nets, 8894342)

  out <- suppressMessages(
    translate_hydrolocations(ne, "nhdpv2", "3dhp", method = "hydro_location"))

  expect_equal(out$status, "no_shared_hydro_location")
  expect_equal(out$mainstem_uri, ms(2408627))
  expect_true(is.na(out$id))
})

test_that("translate_hydrolocations works on New Hope live", {
  skip_if_no_integration()

  fl <- get_nhdplus(comid = 8894344, realization = "flowline")
  fl <- sf::st_cast(sf::st_transform(sf::st_zm(fl), 5070), "LINESTRING")
  pt <- sf::st_sf(comid = 8894344,
                  geometry = sf::st_transform(sf::st_cast(sf::st_line_sample(
                    sf::st_geometry(fl), sample = 0.5), "POINT"), 4326))

  for(method in c("geometry", "hydro_location")) {
    out <- translate_hydrolocations(pt, "nhdpv2", "3dhp", method = method)

    expect_equal(out$status, "placed", info = method)
    expect_equal(out$mainstem_uri, ms(2408056), info = method)
  }
})

test_that("translate_flowlines works on Morgan Creek live", {
  skip_if_no_integration()

  # a mid-chain Morgan Creek comid (4.284 km), to 3DHP and back. The bottom
  # comid is a poor case: it ends in Jordan Lake, where the V2 and 3DHP
  # confluences with New Hope are about 700 m apart.
  out <- translate_flowlines(8896344, "nhdpv2", "3dhp")

  expect_true(all(out$status == "placed"))
  expect_lt(abs(sum(out$length_km) - 4.284) / 4.284, 0.1)

  back <- translate_flowlines(out$id, "3dhp", "nhdpv2")

  # 3DHP flowlines shorter than snap_m (7Q8YP is 46 m) collapse to one point
  expect_true(all(back$status == "placed" |
                    (back$status == "collapsed" &
                       back$line_id %in% out$id[out$length_km < 0.05])))
  expect_true(8896344 %in% back$id)
})


test_that("translate_flowlines covers a flowline with target pieces", {
  nets <- local_translate_mocks()
  on.exit(hydrogeofetch_data_dir(attr(nets, "old_dir")), add = TRUE)

  out <- translate_flowlines(8894344, "nhdpv2", "3dhp")

  expect_s3_class(out, "sf")
  expect_equal(names(out), c("line_id", "segment_order", "id",
                             "id_from_measure", "id_to_measure",
                             "line_from_measure", "line_to_measure",
                             "length_km", "from_offset", "to_offset",
                             "status", "measured_on", "geometry"))
  expect_true(all(out$status == "placed"))
  expect_equal(out$segment_order, seq_len(nrow(out)))

  # the upstream end snaps to a 3DHP flowline boundary; no sliver piece on
  # the flowline above it
  expect_true(all(out$length_km > 0.001))

  nh_3dhp <- nets[["3dhp"]]$id3dhp[nets[["3dhp"]]$mainstemid == ms(2408056)]
  expect_true(all(out$id %in% nh_3dhp))

  src_km <- nets$nhdpv2$lengthkm[nets$nhdpv2$comid == 8894344]
  expect_lt(abs(sum(out$length_km) - src_km) / src_km, 0.1)

  expect_equal(out$line_from_measure[1], 0)
  expect_equal(out$line_to_measure[nrow(out)], 100)

  # and back, from a 3DHP id to V2
  back <- translate_flowlines(out$id[1], "3dhp", "nhdpv2")

  expect_true(all(back$status == "placed"))
  expect_true(all(back$id %in% nets$nhdpv2$comid[
    nets$nhdpv2$mainstemid == ms(2408056)]))
  expect_equal(unique(back$measured_on), "nhdpv2 ref_rivers v3.2")
})

test_that("translate_flowlines reports flowlines it cannot place", {
  nets <- local_translate_mocks()
  on.exit(hydrogeofetch_data_dir(attr(nets, "old_dir")), add = TRUE)

  out <- translate_flowlines(c(1, 8894344), "nhdpv2", "3dhp",
                             search_radius = units::set_units(1, "mm"))

  expect_equal(out$line_id, c(1, 8894344))
  expect_equal(out$status, c("no_mainstem", "not_placed"))
  expect_true(all(is.na(out$id)))

  local_mocked_bindings(get_mainstem_flowlines = function(mainstems, network,
                                                          ...) {
    if(network == "3dhp") NULL else nets[[network]]
  })

  out <- translate_flowlines(8894344, "nhdpv2", "3dhp")

  expect_equal(out$status, "mainstem_not_in_target")
  expect_false(inherits(out, "sf"))
})
