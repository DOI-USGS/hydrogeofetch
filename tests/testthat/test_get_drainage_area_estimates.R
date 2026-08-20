test_that("find_immediate_huc12_outlets with synthetic network", {
  # Linear: 1 -> 2 -> 3 -> 4 -> 5 (outlet)
  # HUC12 outlets at 2 and 4.
  # Trimmed (remove {2,4}): {1,3,5} with toids {2,4,0}.
  # navigate_network_dfs up from 5 in trimmed: nothing flows to 5
  # -> reachable = {5}.
  # comid 4: toid=5 == start -> immediate.
  # comid 2: toid=3, 3 not in {5} -> NOT immediate (blocked by 4).
  net <- data.frame(
    comid = c(1, 2, 3, 4, 5),
    toid = c(2, 3, 4, 5, 0),
    areasqkm = rep(10, 5),
    totdasqkm = c(50, 40, 30, 20, 10)
  )

  result <- suppressWarnings(hydrogeofetch:::find_immediate_huc12_outlets(
    net, 5, c(2, 4)
  ))
  expect_equal(result, 4)

  # Branching: 1, 2, 3 all flow to 4, 4 flows to 5 (outlet)
  # HUC12 outlets at 1 and 2.
  # Trimmed (remove {1,2}): {3,4,5} with toids {4,5,0}.
  # navigate up from 5: 4->5, 3->4. reachable = {5,4,3}.
  # comid 1: toid=4, 4 in reachable -> immediate.
  # comid 2: toid=4, 4 in reachable -> immediate.
  net2 <- data.frame(
    comid = c(1, 2, 3, 4, 5),
    toid = c(4, 4, 4, 5, 0)
  )

  result2 <- suppressWarnings(hydrogeofetch:::find_immediate_huc12_outlets(
    net2, 5, c(1, 2)
  ))
  expect_equal(sort(result2), c(1, 2))

  # Linear: 1 -> 2 -> 3 -> 4 (outlet)
  # HUC12 outlets at 1 and 3. 1 is behind 3.
  # Trimmed (remove {1,3}): {2,4} with toids {3,0}.
  # navigate up from 4: nothing flows to 4 -> reachable = {4}.
  # comid 3: toid=4 == start -> immediate.
  # comid 1: toid=2, 2 not in {4} -> NOT immediate (blocked by 3).
  net3 <- data.frame(
    comid = c(1, 2, 3, 4),
    toid = c(2, 3, 4, 0)
  )

  result3 <- suppressWarnings(hydrogeofetch:::find_immediate_huc12_outlets(
    net3, 4, c(1, 3)
  ))
  expect_equal(result3, 3)
})

test_that("add_huc12_area_columns", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(
      c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)
    ))),
    crs = 5070
  )
  hu12 <- sf::st_sf(ncontrb_a = 100, geometry = poly)

  result <- hydrogeofetch:::add_huc12_area_columns(hu12)

  expect_true("dasqkm" %in% names(result))
  expect_true("ncontrb_sqkm" %in% names(result))
  expect_true("contrib_sqkm" %in% names(result))
  expect_true(result$dasqkm > 0)
  expect_equal(result$ncontrb_sqkm, 100 * 0.00404686)
  expect_equal(result$contrib_sqkm, result$dasqkm - result$ncontrb_sqkm)
})

test_that("plan_upstream_huc12_fetches single HUC10", {
  # All IDs in one HUC10 -> HUC10 and HUC08 estimates reuse HUC12-only
  ids <- c("070702010101", "070702010102", "070702010103")
  outlet <- "070702010101"

  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(ids, outlet)

  expect_equal(plan$huc12_est$fetch_ids, ids)
  expect_true(plan$huc10_est$same_as_huc12)
  expect_equal(plan$huc10_est$bulk_huc10s, character(0))
  expect_null(plan$huc10_est$outlet_backfill)
  expect_true(plan$huc08_est$same_as_huc10)
  expect_equal(plan$huc08_est$bulk_huc08s, character(0))
  expect_null(plan$huc08_est$outlet_backfill)
})

test_that("plan_upstream_huc12_fetches multi-HUC10 single-HUC08", {
  # Two HUC10s in the same HUC08 (07070201)
  ids <- c("070702010101", "070702010102", "070702010501")
  outlet <- "070702010101"

  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(ids, outlet)

  # HUC10 estimate: one upstream HUC10, one local HUC12 in outlet HUC10
  expect_false(plan$huc10_est$same_as_huc12)
  expect_equal(plan$huc10_est$bulk_huc10s, "0707020105")
  expect_equal(plan$huc10_est$local_huc12_ids, "070702010102")

  # outlet_backfill present for multi-HUC10
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc10s, "0707020101")
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc12_ids, "070702010101")

  # HUC08 estimate: single HUC08 -> same as HUC10
  expect_true(plan$huc08_est$same_as_huc10)
})

test_that("plan_upstream_huc12_fetches multi-HUC08", {
  ids <- c(
    "070702010101", "070702010102",  # outlet HUC08 07070201, outlet HUC10
    "070703020301",                   # different HUC08 07070302
    "070801010101"                    # different HUC08 07080101
  )
  outlet <- "070702010101"

  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(ids, outlet)

  # HUC10 estimate: 2 upstream HUC10s
  expect_false(plan$huc10_est$same_as_huc12)
  expect_equal(sort(plan$huc10_est$bulk_huc10s),
    sort(c("0707030203", "0708010101")))
  expect_equal(plan$huc10_est$local_huc12_ids, "070702010102")

  # outlet_backfill for HUC10 estimate
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc10s, "0707020101")
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc12_ids, "070702010101")

  # HUC08 estimate: two upstream HUC08s
  expect_false(plan$huc08_est$same_as_huc10)
  expect_equal(sort(plan$huc08_est$bulk_huc08s),
    sort(c("07070302", "07080101")))
  # local HUC12s = huc12pp in outlet HUC08 (excluding outlet HUC12)
  expect_equal(plan$huc08_est$local_huc12_ids, "070702010102")

  # outlet_backfill for HUC08 estimate
  expect_equal(plan$huc08_est$outlet_backfill$outlet_huc10s, "0707020101")
  expect_equal(plan$huc08_est$outlet_backfill$outlet_huc12_ids, "070702010101")
})

test_that("plan_upstream_huc12_fetches single ID equals outlet", {
  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(
    "070702010101", "070702010101"
  )

  expect_equal(plan$huc12_est$fetch_ids, "070702010101")
  expect_true(plan$huc10_est$same_as_huc12)
  expect_null(plan$huc10_est$outlet_backfill)
  expect_true(plan$huc08_est$same_as_huc10)
  expect_null(plan$huc08_est$outlet_backfill)
})

test_that("plan_upstream_huc12_fetches multiple outlets same HUC10", {
  ids <- c("070702010101", "070702010102", "070702010103", "070702010501")
  outlets <- c("070702010101", "070702010102")

  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(ids, outlets)

  expect_equal(plan$huc12_est$fetch_ids, ids)
  # Both outlets in HUC10 0707020101. One upstream HUC10 0707020105.
  expect_false(plan$huc10_est$same_as_huc12)
  expect_equal(plan$huc10_est$bulk_huc10s, "0707020105")
  expect_equal(plan$huc10_est$local_huc12_ids, "070702010103")

  # outlet_backfill has both outlets
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc10s, "0707020101")
  expect_equal(sort(plan$huc10_est$outlet_backfill$outlet_huc12_ids),
    sort(c("070702010101", "070702010102")))
})

test_that("plan_upstream_huc12_fetches multiple outlets different HUC10s", {
  ids <- c("070702010101", "070702010501", "070702010502", "070703020301")
  outlets <- c("070702010101", "070702010501")

  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(ids, outlets)

  # outlet HUC10s: 0707020101, 0707020105
  # upstream HUC10: 0707030203
  expect_false(plan$huc10_est$same_as_huc12)
  expect_equal(plan$huc10_est$bulk_huc10s, "0707030203")
  expect_equal(plan$huc10_est$local_huc12_ids, "070702010502")

  # outlet_backfill has both outlet HUC10s
  expect_equal(sort(plan$huc10_est$outlet_backfill$outlet_huc10s),
    sort(c("0707020101", "0707020105")))
  expect_equal(sort(plan$huc10_est$outlet_backfill$outlet_huc12_ids),
    sort(c("070702010101", "070702010501")))
})

test_that("plan_upstream_huc12_fetches multiple outlets different HUC08s", {
  ids <- c("070702010101", "070703020301", "070703020302", "070801010101")
  outlets <- c("070702010101", "070703020301")

  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(ids, outlets)

  # outlet HUC08s: 07070201, 07070302
  # upstream HUC08: 07080101
  expect_false(plan$huc08_est$same_as_huc10)
  expect_equal(plan$huc08_est$bulk_huc08s, "07080101")
  expect_equal(plan$huc08_est$local_huc12_ids, "070703020302")

  # outlet_backfill for HUC08 estimate
  expect_equal(sort(plan$huc08_est$outlet_backfill$outlet_huc10s),
    sort(c("0707020101", "0707030203")))
  expect_equal(sort(plan$huc08_est$outlet_backfill$outlet_huc12_ids),
    sort(c("070702010101", "070703020301")))
})

test_that("plan_upstream_huc12_fetches multiple outlets all same HUC10", {
  ids <- c("070702010101", "070702010102", "070702010103")
  outlets <- c("070702010101", "070702010102")

  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(ids, outlets)

  expect_true(plan$huc10_est$same_as_huc12)
  expect_null(plan$huc10_est$outlet_backfill)
  expect_true(plan$huc08_est$same_as_huc10)
  expect_null(plan$huc08_est$outlet_backfill)
})

test_that("plan_upstream_huc12_fetches backfill for missing huc12pp", {
  # Scenario matching the real edge case: outlet HUC12 171200010710.
  # HUC12 171200010707 is in the same HUC10 (1712000107) and upstream
  # by sort order, but has no huc12pp pour point.
  # The plan should carry outlet_backfill info so fetch_upstream_huc12s

  # can query all HUC12s in HUC10 1712000107 and filter by sort order.
  ids <- c("171200010701", "171200010703", "171200010710",
    "171200010501")
  outlet <- "171200010710"

  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(ids, outlet)

  # multi-HUC10: HUC10s 1712000107 (outlet) and 1712000105
  expect_false(plan$huc10_est$same_as_huc12)
  expect_equal(plan$huc10_est$bulk_huc10s, "1712000105")

  # outlet_backfill present with correct values
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc10s, "1712000107")
  expect_equal(plan$huc10_est$outlet_backfill$outlet_huc12_ids, "171200010710")
})

test_that("union_huc12_sets deduplicates and adds missing", {
  poly1 <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    crs = 5070
  )
  poly2 <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(1, 0), c(2, 0), c(2, 1), c(1, 1), c(1, 0)))),
    crs = 5070
  )
  poly3 <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(2, 0), c(3, 0), c(3, 1), c(2, 1), c(2, 0)))),
    crs = 5070
  )

  target <- sf::st_sf(huc_12 = c("A", "B"), geometry = c(poly1, poly2))
  base <- sf::st_sf(huc_12 = c("B", "C"), geometry = c(poly2, poly3))

  result <- hydrogeofetch:::union_huc12_sets(target, base)
  expect_equal(nrow(result), 3)
  expect_equal(sort(result$huc_12), c("A", "B", "C"))

  # NULL/empty base returns target unchanged
  expect_equal(nrow(hydrogeofetch:::union_huc12_sets(target, NULL)), 2)
  empty <- sf::st_sf(geometry = sf::st_sfc(crs = 5070))
  expect_equal(nrow(hydrogeofetch:::union_huc12_sets(target, empty)), 2)

  # NULL/empty target returns base
  expect_equal(nrow(hydrogeofetch:::union_huc12_sets(NULL, base)), 2)
  expect_equal(nrow(hydrogeofetch:::union_huc12_sets(empty, base)), 2)
})

test_that("valid_split_catchment rejects degraded service responses", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    crs = 5070
  )

  good <- sf::st_sf(id = c("catchment", "splitCatchment"),
                    geometry = c(poly, poly))
  expect_true(hydrogeofetch:::valid_split_catchment(good))

  # get_split_catchment returns NULL when the processing service fails
  expect_false(hydrogeofetch:::valid_split_catchment(NULL))

  # service returned features but not the expected pair
  expect_false(hydrogeofetch:::valid_split_catchment(
    sf::st_sf(id = "catchment", geometry = poly)))
  expect_false(hydrogeofetch:::valid_split_catchment(
    sf::st_sf(other = "x", geometry = poly)))
  expect_false(hydrogeofetch:::valid_split_catchment(
    sf::st_sf(geometry = sf::st_sfc(crs = 5070))))
})

test_that("compute_gap_area degrades when split catchment is unavailable", {
  # 1 -> 2 -> 3 (outlet) on one levelpath, HUC12 outlet at comid 1.
  # Nothing is upstream of comid 1, so the gap is comids 2 and 3.
  all_net <- data.frame(
    comid = c(1, 2, 3),
    toid = c(2, 3, 0),
    areasqkm = rep(10, 3),
    levelpathi = rep(1, 3),
    hydroseq = c(3, 2, 1),
    dnhydroseq = c(2, 1, 0),
    dnminorhyd = rep(0, 3)
  )

  pt <- sf::st_sfc(sf::st_point(c(-89.5, 43)), crs = 4326)
  outlet_huc <- sf::st_sf(comid = 1, identifier = "070900020101",
                          geometry = pt)

  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    crs = 5070
  )
  gap_catchments <- sf::st_sf(comid = c(2, 3), geometry = c(poly, poly))

  # empty list -> lookup yields NULL, standing in for a failed service call
  expect_warning(
    result <- hydrogeofetch:::compute_gap_area(
      outlet_huc, all_net,
      split_catchments = list(),
      gap_catchments = gap_catchments
    ),
    "No split catchment available for HUC12 outlet COMID 1"
  )

  # gap area still computed from the extra network, local area omitted
  expect_equal(result$extra_and_local, 20)
  expect_null(result$split_catchment)
  expect_s3_class(result$extra_catchments, "sf")
})

test_that("dissolve_nhdplusv2_basin unions catchments and trims the outlet", {
  # three 1000 m squares in a row: comids 1 and 2 upstream, 3 the outlet.
  # Each is 1 sq km, so the full union is 3 sq km.
  sq <- function(x) {
    sf::st_polygon(list(rbind(c(x, 0), c(x + 1000, 0), c(x + 1000, 1000),
                              c(x, 1000), c(x, 0))))
  }

  catch <- sf::st_sf(
    featureid = c(1, 2, 3),
    geometry = sf::st_sfc(sq(0), sq(1000), sq(2000), crs = 5070)
  )

  all_net <- data.frame(comid = c(1, 2, 3), toid = c(2, 3, 0))

  # split keeps the upstream half of the outlet catchment: 0.5 sq km
  half <- sf::st_polygon(list(rbind(c(2000, 0), c(2500, 0), c(2500, 1000),
                                    c(2000, 1000), c(2000, 0))))
  split <- sf::st_sf(
    id = c("catchment", "splitCatchment"),
    geometry = sf::st_sfc(sq(2000), half, crs = 5070)
  )

  basin <- hydrogeofetch:::dissolve_nhdplusv2_basin(all_net, 3, catch, split)

  expect_s3_class(basin, "sf")
  expect_equal(nrow(basin), 1)
  # dissolve_polygons snaps nodes, costing about a square meter on these
  # squares, so areas are compared with a tolerance well inside any real error
  expect_equal(basin$dasqkm, 2.5, tolerance = 1e-5)
  expect_equal(sf::st_crs(basin), sf::st_crs(5070))

  # no split -> the full outlet catchment stays in
  expect_equal(
    hydrogeofetch:::dissolve_nhdplusv2_basin(all_net, 3, catch, NULL)$dasqkm, 3,
    tolerance = 1e-5)

  # headwater: the split catchment is the whole basin by construction
  expect_equal(
    hydrogeofetch:::dissolve_nhdplusv2_basin(
      data.frame(comid = 3, toid = 0), 3, catch[3, ], split)$dasqkm, 0.5,
    tolerance = 1e-5)

  # several outlet comids and no split, as for a waterbody start
  expect_equal(
    hydrogeofetch:::dissolve_nhdplusv2_basin(all_net, c(2, 3), catch,
                                             NULL)$dasqkm, 3,
    tolerance = 1e-5)

  # comid stands in for featureid
  expect_equal(
    hydrogeofetch:::dissolve_nhdplusv2_basin(
      all_net, 3, dplyr::rename(catch, comid = "featureid"), split)$dasqkm, 2.5,
    tolerance = 1e-5)
})

test_that("dissolve_nhdplusv2_basin refuses a due split that is missing", {
  sq <- function(x) {
    sf::st_polygon(list(rbind(c(x, 0), c(x + 1000, 0), c(x + 1000, 1000),
                              c(x, 1000), c(x, 0))))
  }
  catch <- sf::st_sf(
    featureid = c(1, 2, 3),
    geometry = sf::st_sfc(sq(0), sq(1000), sq(2000), crs = 5070)
  )
  all_net <- data.frame(comid = c(1, 2, 3), toid = c(2, 3, 0))

  # no split was due, so the whole outlet catchment belongs in the basin
  expect_equal(
    hydrogeofetch:::dissolve_nhdplusv2_basin(
      all_net, 3, catch, NULL,
      list(threshold_exceeded = FALSE))$dasqkm, 3, tolerance = 1e-5)

  # a split was due and did not come back: keeping the outlet catchment would
  # report below-point area as basin, so there is no basin to report
  expect_null(hydrogeofetch:::dissolve_nhdplusv2_basin(
    all_net, 3, catch, NULL, list(threshold_exceeded = TRUE)))

  # same for a response the service degraded to a single catchment row
  expect_null(hydrogeofetch:::dissolve_nhdplusv2_basin(
    all_net, 3, catch,
    sf::st_sf(id = "catchment", geometry = sf::st_sfc(sq(2000), crs = 5070)),
    list(threshold_exceeded = TRUE)))

  # a due split that arrived is trimmed as usual
  half <- sf::st_polygon(list(rbind(c(2000, 0), c(2500, 0), c(2500, 1000),
                                    c(2000, 1000), c(2000, 0))))
  split <- sf::st_sf(
    id = c("catchment", "splitCatchment"),
    geometry = sf::st_sfc(sq(2000), half, crs = 5070)
  )
  expect_equal(
    hydrogeofetch:::dissolve_nhdplusv2_basin(
      all_net, 3, catch, split, list(threshold_exceeded = TRUE))$dasqkm, 2.5,
    tolerance = 1e-5)

  # a split covers one outlet catchment. Substituting it while dropping both
  # outlets would silently lose comid 2, reporting 1.5 where 2.5 is right.
  expect_null(hydrogeofetch:::dissolve_nhdplusv2_basin(
    all_net, c(2, 3), catch, split, list(threshold_exceeded = TRUE)))

  # with no split there is nothing to substitute, so both outlets are kept
  expect_equal(
    hydrogeofetch:::dissolve_nhdplusv2_basin(
      all_net, c(2, 3), catch, NULL,
      list(threshold_exceeded = FALSE))$dasqkm, 3, tolerance = 1e-5)
})

test_that("dissolve_nhdplusv2_basin returns NULL without full coverage", {
  poly <- sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(1, 1), c(0, 1), c(0, 0)))),
    crs = 5070
  )
  all_net <- data.frame(comid = c(1, 2, 3), toid = c(2, 3, 0))

  # gap-zone catchments only, as when catchments = FALSE
  expect_null(hydrogeofetch:::dissolve_nhdplusv2_basin(
    all_net, 3, sf::st_sf(featureid = c(2, 3), geometry = c(poly, poly)), NULL))

  expect_null(hydrogeofetch:::dissolve_nhdplusv2_basin(all_net, 3, NULL, NULL))
  expect_null(hydrogeofetch:::dissolve_nhdplusv2_basin(
    all_net, 3, sf::st_sf(geometry = sf::st_sfc(crs = 5070)), NULL))

  # no usable id column
  expect_null(hydrogeofetch:::dissolve_nhdplusv2_basin(
    all_net, 3, sf::st_sf(other = 1, geometry = poly), NULL))
})

test_that("headwater basin area drives the DA estimates", {
  # in a headwater there are no upstream HUC12s, so the estimates come
  # entirely from the basin area substituted into extra_and_local
  hu12_result <- list(
    hu12_by_huc12 = sf::st_sf(geometry = sf::st_sfc(crs = 5070)),
    hu12_by_huc10 = NULL,
    hu12_by_huc08 = NULL
  )

  est <- hydrogeofetch:::assemble_da_estimates(hu12_result, 0.5)

  expect_equal(est$da_huc12_sqkm, 0.5)
  expect_equal(est$contrib_da_huc12_sqkm, 0.5)
  expect_true(is.na(est$da_huc10_sqkm))
  expect_true(is.na(est$da_huc08_sqkm))
})

test_that("get_drainage_area_estimates Black Earth Creek smoke test", {
  skip_if_no_integration()

  start <- list(featureSource = "nwissite", featureID = "USGS-05406500")

  result <- suppressWarnings(suppressMessages(
    get_drainage_area_estimates(start)
  ))

  # check return structure
  expect_type(result, "list")
  expected_names <- c(
    "da_huc12_sqkm", "da_huc10_sqkm", "da_huc08_sqkm",
    "contrib_da_huc12_sqkm", "contrib_da_huc10_sqkm",
    "contrib_da_huc08_sqkm",
    "network_da_sqkm", "start_feature",
    "hu12_by_huc12", "hu12_by_huc10", "hu12_by_huc08",
    "extra_catchments", "split_catchment",
    "all_network", "all_catchments", "hu12_outlet"
  )
  expect_true(all(expected_names %in% names(result)))

  # catchments should be NULL when not requested
  expect_null(result$all_catchments)

  # HUC12-level DA should be positive and close to network DA
  expect_true(result$da_huc12_sqkm > 0)
  expect_true(result$network_da_sqkm > 0)
  expect_true(result$contrib_da_huc12_sqkm > 0)
  expect_true(result$contrib_da_huc12_sqkm <= result$da_huc12_sqkm)

  # single HUC10 basin so huc10 and huc08 estimates should be NA
  expect_true(is.na(result$da_huc10_sqkm))
  expect_true(is.na(result$da_huc08_sqkm))
  expect_true(is.na(result$contrib_da_huc10_sqkm))
  expect_true(is.na(result$contrib_da_huc08_sqkm))
  expect_null(result$hu12_by_huc10)
  expect_null(result$hu12_by_huc08)

  # spatial outputs should be sf
  expect_s3_class(result$hu12_by_huc12, "sf")
  expect_s3_class(result$extra_catchments, "sf")
  expect_s3_class(result$split_catchment, "sf")
})

test_that("resolve_comid_from_reachcode picks flowline by measure interval", {
  vaa <- data.frame(
    comid = c(101L, 102L, 103L, 999L),
    reachcode = c("14070003001234", "14070003001234",
      "14070003001234", "14070003009999"),
    frommeas = c(0, 50, 80, 0),
    tomeas = c(50, 80, 100, 100)
  )

  # measure in second interval
  expect_equal(
    hydrogeofetch:::resolve_comid_from_reachcode("14070003001234", 65, vaa),
    102L
  )
  # measure on boundary -> first matching interval
  expect_equal(
    hydrogeofetch:::resolve_comid_from_reachcode("14070003001234", 50, vaa),
    101L
  )
  # measure out of bounds (>100) -> closest interval boundary (comid 103, tomeas=100)
  expect_equal(
    hydrogeofetch:::resolve_comid_from_reachcode("14070003001234", 110, vaa),
    103L
  )
  # missing reachcode -> error
  expect_error(
    hydrogeofetch:::resolve_comid_from_reachcode("00000000000000", 50, vaa),
    "No flowlines found in VAA"
  )
})

test_that("get_drainage_area_estimates accepts reachcode/measure in start", {
  skip_if_no_integration()

  # source the actual reachcode/measure from a one-time NLDI call
  ref <- get_nldi_feature(
    list(featureSource = "nwissite", featureID = "USGS-05406500")
  )
  ref_measure <- as.numeric(ref$measure)

  # 1. override path: NLDI still called for COMID, reachcode/measure overridden
  override_measure <- ref_measure - 5
  start_ovr <- list(
    featureSource = "nwissite", featureID = "USGS-05406500",
    reachcode = ref$reachcode, measure = override_measure
  )
  res_ovr <- suppressWarnings(suppressMessages(
    get_drainage_area_estimates(start_ovr, nhdplushr = FALSE)
  ))
  expect_equal(res_ovr$start_feature$measure, override_measure)
  expect_equal(res_ovr$start_feature$reachcode, ref$reachcode)
  expect_true(res_ovr$da_huc12_sqkm > 0)
  expect_true(res_ovr$network_da_sqkm > 0)

  # 2. no-NLDI path: only reachcode/measure provided; COMID resolved via OGC,
  # start point derived from the flowline at the supplied measure
  start_no_nldi <- list(
    reachcode = ref$reachcode, measure = ref_measure
  )
  res_no_nldi <- suppressWarnings(suppressMessages(
    get_drainage_area_estimates(start_no_nldi, nhdplushr = FALSE)
  ))
  expect_equal(as.integer(res_no_nldi$start_feature$comid),
    as.integer(ref$comid))
  expect_equal(res_no_nldi$start_feature$reachcode, ref$reachcode)
  expect_equal(res_no_nldi$start_feature$measure, ref_measure)
  expect_true(res_no_nldi$da_huc12_sqkm > 0)
  expect_equal(res_no_nldi$network_da_sqkm, res_ovr$network_da_sqkm)

  # derived start point should be a real POINT near the NLDI-reported location
  geom <- sf::st_geometry(res_no_nldi$start_feature)
  expect_s3_class(geom, "sfc_POINT")
  expect_false(sf::st_is_empty(geom)[1])
  dist_m <- as.numeric(sf::st_distance(
    sf::st_transform(geom, 5070),
    sf::st_transform(sf::st_geometry(ref), 5070)
  ))
  expect_lt(dist_m, 500)
})

test_that("get_drainage_area_estimates local_navigation smoke test", {
  skip_if_no_integration()

  start <- list(featureSource = "nwissite", featureID = "USGS-05406500")

  result <- suppressWarnings(suppressMessages(
    get_drainage_area_estimates(start, local_navigation = TRUE)
  ))

  # check return structure
  expect_type(result, "list")
  expected_names <- c(
    "da_huc12_sqkm", "da_huc10_sqkm", "da_huc08_sqkm",
    "contrib_da_huc12_sqkm", "contrib_da_huc10_sqkm",
    "contrib_da_huc08_sqkm",
    "network_da_sqkm", "start_feature",
    "hu12_by_huc12", "hu12_by_huc10", "hu12_by_huc08",
    "extra_catchments", "split_catchment",
    "all_network", "all_catchments", "hu12_outlet"
  )
  expect_true(all(expected_names %in% names(result)))

  # HUC12-level DA should be positive and close to network DA
  expect_true(result$da_huc12_sqkm > 0)
  expect_true(result$network_da_sqkm > 0)
  expect_true(result$contrib_da_huc12_sqkm > 0)
  expect_true(result$contrib_da_huc12_sqkm <= result$da_huc12_sqkm)

  # single HUC10 basin so huc10 and huc08 estimates should be NA
  expect_true(is.na(result$da_huc10_sqkm))
  expect_true(is.na(result$da_huc08_sqkm))

  # spatial outputs should be sf
  expect_s3_class(result$hu12_by_huc12, "sf")
  expect_s3_class(result$extra_catchments, "sf")
  expect_s3_class(result$split_catchment, "sf")
})

test_that("get_drainage_area_estimates Lake Mendota multi-outlet smoke test", {
  skip_if_no_integration()

  start <- list(featureSource = "nwissite", featureID = "USGS-05428000")

  result <- suppressWarnings(suppressMessages(
    get_drainage_area_estimates(start)
  ))

  # check return structure
  expect_type(result, "list")
  expected_names <- c(
    "da_huc12_sqkm", "da_huc10_sqkm", "da_huc08_sqkm",
    "contrib_da_huc12_sqkm", "contrib_da_huc10_sqkm",
    "contrib_da_huc08_sqkm",
    "network_da_sqkm", "start_feature",
    "hu12_by_huc12", "hu12_by_huc10", "hu12_by_huc08",
    "extra_catchments", "split_catchment",
    "all_network", "all_catchments", "hu12_outlet"
  )
  expect_true(all(expected_names %in% names(result)))

  # DA should be positive
  expect_true(result$da_huc12_sqkm > 0)
  expect_true(result$network_da_sqkm > 0)
  expect_true(result$contrib_da_huc12_sqkm > 0)
  expect_true(result$contrib_da_huc12_sqkm <= result$da_huc12_sqkm)

  # split_catchment should have rows for multiple outlets (2 rows each)
  expect_true(nrow(result$split_catchment) >= 4)

  # superset property: HUC08 >= HUC10 >= HUC12
  if(!is.na(result$da_huc10_sqkm))
    expect_true(result$da_huc10_sqkm >= result$da_huc12_sqkm)
  if(!is.na(result$da_huc08_sqkm))
    expect_true(result$da_huc08_sqkm >= result$da_huc10_sqkm)

  # spatial outputs should be sf
  expect_s3_class(result$hu12_by_huc12, "sf")
  expect_s3_class(result$split_catchment, "sf")
})

test_that("assemble_hu12_sets with synthetic polygons (HUC08 active)", {
  # Build a minimal plan and polygon set to exercise the assembly logic
  # without any web calls.
  make_poly <- function(id, x0 = 0) {
    sf::st_sf(
      huc_12 = id,
      ncontrb_a = 0,
      geometry = sf::st_sfc(
        sf::st_polygon(list(rbind(
          c(x0, 0), c(x0 + 1000, 0),
          c(x0 + 1000, 1000), c(x0, 1000), c(x0, 0)
        ))),
        crs = 5070
      )
    )
  }

  # Simulate 3 HUC12s: h12_outlet in outlet HUC10 0707020101 (HUC08 07070201),
  # h12_up1 in upstream HUC10 0707020105 (HUC08 07070201),
  # h12_up2 in upstream HUC10 0707030203 (HUC08 07070302)
  h12_outlet <- make_poly("070702010101", 0)
  h12_up1 <- make_poly("070702010501", 1000)
  h12_up2 <- make_poly("070703020301", 2000)

  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(
    all_huc12_ids = c("070702010101", "070702010501", "070703020301"),
    outlet_huc12_ids = "070702010101"
  )

  polygons <- list(
    by_id = dplyr::bind_rows(h12_outlet, h12_up1, h12_up2),
    backfill_raw = sf::st_sf(geometry = sf::st_sfc(crs = 5070)),
    huc08_bulk = h12_up2,
    huc10_extra = h12_up1,
    huc10_bulk = NULL
  )

  result <- hydrogeofetch:::assemble_hu12_sets(plan, polygons)

  expect_type(result, "list")
  expect_s3_class(result$hu12_by_huc12, "sf")
  expect_true(nrow(result$hu12_by_huc12) == 3)
  expect_true("dasqkm" %in% names(result$hu12_by_huc12))
  expect_true("contrib_sqkm" %in% names(result$hu12_by_huc12))
  expect_true(all(result$hu12_by_huc12$dasqkm > 0))

  # HUC10 and HUC08 active
  expect_s3_class(result$hu12_by_huc10, "sf")
  expect_s3_class(result$hu12_by_huc08, "sf")

  # superset property: HUC08 >= HUC10 >= HUC12
  expect_true(
    nrow(result$hu12_by_huc10) >= nrow(result$hu12_by_huc12)
  )
  expect_true(
    nrow(result$hu12_by_huc08) >= nrow(result$hu12_by_huc10)
  )
})

test_that("assemble_hu12_sets single HUC10 returns NULL for broader", {
  make_poly <- function(id, x0 = 0) {
    sf::st_sf(
      huc_12 = id,
      ncontrb_a = 0,
      geometry = sf::st_sfc(
        sf::st_polygon(list(rbind(
          c(x0, 0), c(x0 + 1000, 0),
          c(x0 + 1000, 1000), c(x0, 1000), c(x0, 0)
        ))),
        crs = 5070
      )
    )
  }

  plan <- hydrogeofetch:::plan_upstream_huc12_fetches(
    all_huc12_ids = c("070702010101", "070702010102"),
    outlet_huc12_ids = "070702010101"
  )

  polygons <- list(
    by_id = dplyr::bind_rows(
      make_poly("070702010101", 0),
      make_poly("070702010102", 1000)
    ),
    backfill_raw = sf::st_sf(geometry = sf::st_sfc(crs = 5070)),
    huc08_bulk = NULL,
    huc10_extra = NULL,
    huc10_bulk = NULL
  )

  result <- hydrogeofetch:::assemble_hu12_sets(plan, polygons)

  expect_true(nrow(result$hu12_by_huc12) == 2)
  expect_null(result$hu12_by_huc10)
  expect_null(result$hu12_by_huc08)
})

test_that("assemble_da_estimates", {
  # Build a minimal hu12_result
  make_poly <- function(id, x0 = 0) {
    sf::st_sf(
      huc_12 = id,
      ncontrb_a = 0,
      geometry = sf::st_sfc(
        sf::st_polygon(list(rbind(
          c(x0, 0), c(x0 + 1000, 0),
          c(x0 + 1000, 1000), c(x0, 1000), c(x0, 0)
        ))),
        crs = 5070
      )
    )
  }
  hu12 <- dplyr::bind_rows(
    make_poly("A", 0), make_poly("B", 1000)
  )
  hu12 <- hydrogeofetch:::add_huc12_area_columns(hu12)

  hu12_result <- list(
    hu12_by_huc12 = hu12,
    hu12_by_huc10 = NULL,
    hu12_by_huc08 = NULL
  )

  est <- hydrogeofetch:::assemble_da_estimates(hu12_result, 5.0)

  expect_true(est$da_huc12_sqkm > 0)
  expect_equal(est$da_huc12_sqkm, sum(hu12$dasqkm) + 5.0)
  expect_true(is.na(est$da_huc10_sqkm))
  expect_true(is.na(est$da_huc08_sqkm))
  expect_true(is.na(est$contrib_da_huc10_sqkm))
  expect_true(is.na(est$contrib_da_huc08_sqkm))
})

test_that("filter_disconnected_huc12s", {
  # HUC08 12050001: outlet HUC12 (120500011305) is 
  # on-network -> keep HUC12s including disconnected headwaters.
  broader_12050001 <- c(
    "120500010101", "120500010102", "120500010103", "120500010104",
    "120500011305"
  )
  net_12050001 <- c(
    "120500011304", "120500011305"
  )
  # HUC08 grouping: outlet 120500011305 (max in HUC08) is on-network
  # -> keep all HUC12s.
  result <- hydrogeofetch:::filter_disconnected_huc12s(
    broader_12050001, net_12050001, parent_nchar = 8L
  )
  expect_equal(sort(result), sort(broader_12050001))

  # HUC08 12040205: outlet 120402050400 (max in HUC08) is NOT on-network
  # -> only keep 120402050100 which is individually on-network.
  result <- hydrogeofetch:::filter_disconnected_huc12s(
    c("120402050100", "120402050200", "120402050300", "120402050400"),
    "120402050100",
    parent_nchar = 8L
  )
  expect_equal(result, "120402050100")
})

# -- negotiate_outlet_catchment ------------------------------------------------

test_that("negotiate_outlet_catchment with single-flowline reachcode", {
  # USGS-08110075: frommeas=0, tomeas=100, lengthkm=4.92
  fix <- readRDS(list.files(pattern = "negotiate_outlet_fixture.rds",
    recursive = TRUE, full.names = TRUE))
  d <- fix[["USGS-08110075"]]

  # threshold high enough that no split is triggered (no web call for geometry)
  result <- suppressMessages(hydrogeofetch:::negotiate_outlet_catchment(
    d$start_info, d$vaa_row, outlet_split_threshold_m = 99999
  ))

  expect_true(is.list(result))
  # frommeas=0, tomeas=100 => rescale_measures(64.8248, 0, 100) = 64.8248
  expect_equal(result$flowline_measure, 64.8248, tolerance = 0.01)
  expect_false(result$threshold_exceeded)
  expect_null(result$gage_point)
})

test_that("negotiate_outlet_catchment with multi-flowline reachcode", {
  # USGS-08109000: frommeas=13.53, tomeas=91.14, measure=71.41, lengthkm=12.062
  fix <- readRDS(list.files(pattern = "negotiate_outlet_fixture.rds",
    recursive = TRUE, full.names = TRUE))
  d <- fix[["USGS-08109000"]]

  result <- suppressMessages(hydrogeofetch:::negotiate_outlet_catchment(
    d$start_info, d$vaa_row, outlet_split_threshold_m = 99999
  ))

  expect_true(is.list(result))
  # rescale_measures(71.4071, 13.53087, 91.14478)
  expected <- (71.4071 - 13.53087) / (91.14478 - 13.53087) * 100
  expect_equal(result$flowline_measure, expected, tolerance = 0.1)
  expect_false(result$threshold_exceeded)
})

test_that("negotiate_outlet_catchment returns NULL for waterbody start", {
  start_feature <- sf::st_sf(
    comid = "99999",
    geometry = sf::st_sfc(sf::st_point(c(-89.0, 43.0)), crs = 4326)
  )
  start_info <- list(
    start_feature = start_feature,
    outlet_comids = 99999L
  )

  expect_null(suppressMessages(
    hydrogeofetch:::negotiate_outlet_catchment(start_info, NULL)
  ))
})

test_that("negotiate_outlet_catchment returns NULL when measure is NA", {
  start_feature <- sf::st_sf(
    comid = "5567571",
    measure = NA_real_,
    reachcode = "12070102000024",
    geometry = sf::st_sfc(sf::st_point(c(-96.69, 30.54)), crs = 4326)
  )
  start_info <- list(
    start_feature = start_feature,
    outlet_comids = 5567571L
  )

  expect_null(suppressMessages(
    hydrogeofetch:::negotiate_outlet_catchment(start_info, NULL)
  ))
})

test_that("negotiate_outlet_catchment returns NULL at outlet", {
  start_feature <- sf::st_sf(
    comid = "5567571",
    measure = 0.5,
    reachcode = "12070102000024",
    geometry = sf::st_sfc(sf::st_point(c(-96.69, 30.54)), crs = 4326)
  )
  start_info <- list(
    start_feature = start_feature,
    outlet_comids = 5567571L
  )
  vaa <- data.frame(
    comid = 5567571L,
    frommeas = 0,
    tomeas = 100,
    reachcode = "12070102000024"
  )

  # rescale_measures(0.5, 0, 100) = 0.5 which is < 1 => NULL
  expect_null(suppressMessages(
    hydrogeofetch:::negotiate_outlet_catchment(start_info, vaa)
  ))
})

test_that("negotiate_outlet_catchment handles out-of-bounds measure", {
  start_feature <- sf::st_sf(
    comid = "12345",
    measure = 10,
    reachcode = "99990001000001",
    geometry = sf::st_sfc(sf::st_point(c(-90.0, 40.0)), crs = 4326)
  )
  start_info <- list(
    start_feature = start_feature,
    outlet_comids = 12345L
  )
  vaa <- data.frame(
    comid = 12345L,
    frommeas = 50,
    tomeas = 100,
    reachcode = "99990001000001"
  )

  # measure 10 is closer to frommeas=50 => flowline_measure=0 => < 1 => NULL
  expect_message(
    result <- hydrogeofetch:::negotiate_outlet_catchment(start_info, vaa),
    "outside flowline bounds"
  )
  expect_null(result)
})

test_that("negotiate_outlet_catchment threshold not exceeded", {
  # USGS-08110075: flowline_measure=64.8248, lengthkm=4.92
  # distance to outlet = 64.8248/100 * 4920 = ~3190 m
  fix <- readRDS(list.files(pattern = "negotiate_outlet_fixture.rds",
    recursive = TRUE, full.names = TRUE))
  d <- fix[["USGS-08110075"]]

  # threshold above distance => not exceeded, no geometry fetch
  result <- suppressMessages(hydrogeofetch:::negotiate_outlet_catchment(
    d$start_info, d$vaa_row, outlet_split_threshold_m = 99999
  ))
  expect_true(is.list(result))
  expect_false(result$threshold_exceeded)
  expect_null(result$gage_point)
  expect_equal(result$flowline_measure, 64.8248, tolerance = 0.01)
})

test_that("negotiate_outlet_catchment threshold exceeded fetches gage_point", {
  skip_if_no_integration()
  # USGS-08110075: flowline_measure=64.8248, lengthkm=4.92
  # distance to outlet = 64.8248/100 * 4920 = ~3190 m
  fix <- readRDS(list.files(pattern = "negotiate_outlet_fixture.rds",
    recursive = TRUE, full.names = TRUE))
  d <- fix[["USGS-08110075"]]

  # threshold well below distance => exceeded, fetches geometry
  result <- suppressMessages(hydrogeofetch:::negotiate_outlet_catchment(
    d$start_info, d$vaa_row, outlet_split_threshold_m = 100
  ))
  expect_true(result$threshold_exceeded)
  expect_true(inherits(result$gage_point, "sfc"))
  expect_equal(result$flowline_measure, 64.8248, tolerance = 0.01)
})

test_that("negotiate_outlet_catchment returns correct structure", {
  fix <- readRDS(list.files(pattern = "negotiate_outlet_fixture.rds",
    recursive = TRUE, full.names = TRUE))
  d <- fix[["USGS-08110075"]]

  # threshold not exceeded — list with NULL gage_point
  result <- suppressMessages(hydrogeofetch:::negotiate_outlet_catchment(
    d$start_info, d$vaa_row, outlet_split_threshold_m = 99999
  ))

  expect_true(is.list(result))
  expect_named(result, c("flowline_measure", "gage_point", "threshold_exceeded"))
  expect_true(is.numeric(result$flowline_measure))
  expect_false(result$threshold_exceeded)
  expect_null(result$gage_point)

  # NULL return for waterbody start
  wb <- list(
    start_feature = sf::st_sf(
      comid = "99999",
      geometry = sf::st_sfc(sf::st_point(c(-89, 43)), crs = 4326)),
    outlet_comids = 99999L)
  expect_null(suppressMessages(
    hydrogeofetch:::negotiate_outlet_catchment(wb, NULL)
  ))
})

test_that("prepare_huc12_outlets renames and validates", {
  pts <- sf::st_sfc(
    sf::st_point(c(-89, 43)), sf::st_point(c(-88, 42)),
    crs = 4326
  )
  raw <- sf::st_sf(
    COMID = c(1001, 1002),
    FinalWBD_HUC12 = c("070702010101", "070702010102"),
    other = c("a", "b"),
    geometry = pts
  )

  out <- hydrogeofetch:::prepare_huc12_outlets(raw)

  expect_true(all(c("comid", "identifier", "other") %in% names(out)))
  expect_type(out$comid, "integer")
  expect_type(out$identifier, "character")
  expect_equal(out$identifier, c("070702010101", "070702010102"))
  expect_true(inherits(out, "sf"))

  # Missing required columns triggers stop
  bad <- sf::st_sf(x = 1, geometry = sf::st_sfc(sf::st_point(c(0, 0)),
    crs = 4326))
  expect_error(hydrogeofetch:::prepare_huc12_outlets(bad),
    "missing required columns")

  # Non-sf, non-character input triggers stop
  expect_error(hydrogeofetch:::prepare_huc12_outlets(data.frame(comid = 1)),
    "sf data.frame or a path")

  # Non-existent path triggers stop
  expect_error(
    hydrogeofetch:::prepare_huc12_outlets("/no/such/file.gpkg"),
    "file not found"
  )
})

test_that("get_drainage_area_estimates waterbody_data Lake Mendota smoke test", {
  skip_if_no_integration()
  source(system.file("extdata/sample_data.R", package = "hydrogeofetch"))

  start <- sf::st_sfc(sf::st_point(c(-89.4, 43.1)), crs = 4326)
  waterbody_data <- sf::read_sf(sample_data, "NHDWaterbody")

  result <- suppressWarnings(suppressMessages(
    get_drainage_area_estimates(start,
      waterbody_data = waterbody_data,
      nhdplushr = FALSE)
  ))

  expect_type(result, "list")
  expect_s3_class(result$start_feature, "sf")
  expect_true(result$da_huc12_sqkm > 0)
  expect_true(result$network_da_sqkm > 0)
  expect_null(result$all_catchments)
})

test_that("get_drainage_area_estimates catchment_data local retrieval", {
  skip_if_no_integration()
  source(system.file("extdata/sample_data.R", package = "hydrogeofetch"))

  start <- sf::st_sfc(sf::st_point(c(-89.4, 43.1)), crs = 4326)
  waterbody_data <- sf::read_sf(sample_data, "NHDWaterbody")
  catchment_data <- sf::read_sf(sample_data, "CatchmentSP")

  result <- suppressWarnings(suppressMessages(
    get_drainage_area_estimates(start,
      waterbody_data = waterbody_data,
      catchment_data = catchment_data,
      catchments = TRUE,
      nhdplushr = FALSE)
  ))

  expect_s3_class(result$all_catchments, "sf")
  expect_true(nrow(result$all_catchments) > 0)
})

test_that("find_start_huc12 with local huc12_data", {
  # two adjacent unit squares in EPSG:4326; HUC12 A on the left, B on the right
  poly_a <- sf::st_polygon(list(rbind(
    c(-100, 40), c(-99, 40), c(-99, 41), c(-100, 41), c(-100, 40)
  )))
  poly_b <- sf::st_polygon(list(rbind(
    c(-99, 40), c(-98, 40), c(-98, 41), c(-99, 41), c(-99, 40)
  )))
  huc12_data <- sf::st_sf(
    huc_12 = c("171200010710", "171200010711"),
    ncontrb_a = c(0, 0),
    geometry = sf::st_sfc(poly_a, poly_b, crs = 4326)
  )

  pt_a <- sf::st_sfc(sf::st_point(c(-99.5, 40.5)), crs = 4326)
  expect_equal(
    hydrogeofetch:::find_start_huc12(pt_a, huc12_data),
    "171200010710"
  )

  pt_b <- sf::st_sfc(sf::st_point(c(-98.5, 40.5)), crs = 4326)
  expect_equal(
    hydrogeofetch:::find_start_huc12(pt_b, huc12_data),
    "171200010711"
  )

  pt_out <- sf::st_sfc(sf::st_point(c(-110, 50)), crs = 4326)
  expect_null(hydrogeofetch:::find_start_huc12(pt_out, huc12_data))
})

test_that("augment_override_for_closed_basin_start adds HUC10 when start is off-network", {
  poly <- sf::st_polygon(list(rbind(
    c(-100, 40), c(-99, 40), c(-99, 41), c(-100, 41), c(-100, 40)
  )))
  huc12_data <- sf::st_sf(
    huc_12 = "171200010710",
    ncontrb_a = 0,
    geometry = sf::st_sfc(poly, crs = 4326)
  )
  pt <- sf::st_sfc(sf::st_point(c(-99.5, 40.5)), crs = 4326)

  # start HUC12 is NOT in the on-network set -> override augmented with HUC10
  override <- suppressMessages(
    hydrogeofetch:::augment_override_for_closed_basin_start(
      pt, network_huc12_ids = c("171200020101", "171200030101"),
      huc12_data = huc12_data, HU_inclusion_override = NULL
    )
  )
  expect_equal(override, "1712000107")

  # existing overrides are preserved, deduplicated
  override2 <- suppressMessages(
    hydrogeofetch:::augment_override_for_closed_basin_start(
      pt, network_huc12_ids = c("171200020101"),
      huc12_data = huc12_data,
      HU_inclusion_override = c("10130106", "1712000107")
    )
  )
  expect_equal(override2, c("1712000107", "10130106"))

  # start HUC12 IS in the on-network set -> override unchanged
  override3 <- suppressMessages(
    hydrogeofetch:::augment_override_for_closed_basin_start(
      pt, network_huc12_ids = c("171200010710", "171200020101"),
      huc12_data = huc12_data, HU_inclusion_override = NULL
    )
  )
  expect_null(override3)

  # non-point start: passthrough
  expect_equal(
    hydrogeofetch:::augment_override_for_closed_basin_start(
      list(featureSource = "x", featureID = "y"),
      network_huc12_ids = character(0),
      huc12_data = huc12_data,
      HU_inclusion_override = "10130106"
    ),
    "10130106"
  )
})

test_that("force_include_override_huc12s adds missing HU12s under override parents", {
  # huc12_data with 4 HU12s spanning two HU10s in HU8 17120001
  poly <- function(xmin, ymin) {
    sf::st_polygon(list(rbind(
      c(xmin, ymin), c(xmin + 0.1, ymin),
      c(xmin + 0.1, ymin + 0.1), c(xmin, ymin + 0.1),
      c(xmin, ymin)
    )))
  }
  huc12_data <- sf::st_sf(
    huc_12 = c("171200010704", "171200010710",
      "171200010501", "171200010504"),
    ncontrb_a = c(0, 200, 0, 0),
    geometry = sf::st_sfc(
      poly(-99.4, 40), poly(-99.3, 40),
      poly(-99.2, 40), poly(-99.1, 40),
      crs = 5070
    )
  )

  # simulate fetch result: by_huc10 has only the on-network 0501, by_huc08
  # has the same single HU12 (mimicking the outlet-HU8 partial-fetch case)
  on_network <- huc12_data[huc12_data$huc_12 == "171200010501", ]
  on_network <- hydrogeofetch:::add_huc12_area_columns(on_network)
  hu12_result <- list(
    hu12_by_huc12 = on_network,
    hu12_by_huc10 = on_network,
    hu12_by_huc08 = on_network
  )

  # 10-char override forces fetch of HU10 1712000107 (2 HU12s) at HU10 level,
  # and HU8 17120001 (all 4 HU12s) at HU08 level via cascade.
  result <- suppressMessages(
    hydrogeofetch:::force_include_override_huc12s(
      hu12_result, "1712000107", huc12_data
    )
  )

  expect_setequal(result$hu12_by_huc10$huc_12,
    c("171200010501", "171200010704", "171200010710"))
  expect_setequal(result$hu12_by_huc08$huc_12,
    c("171200010501", "171200010504", "171200010704", "171200010710"))
  # hu12_by_huc12 is the on-network set; not augmented
  expect_equal(result$hu12_by_huc12$huc_12, "171200010501")
})
