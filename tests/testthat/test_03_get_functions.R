# TESTING UNITS
# ==============================================================================
# area = AOI::aoi_get("Eureka, Montana")
# st_as_text(area$geometry)
area = data.frame(loc = "eurika", geometry =
        "POLYGON ((-115.0631 48.86286, -115.0345 48.86286, -115.0345 48.88706, -115.0631 48.88706, -115.0631 48.86286))") |>
sf::st_as_sf(wkt = "geometry", crs = 4326)

# pt = AOI::geocode("Eureka, Montana", pt = TRUE)
# st_as_text(pt$geometry)
pt = data.frame(loc = "eurika", geometry = "POINT (-115.0535 48.87996)") |>
  sf::st_as_sf(wkt = "geometry", crs = 4326)

pt2 = data.frame(loc = "ucsb", geometry = "POINT (-119.8458 34.4146)") |>
  sf::st_as_sf(wkt = "geometry", crs = 4326)

# ==============================================================================

test_that("query water oafeat...",{
  df <- hydrogeofetch:::query_usgs_oafeat()
  expect_equal(ncol(df), 6)

  expect_error(hydrogeofetch:::query_usgs_oafeat(AOI = pt, type = 'wrong'),
               "Type")
  expect_error(hydrogeofetch:::query_usgs_oafeat(AOI = NULL, id = NULL,  type = 'catchmentsp'),
               "IDs or a spatial AOI")
  expect_error(hydrogeofetch:::query_usgs_oafeat(AOI = pt, id = 17010101,  type = 'huc8_legacy'),
               "Either")
})

# ==============================================================================

test_that("bbox query coordinates are rounded outward", {
  # a bbox that came back from a coordinate transform carries digits that
  # differ between PROJ builds; the request has to be reproducible anyway
  bb <- c(-115.212532125541, 48.7726462196656,
          -114.894819558585, 48.9872432733115)

  rounded <- hydrogeofetch:::round_bbox_out(bb)

  expect_equal(paste(rounded, collapse = ","),
               "-115.212533,48.772646,-114.894819,48.987244")

  # rounding outward so the search area never shrinks
  expect_true(all(rounded[1:2] <= bb[1:2]))
  expect_true(all(rounded[3:4] >= bb[3:4]))

  # a bbox already at or below the rounding precision is left alone
  exact <- c(-115.0631, 48.86286, -115.0345, 48.88706)
  expect_equal(hydrogeofetch:::round_bbox_out(exact), exact)
})

# Walk our way through the 7 different offerings...
#   server   user_call           pygeoapi       ids
# 1 wmadata       huc08               huc08      huc8
# 2 wmadata       huc12               huc12     huc12
# 3 wmadata         nhd nhdflowline_network     comid
# 4 wmadata   catchment         catchmentsp featureid
# 5 wmadata     nhdarea             nhdarea     comid
# 6 wmadata waterbodies        nhdwaterbody     comid
# 7 wmadata     gagesII             gagesii     staid

# ==============================================================================

test_that("huc8", {
  skip_if_no_integration()

  expect_error(get_huc(AOI = rbind(pt, pt2), type = "huc08_2020"),
               "AOI must be one an only one feature.")

  ptHUC8 = get_huc(AOI = pt, type = "huc08_2020")
  expect_equal(nrow(ptHUC8), 1)
  expect_equal(ptHUC8$huc8, "17010101")
  expect_equal(sf::st_crs(ptHUC8)$epsg, 4326)

  areaHUC8 = get_huc(AOI = area, t_srs = 5070, type = "huc08_2020")
  expect_equal(sf::st_crs(areaHUC8)$epsg, 5070)
  expect_equal(nrow(areaHUC8), 1)

  ptHUC8id = get_huc(id = "17010101", type = "huc08_2020")
  expect_identical(ptHUC8$huc8, ptHUC8id$huc8)
  expect_true(sf::st_crs(ptHUC8) == sf::st_crs(ptHUC8id))
})

# ==============================================================================

test_that("huc", {
  skip_if_no_integration()

  expect_error(get_huc(AOI = pt, type = "borked"),
               "type must be one of")

  ptHUC12 = get_huc(AOI = pt, type = "huc12_nhdplusv2")
  expect_equal(nrow(ptHUC12), 1)
  expect_equal(ptHUC12$huc_12, "170101010306")

  areaHUC12 = get_huc(AOI = area, type = "huc12_nhdplusv2")
  expect_equal(nrow(areaHUC12), 2)

  HUC12id = get_huc(id = "170101010306", type = "huc12_nhdplusv2")
  expect_identical(ptHUC12$huc_12, HUC12id$huc_12)

  HUC12id2 = get_huc(id = areaHUC12$huc_12, type = "huc12_nhdplusv2") |>
    sf::st_transform(sf::st_crs(area))

  # expect_equal, not expect_identical -- HUC12id2 has been through a
  # transform, so its coordinates carry PROJ-build-dependent last digits
  expect_equal(HUC12id2$geometry, areaHUC12$geometry)

  hu12 <- get_huc(AOI = pt, type = "huc12_2020")
  expect_equal(hu12$huc12, "170101010806")

  expect_message(get_huc(AOI = pt, type = "huc12"),
                 "defaulting to 2025 version of WBD")

  hu10 <- get_huc(AOI = pt, type = "huc10_2020")
  expect_equal(hu10$huc10, "1701010108")

  hu08 <- get_huc(AOI = pt, type = "huc08_2020")
  expect_equal(hu08$huc8, "17010101")

  hu06 <- get_huc(AOI = pt, type = "huc06_2020")
  expect_equal(hu06$huc6, "170101")

  hu04 <- get_huc(AOI = pt, type = "huc04_2020")
  expect_equal(hu04$huc4, "1701")

  hu02 <- get_huc(AOI = pt, type = "huc02_2020")
  expect_equal(hu02$huc2, "17")

  expect_error(get_huc(AOI = pt, type = "test"))
})

# ==============================================================================

test_that("huc type autodetection from id length", {

  # autodetect huc level from id length (no type given)
  expect_message(
    out <- get_huc(id = "07", type = NULL),
    "Inferred type 'huc02' from ID length"
  )

  expect_message(
    out <- get_huc(id = "0709", type = NULL),
    "Inferred type 'huc04' from ID length"
  )

  expect_message(
    out <- get_huc(id = "070900020604"),
    "Inferred type 'huc12' from ID length"
  )

  # version suffix combined with autodetected huc level
  expect_message(
    out <- get_huc(id = "07090002", type = "_2020"),
    "Inferred type 'huc08_2020' from ID length and version suffix"
  )

  expect_message(
    out <- get_huc(id = "070900020604", type = "_nhdplusv2"),
    "Inferred type 'huc12_nhdplusv2' from ID length and version suffix"
  )

  # error on mixed-length ids

  expect_error(
    get_huc(id = c("07", "0709")),
    "All IDs must be the same length"
  )

  # error on invalid id length
  expect_error(
    get_huc(id = "123"),
    "does not correspond to a valid HUC type"
  )

})

# ==============================================================================

test_that("huc all versions and levels", {
  skip_if_no_integration()

  huc_ids <- c(huc02 = "07", huc04 = "0709", huc06 = "070900",
               huc08 = "07090002", huc10 = "0709000206", huc12 = "070900020604")

  versions <- c("_2025", "_2020", "_nhdplusv2", "_nhdplushr")

  for (v in versions) {
    for (i in seq_along(huc_ids)) {
      type <- paste0(names(huc_ids)[i], v)
      result <- get_huc(id = huc_ids[i], type = type)
      expect_true(nrow(result) >= 1,
                  info = paste("Failed for", type, "with id", huc_ids[i]))
    }
  }
})

# ==============================================================================

test_that("get_nhdplus...", {
  expect_error(get_nhdplus(AOI = area, realization = "wrong"))
  expect_error(get_nhdplus(AOI = as_Spatial(area)))
  expect_error(get_nhdplus(AOI = area, comid  = 101))
  expect_error(get_nhdplus(AOI = NULL, comid = NULL))

  with_mock_hgf("get_nhdplus", {
    fl = get_nhdplus(AOI = pt, realization = 'flowline')
    expect_equal(nrow(fl), 1)
    expect_equal(sf::st_crs(fl)$epsg, 4326)
    expect_equal(as.character(sf::st_geometry_type(fl)), 'LINESTRING')

    fl5070 = get_nhdplus(AOI = pt, realization = 'flowline', t_srs = 5070)
    expect_equal(sf::st_crs(fl5070)$epsg, 5070)

    cat = get_nhdplus(AOI = pt, realization = 'catchment')
    expect_equal(nrow(cat), 1)
    expect_equal(sf::st_crs(cat)$epsg, 4326)
    expect_true(grepl('POLYGON', as.character(sf::st_geometry_type(cat))))

    catch5070 = get_nhdplus(AOI = pt, realization = 'catchment', t_srs = 5070)
    expect_equal(sf::st_crs(catch5070)$epsg, 5070)

    out = get_nhdplus(AOI = pt, realization = 'outlet')
    expect_equal(nrow(out), 1)
    expect_equal(sf::st_crs(out)$epsg, 4326)
    expect_equal(as.character(sf::st_geometry_type(out)), 'POINT')

    areaNHD  = get_nhdplus(AOI = area, realization = "all")
    expect_equal(length(areaNHD), 3)
    expect_equal(nrow(areaNHD$flowline), nrow(areaNHD$catchment))
    expect_equal(nrow(areaNHD$outlet), nrow(areaNHD$catchment))
    expect_equal(as.character(sf::st_geometry_type(areaNHD$outlet))[1], 'POINT')
    expect_equal(as.character(sf::st_geometry_type(areaNHD$flowline))[1], 'LINESTRING')
    expect_true(grepl('POLYGON', as.character(sf::st_geometry_type(areaNHD$catchment))[1]))

    out <- capture_warnings(get_nhdplus(comid = 1))
    expect_true("No nhd features found" %in% out)

    idCheck  = get_nhdplus(comid = 101, realization = 'all')
    expect_equal(length(idCheck), 3)
    expect_equal(nrow(idCheck$flowline), nrow(idCheck$catchment))
    expect_equal(nrow(idCheck$catchment), nrow(idCheck$outlet))
    expect_equal(as.character(sf::st_geometry_type(idCheck$outlet))[1], 'POINT')
    expect_equal(as.character(sf::st_geometry_type(idCheck$flowline))[1], 'LINESTRING')
    expect_equal(as.character(sf::st_geometry_type(idCheck$catchment))[1], 'POLYGON')

    streamOrderSubset = get_nhdplus(AOI = area, streamorder = 3)
    expect_gt(nrow(areaNHD$flowline), nrow(streamOrderSubset))

    byGage = get_nhdplus(nwis = c('05427718'))
    expect_equal(nrow(byGage), 1)
    expect_equal(byGage$comid, 13293454)

    byGageComid = get_nhdplus(nwis = c('05427718'), comid = 101)
    expect_equal(nrow(byGageComid), 2)
  })
})

# ==============================================================================

test_that("nhdarea", {
  with_mock_hgf("nhdarea", {
    out <- capture_warnings(get_nhdarea(AOI  = pt))
    expect_true("No nhdarea features found" %in% out)

    nhdarea = get_nhdarea(AOI  = pt, buffer = 1e4)
    expect_equal(nhdarea$ftype, "Submerged Stream")
    expect_equal(sf::st_crs(nhdarea)$epsg, 4326)
    nhdarea5070 = get_nhdarea(AOI  = pt, buffer = 1e4, t_srs = 5070)
    expect_equal(sf::st_crs(nhdarea5070)$epsg, 5070)
  })
})

# ==============================================================================

test_that("nhdwaterbody", {
  with_mock_hgf("nhdwaterbody", {
    wb = get_waterbodies(AOI  = pt, buffer = 2e3)
    expect_equal(nrow(wb), 3)
    expect_equal(sf::st_crs(wb)$epsg, 4326)

    wb5070 = get_waterbodies(id = 22887007, t_srs = 5070)
    expect_equal(sf::st_crs(wb5070)$epsg, 5070)
    expect_equal(wb5070$comid, 22887007)
  })
})

# ==============================================================================

test_that("gagesii", {
  with_mock_hgf("gagesii", {
    gages2 = get_gagesII(AOI  = pt, buffer = 3e3)
    expect_equal(nrow(gages2), 1)

    gages2 <- get_gagesII(AOI = pt, buffer = 3e3,
                          basin = TRUE)

    expect_equal(names(gages2), c("site", "basin"))

    gages2 = get_gagesII(id = "01013500", basin = TRUE)
    expect_true("Ref" %in% gages2$site$class)
  })
})

# ==============================================================================

test_that("discover_nhdplus_id", {
  skip_if_no_integration()
  nldi_nwis <- list(featureSource = "nwissite", featureID = "USGS-08279500")
  discover_nhdplus_id(nldi_feature = nldi_nwis)
  expect_error(discover_nhdplus_id(nldi_feature = NULL, point = NULL))
})

test_that("get_nwis", {
  skip_if_no_integration()
  areaSearch = get_nwis(AOI = area)
  expect_equal(nrow(areaSearch), 1)
  expect_equal(sf::st_crs(areaSearch)$epsg, 4326)

  areaSearch5070 = get_nwis(AOI = area, t_srs = 5070)
  expect_equal(nrow(areaSearch5070), 1)
  expect_equal(sf::st_crs(areaSearch5070)$epsg, 5070)

  expect_equal(areaSearch$site_no, areaSearch5070$site_no)

  pt2BuffNorm = get_nwis(AOI = pt2)
  pt2BuffDecrease = get_nwis(AOI = pt2, buffer = 10000)

  Sys.sleep(5)
  pt2BuffIncrease = get_nwis(AOI = pt2, buffer = 40000)

  expect_true(nrow(pt2BuffIncrease) > nrow(pt2BuffNorm))
  expect_true(nrow(pt2BuffNorm) > nrow(pt2BuffDecrease))
  expect_equal(order(pt2BuffNorm$distance_m), 1:nrow(pt2BuffNorm))

  expect_error(get_nwis(AOI = pt2, buffer = 1000000))

  expect_error(get_nwis(AOI = AOI, buffer = 1))
  expect_warning(get_nwis(AOI = sf::st_buffer(sf::st_transform(pt2,5070), 1)))

})

test_that("get_huc12_by_huc", {
  expect_error(hydrogeofetch:::get_huc12_by_huc(c("12050001", "1205000101")),
               "huc_ids must be all 8-digit")
  expect_error(hydrogeofetch:::get_huc12_by_huc("120500"),
               "huc_ids must be all 8-digit")

  skip_if_no_integration()

  h10 <- hydrogeofetch:::get_huc12_by_huc("1701010103")
  expect_s3_class(h10, "sf")
  expect_true(nrow(h10) > 0)
  expect_true(all(substr(h10$huc_12, 1, 10) == "1701010103"))

  h08 <- hydrogeofetch:::get_huc12_by_huc("17010101")
  expect_s3_class(h08, "sf")
  expect_true(nrow(h08) > nrow(h10))
  expect_true(all(substr(h08$huc_12, 1, 8) == "17010101"))
})

test_that("get_huc12_by_huc pages within batch", {
  skip_if_no_integration()

  # two HUC10s together, exercising POST paging
  h10_multi <- hydrogeofetch:::get_huc12_by_huc(c("1701010103", "1701010104"))
  expect_s3_class(h10_multi, "sf")

  h10_a <- hydrogeofetch:::get_huc12_by_huc("1701010103")
  h10_b <- hydrogeofetch:::get_huc12_by_huc("1701010104")

  expect_equal(nrow(h10_multi), nrow(h10_a) + nrow(h10_b))
  expect_true(all(h10_a$huc_12 %in% h10_multi$huc_12))
  expect_true(all(h10_b$huc_12 %in% h10_multi$huc_12))
})
