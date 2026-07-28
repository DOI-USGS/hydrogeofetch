test_that("discover", {

  with_mock_hgf("geoconnex_discover", {
    expect_s3_class(discover_geoconnex_reference(), "data.frame")

    expect_equal(names(discover_geoconnex_reference()),
                 c("id", "title", "description", "url", "attribute", "type"))
  })

})

test_that("get", {

  # debug_hydrogeofetch=true in the environment makes every request emit a
  # "GET <url>" message, which would break the expect_silent below.
  local_mocked_bindings(nhdplus_debug = function() FALSE, .package = "hydrogeofetch")

  with_mock_hgf("geoconnex_get", {
    expect_warning(avail <- get_geoconnex_reference())

    expect_equal(avail, discover_geoconnex_reference())

    AOI <- sf::st_as_sfc(sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
                                       xmax = -89.24681, ymax = 43.17192),
                                     crs = "+proj=longlat +datum=WGS84 +no_defs"))

    AOI_b <- sf::st_bbox(c(xmin = -89.56684, ymin = 42.99816,
                           xmax = -89.24681, ymax = 43.17192))

    out <- get_geoconnex_reference(AOI, type = "hu04")

    expect_silent(out2 <- get_geoconnex_reference(AOI_b, type = "hu04", status = FALSE))

    expect_true(all(out$id %in% out2$id))

    expect_s3_class(out, "sf")
    expect_equal(nrow(out), 2)

    AOI <- sf::st_sfc(sf::st_point(c(-89.56684, 42.99816)),
                      crs = "+proj=longlat +datum=WGS84 +no_defs")

    out <- get_geoconnex_reference(AOI, type = "hu04", buffer = 10000,
                                   t_srs = 5070, status = FALSE)

    expect_true(sf::st_crs(out) == sf::st_crs(5070))

    out <- get_geoconnex_reference("https://geoconnex.us/ref/mainstems/359842",
                                   type = "hu04")

    expect_s3_class(out, "sf", exact = FALSE)
  })
})
