
# A fake httr2 http condition, carrying a response the way req_perform() raises one.
http_cond <- function(status, body = NULL, content_type = "application/json") {
  resp <- httr2::response(status_code = status,
                          headers = list(`content-type` = content_type),
                          body = if(is.null(body)) raw() else charToRaw(body))
  structure(
    class = c(paste0("httr2_http_", status), "httr2_http", "httr2_error",
              "rlang_error", "error", "condition"),
    list(message = paste0("HTTP ", status, "."), status = status, resp = resp,
         call = NULL)
  )
}

test_that("hgf_fail reports the service's own explanation for a 400", {
  body <- paste0('{"type":"InvalidParameterValue","description":"Error executing ',
                 'process: Flowtrace intersected a nodata FDR value; cannot ',
                 'continue downhill."}')

  w <- tryCatch(hgf_fail("features", "http://example.org/split", http_cond(400, body)),
                warning = function(w) w)

  expect_s3_class(w, "warning")
  expect_match(conditionMessage(w), "Flowtrace intersected a nodata FDR value")
  expect_match(conditionMessage(w), "InvalidParameterValue")
  expect_match(conditionMessage(w), "http://example.org/split", fixed = TRUE)
})

test_that("a 400 is permanent and a 503 is not", {
  perm <- tryCatch(hgf_fail("features", "u", http_cond(400, '{"description":"nope"}')),
                   warning = function(w) w)
  expect_s3_class(perm, "hgf_permanent_error")
  expect_match(conditionMessage(perm), "retrying will not help")

  transient <- tryCatch(hgf_fail("features", "u", http_cond(503)),
                        warning = function(w) w)
  expect_s3_class(transient, "hgf_request_failure")
  expect_false(inherits(transient, "hgf_permanent_error"))
  expect_no_match(conditionMessage(transient), "retrying will not help")
})

test_that("429 and 408 stay retryable, other 4xx do not", {
  for(s in c(408, 429)) {
    w <- tryCatch(hgf_fail("features", "u", http_cond(s)), warning = function(w) w)
    expect_false(inherits(w, "hgf_permanent_error"), label = paste("status", s))
  }
  for(s in c(400, 404, 422)) {
    w <- tryCatch(hgf_fail("features", "u", http_cond(s)), warning = function(w) w)
    expect_s3_class(w, "hgf_permanent_error")
  }
})

test_that("a non-http failure warns without claiming to be permanent", {
  e <- simpleError("Could not resolve host: example.org")
  w <- tryCatch(hgf_fail("features", "u", e), warning = function(w) w)

  expect_s3_class(w, "hgf_request_failure")
  expect_false(inherits(w, "hgf_permanent_error"))
  expect_match(conditionMessage(w), "Could not resolve host")
})

test_that("an unparseable body still reaches the caller", {
  w <- tryCatch(hgf_fail("features", "u", http_cond(400, "<html>gateway said no</html>",
                                                    content_type = "text/html")),
                warning = function(w) w)
  expect_match(conditionMessage(w), "gateway said no")
})

test_that("get_split_catchment drops its boundary advice on a permanent failure", {
  # the advice is about where the point is, which is not the problem when the flow
  # direction grid has a hole under it
  local_mocked_bindings(
    hgf_sf = function(...) {
      hgf_fail("features", "u", http_cond(400, '{"description":"nodata FDR"}'))
      NULL
    },
    make_json_input_split = function(...) "{}",
    check_point = function(point, ...) list(point),
    get_nldi_url = function(...) "http://example.org/"
  )

  msgs <- character()
  out <- withCallingHandlers(
    suppressWarnings(get_split_catchment(sf::st_sfc(sf::st_point(c(-111.8, 40.7)),
                                                    crs = 4326))),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }
  )

  expect_null(out)
  expect_false(any(grepl("get_raindrop_trace", msgs)))
})

test_that("get_split_catchment keeps its boundary advice on a transient failure", {
  local_mocked_bindings(
    hgf_sf = function(...) {
      hgf_fail("features", "u", http_cond(503))
      NULL
    },
    make_json_input_split = function(...) "{}",
    check_point = function(point, ...) list(point),
    get_nldi_url = function(...) "http://example.org/"
  )

  msgs <- character()
  out <- withCallingHandlers(
    suppressWarnings(get_split_catchment(sf::st_sfc(sf::st_point(c(-111.8, 40.7)),
                                                    crs = 4326))),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") }
  )

  expect_null(out)
  expect_true(any(grepl("get_raindrop_trace", msgs)))
})
