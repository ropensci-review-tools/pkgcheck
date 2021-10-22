test_that("pkgchk_has_superseded_deps() works", {
  checks <- list(pkg = list(dependencies = tibble::tibble(package = c("RCurl", "rjson", "XML"))))
  expect_equal(
    pkgchk_has_superseded_deps(checks),
    "RCurl (recommended: curl, crul, httr or httr2), XML (recommended: xml2), rjson (recommended: jsonlite)"
  )
})
