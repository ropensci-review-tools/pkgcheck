library (testthat)
library (pkgcheck)

options (repos = c (
    ropenscireviewtools = "https://ropensci-review-tools.r-universe.dev",
    CRAN = "https://cloud.r-project.org"
))

test_check ("pkgcheck")
