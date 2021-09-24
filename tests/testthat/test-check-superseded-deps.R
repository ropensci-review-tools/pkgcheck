test_that("pkgchk_has_superseded_deps() works", {
  pkgname <- "OLD"
  d <- srr::srr_stats_pkg_skeleton (pkg_name = pkgname)
  deps <- desc::desc_get_deps(file.path(d, "DESCRIPTION"))
  deps <- rbind(deps, data.frame(type = "Imports", package = "XML", version = "*"))
  desc::desc_set_deps(deps, file = file.path(d, "DESCRIPTION"))
  s <- pkgstats::pkgstats(d)
  pkgchk_has_superseded_deps(s)
})
