#' Check whether the package contains at least one vignette.
#'
#' @inheritParams pkg_uses_roxygen2
#' @return Logical flag
#' @noRd
pkgchk_has_vignette <- function (path) {
  # from pkgdown https://github.com/r-lib/pkgdown/blob/705ff7c650bb1c7d46d35e72f27ad093689e2f29/R/package.r#L202
  base <- file.path(path, "vignettes")

  if (!dir.exists(base)) {
    vig_path <- character()
  } else {
    vig_path <- dir(base, pattern = "\\.[rR]md$")
  }
  vig_path <- basename(vig_path)
  vig_path <- vig_path[!grepl("^_", basename(vig_path))]
  return (length (vig_path) >= 1L)

}
