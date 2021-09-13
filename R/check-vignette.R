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

#' @param checks Result of main \link{pkgcheck} function
#' @return tick or cross
#' @noRd
summarise_has_vignette <- function (checks) {

    # has_this is in summarise-checks.R
    has_this (checks,
              "has_vignette",
              "has",
              "does not have",
              "at least one vignette.")
}


vignette_checks_md <- function (checks) {

    if (checks$vignette)
        return (NULL)

    c ("",
       paste0 (symbol_crs (),
               " Package contains no vignette."),
       "")
}
