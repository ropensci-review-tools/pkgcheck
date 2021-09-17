#' Check whether the package contains at least one vignette.
#'
#' @inheritParams pkg_uses_roxygen2
#' @return Logical flag
#' @noRd
pkgchk_has_vignette <- function (path) {

    # from pkgdown
    # https://github.com/r-lib/pkgdown/blob/705ff7c650bb1c7d46d35e72f27ad093689e2f29/R/package.r#L202 # nolint
    base <- file.path(path, "vignettes")

    if (!dir.exists(base)) {
        vig_path <- character()
    } else {
        vig_path <- dir(base, pattern = "\\.[rR]md$")
    }

    vig_path <- basename(vig_path)
    vig_path <- vig_path[!grepl("^_", basename(vig_path))]

    is_html <- function(vignette_path) {

        f <- file.path("vignettes", vignette_path)
        format <- rmarkdown::default_output_format(f)$name

        # See suffix dictionary at
        # ropensci-review-tools/pkgstats/R/type-dict.R
        # `.xht` is ignored here, otherwise the full list is
        # - .html.hl
        # - .htm
        # - .html
        # - .xhtml
        # - .phtml
        # - .rhtml
        # - .cshtml
        # so the following pattern suffices:
        html_prefixes <- "htm(l?)"

        grepl(html_prefixes, format, ignore.case = TRUE)
    }

    is_html <- unlist(lapply(vig_path, is_html))
    vig_path <- vig_path[is_html]

    return (length (vig_path) >= 1L)

}

#' @param checks Result of main \link{pkgcheck} function
#' @return tick or cross
#' @noRd
summarise_has_vignette <- function (checks) {

    ifelse (checks$vignette,
            paste0 ("- ", symbol_tck (),
                    " Package has at least one HTML vignette"),
            paste0 ("- ", symbol_crs (),
                    " Package has at no HTML vignettes"))
}


vignette_checks_md <- function (checks) {

    if (checks$vignette)
        return (NULL)

    c ("",
       paste0 (symbol_crs (),
               " Package contains no vignette."),
       "")
}
