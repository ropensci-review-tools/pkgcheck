#' Check whether the package contains at least one vignette.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Logical flag
#' @noRd
pkgchk_has_vignette <- function (checks) {

    # from pkgdown
    # https://github.com/r-lib/pkgdown/blob/705ff7c650bb1c7d46d35e72f27ad093689e2f29/R/package.r#L202 # nolint
    base <- file.path (checks$package$path, "vignettes")

    vig_path <- character(0L)

    if (dir.exists(base)) {

        vig_path <- list.files(base,
                               pattern = "\\.[rR]md$",
                               full.names = TRUE)
    }

    is_html <- function(f) {

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

    return (any (is_html))

}

output_pkgchk_has_vignette <- function (checks) {

    out <- list (check_pass = checks$checks$vignette,
                summary = "",
                print = "") # no print method

    out$summary <- ifelse (out$check_pass,
                           "Package has at least one HTML vignette",
                           "Package has at no HTML vignettes")

    return (out)
}
