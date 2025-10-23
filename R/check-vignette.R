#' Check whether the package contains at least one vignette.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Logical flag
#' @noRd
pkgchk_has_vignette <- function (checks) {

    # from pkgdown
    # https://github.com/r-lib/pkgdown/blob/705ff7c650bb1c7d46d35e72f27ad093689e2f29/R/package.r#L202 # nolint
    base <- fs::path (checks$pkg$path, "vignettes")

    vig_path <- character (0L)

    if (dir.exists (base)) {
        vig_path <- list.files (base,
            pattern = "\\.[rRq]md$",
            recursive = TRUE,
            full.names = TRUE
        )
    }

    is_html <- function (f) {
        format <- tryCatch (
            rmarkdown::default_output_format (f)$name,
            error = function (e) NULL
        )
        if (is.null (format)) {
            # See https://yihui.org/litedown/#sec:yaml-syntax
            # litedown uses "taml", which fails with yaml parsers
            f_lines <- readLines (f)
            requireNamespace ("xfun", quietly = TRUE)
            # This can fail (xfun v0.53), so requires a fallback:
            taml <- tryCatch (
                xfun::taml_load (f_lines),
                error = function (e) NULL
            )
            if (!is.null (taml)) {
                format <- names (taml$output)
            } else {
                # manual taml parsing
                taml_index <- grep ("^\\-\\-+", f_lines)
                if (length (taml_index) >= 2L) {
                    taml_content <- f_lines [seq (taml_index [1] + 1, taml_index [2] - 1)]
                    index <- grep ("(output|vignette)\\:", taml_content)
                    if (length (index) >= 2) {
                        # the whole "output" fields which will contain format
                        format <- taml_content [seq (index [1], index [2])]
                    }
                }
            }
        }

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
        # - .distill_article
        # so the following pattern suffices:
        html_prefixes <- "htm(l?)|distill"

        grepl (html_prefixes, format, ignore.case = TRUE)
    }

    is_html <- unlist (lapply (vig_path, is_html))

    return (any (is_html))
}

output_pkgchk_has_vignette <- function (checks) {

    out <- list (
        check_pass = checks$checks$has_vignette,
        summary = "",
        print = ""
    ) # no print method

    out$summary <- ifelse (
        out$check_pass,
        "Package has at least one HTML vignette",
        "Package has no HTML vignettes"
    )

    return (out)
}
