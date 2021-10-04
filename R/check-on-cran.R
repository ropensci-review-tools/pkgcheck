
#' Check whether a package is on CRAN or not
#'
#' This does not currently appear in either summary or print methods, and is
#' only used as part of `pkcchk_pkgname_available`.
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @noRd
pkgchk_on_cran <- function (checks) {
    desc <- data.frame (read.dcf (file.path (
        checks$pkg$path,
        "DESCRIPTION"
    )))
    pkg <- desc$Package

    ap <- data.frame (utils::available.packages ())
    res <- pkg %in% ap$Package

    if (res) {
        # Check whether CRAN package of that name has same title

        u <- paste0 (
            "https://cran.r-project.org/web/packages/",
            pkg, "/index.html"
        )
        x <- rvest::read_html (u)
        h2 <- paste0 (rvest::html_elements (x, "h2"))
        h2 <- gsub ("<h2>|<\\/h2>", "", h2)
        res <- grepl (desc$Title, h2, ignore.case = TRUE)
    }

    return (res)
}
