
#' Check whether a package is on CRAN or not
#'
#' This does not currently appear in either summary or print methods, and is
#' only used as part of `pkcchk_pkgname_available`.
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @noRd
pkgchk_on_cran <- function (checks) {

    desc <- data.frame (read.dcf (fs::path (
        checks$pkg$path,
        "DESCRIPTION"
    )),
    stringsAsFactors = FALSE
    )
    pkg <- desc$Package

    ap <- data.frame (utils::available.packages (),
        stringsAsFactors = FALSE
    )
    res <- pkg %in% ap$Package

    if (res) {
        # Check whether CRAN package of that name has same title

        u <- paste0 (
            "https://cran.r-project.org/web/packages/",
            pkg, "/index.html"
        )
        x <- tryCatch (
            rvest::read_html (u),
            error = function (e) e
        )

        if (methods::is (x, "simpleError")) {
            res <- FALSE
        } else {
            h2 <- paste0 (rvest::html_elements (x, "h2"))
            h2 <- gsub ("<h2>|<\\/h2>", "", h2)
            res <- grepl (desc$Title, h2, fixed = TRUE) |
                grepl (desc$Title, h2, ignore.case = TRUE, perl = TRUE)

            if (!res) { # Title may have changed; check URL as backup
                tab <- rvest::html_table (x) [[1]]
                if (ncol (tab) == 2) {
                    names (tab) <- c ("field", "value")
                    tab <- tab [grep ("^URL", tab$field), ]
                    url <- paste0 (tab$value, collapse = ", ")
                    res <- identical (url, desc$URL)
                }
            }
        }
    }

    return (res)
}
