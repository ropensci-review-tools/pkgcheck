#' Check whether a package is on CRAN or not.
#'
#' This does not currently appear in any 'pkgcheck' output (that is, neither in
#' summary or print methods), and is only used as part of
#' `pkcchk_pkgname_available`.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return A single logical value indicating whether a package is on CRAN
#' (`TRUE`), or not (`FALSE`).
#' @noRd
pkgchk_on_cran <- function (checks) {

    desc <- data.frame (
        read.dcf (fs::path (
            checks$pkg$path,
            "DESCRIPTION"
        )),
        stringsAsFactors = FALSE
    )
    desc$Title <- gsub ("\\s+", " ", gsub ("\\n", " ", desc$Title))
    pkg <- desc$Package

    ap <- get_available_packages ()
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

        same_title <- same_url <- FALSE
        if (!methods::is (x, "simpleError")) {
            h2 <- paste0 (rvest::html_elements (x, "h2"))
            h2 <- gsub ("<h2>|<\\/h2>", "", h2)
            h2 <- gsub ("\\n|\\s+", " ", h2)
            h2 <- gsub (paste0 (pkg, "\\:(\\s+?)"), "", h2)
            if (length (h2) > 0L) {
                same_title <- grepl (desc$Title, h2, fixed = TRUE) |
                    grepl (desc$Title, h2, ignore.case = TRUE, perl = TRUE)
            }

            tab <- rvest::html_table (x)
            if (length (tab) > 0L) {
                tab <- tab [[1]]
                if (ncol (tab) == 2) {
                    names (tab) <- c ("field", "value")
                    tab <- tab [grep ("^URL", tab$field), ]
                    url <- paste0 (tab$value, collapse = ", ")
                    url <- gsub ("\\n|\\s+", " ", url)
                    same_url <- identical (url, desc$URL)
                }
            }
        }
        res <- same_title || same_url
    }

    return (res)
}
