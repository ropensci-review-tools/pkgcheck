

#' Check whether package's 'contributing.md' has a lifecycle statement
#'
#' This repeats code from `has_contrib_md`, which is somewhat inefficient, but
#' memoising the main extraction is not viable because path may stay the same
#' while contents at `path` may change.
#' @inheritParams pkg_uses_roxygen2
#' @return Single logical flag
#' @noRd
pkgchk_has_lifecycle <- function (path) {


    flist <- list.files (path,
                         all.files = TRUE,
                         recursive = TRUE,
                         full.names = TRUE)

    # contributing either with or without ".md" extension:
    ptn <- paste0 (.Platform$file.sep, "contributing", c ("$", "\\.md$"))
    f <- grep (paste0 (ptn, collapse = "|"), flist, ignore.case = TRUE)

    has_lifecycle <- FALSE
    if (length (f) == 1L) {

        contrib <- readLines (flist [f], encoding = "UTF-8")

        has_lifecycle <- any (grepl ("life\\s?cycle",
                                     contrib,
                                     ignore.case = TRUE))
    }

    return (has_lifecycle)
}

#' @param checks Result of main \link{pkgcheck} function
#' @return Test output with formatted check items as tick or cross.
#' @noRd
summarise_has_lifecycle <- function (checks) {

    # has_this is in summarise-checks.R
    has_this (checks, "has_lifecycle",
              "has", "does not have", "a lifecycle statement")
}
