

#' Check whether package has 'contributing.md', as well as whether it has a
#' life cycle statement
#'
#' @inheritParams pkg_uses_roxygen2
#' @return Logical flag
#' @noRd
pkgchk_has_contrib_md <- function (path) {

    flist <- list.files (path,
                         all.files = TRUE,
                         recursive = TRUE,
                         full.names = TRUE)

    # contributing either with or without ".md" extension:
    ptn <- paste0 (.Platform$file.sep, "contributing", c ("$", "\\.md$"))
    f <- grep (paste0 (ptn, collapse = "|"), flist, ignore.case = TRUE)

    return (length (f) == 1L)
}

#' @param checks Result of main \link{pkgcheck} function
#' @return Test output with formatted check items as tick or cross.
#' @noRd
summarise_has_contrib <- function (checks) {

    # has_this is in summarise-checks.R
    has_this (checks, "has_contrib",
              "has", "does not have", "a 'contributing.md' file")
}
