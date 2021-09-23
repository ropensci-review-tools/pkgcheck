

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
                         full.names = FALSE)
    flist <- vapply (flist, function (i)
                     utils::tail (decompose_path (i) [[1]], 1L),
                     character (1),
                     USE.NAMES = FALSE)

    chk <- any (grepl ("^contributing(\\.|$)", flist, ignore.case = TRUE))

    return (chk)
}

#' @param checks Result of main \link{pkgcheck} function
#' @return Test output with formatted check items as tick or cross.
#' @noRd
summarise_has_contrib <- function (checks) {

    # has_this is in summarise-checks.R
    has_this (checks, "has_contrib",
              "has", "does not have", "a 'contributing.md' file.")
}
