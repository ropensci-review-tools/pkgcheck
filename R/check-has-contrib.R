

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

output_pkgchk_has_contrib <- function (checks) {

    out <- list (check_pass = checks$checks$has_contrib,
                summary = "",
                print = "") # no print method

    out$summary <- paste0 (ifelse (out$check_pass, "has", "does not have"),
                           " a 'contributing' file.")

    return (out)
}
