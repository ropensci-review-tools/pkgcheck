
#' Check for renv files in package directory
#'
#' @param path Location of local repository
#'
#' @noRd
pkginfo_uses_renv <- function (path) {

    files <- list.files (
        path,
        pattern = "renv\\.lock$",
        full.names = TRUE
    )

    return (length (files) > 0L)
}
