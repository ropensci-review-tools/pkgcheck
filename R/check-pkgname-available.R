

pkgchk_pkgname_available <- function (path) {

    desc <- data.frame (read.dcf (file.path (path, "DESCRIPTION")))
    pkg <- desc$Package

    pkg_grepped <- grep (.standard_regexps()$valid_package_name,
                         pkg,
                         value = TRUE)

    ap <- data.frame (utils::available.packages ())

    return (!pkg %in% ap$Package &
            pkg == pkg_grepped)
}

#' Summarise checks that package name is available
#'
#' @return tick or cross
#' @noRd
summarise_pkgname_chk <- function (checks) {

    if (checks$pkgname_available & !checks$pkg_on_cran) {

        res <- paste0 ("- ", symbol_tck (),
                       " Package name is available.")

    } else if (checks$pkg_on_cran) {

        res <- paste0 ("- ", symbol_tck (),
                       " Package is already on CRAN.")

    } else {

        res <- paste0 ("- ", symbol_crs (),
                       " Package name is not available (on CRAN).")
    }

    return (res)
}
