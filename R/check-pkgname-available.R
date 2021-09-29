

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

output_pkgchk_pkgname <- function (checks) {

    out <- list (check_pass = TRUE,
                summary = "",
                print = "") # no print method

    if (checks$checks$pkgname_available & !checks$checks$pkg_on_cran) {

        out$summary <- "Package name is available"

    } else if (checks$checks$pkg_on_cran) {

        out$summary <- "Package is already on CRAN."

    } else {

        out$check_pass <- FALSE
        out$summary <- "Package name is not available (on CRAN)."
    }

    return (out)
}
