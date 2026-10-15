#' Check that package name is available and/or already on CRAN.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @noRd
pkgchk_pkgname_available <- function (checks) {

    desc <- data.frame (
        read.dcf (fs::path (
            checks$pkg$path,
            "DESCRIPTION"
        )),
        stringsAsFactors = FALSE
    )
    pkg <- desc$Package

    valid_name <- grepl (
        paste0 ("^", .standard_regexps ()$valid_package_name, "$"),
        pkg
    )

    ap <- get_available_packages ()
    if (is.null (ap)) {
        return (NA)
    }

    return (valid_name && !pkg %in% ap$Package)
}

output_pkgchk_pkgname <- function (checks) {

    out <- list (
        check_pass = TRUE,
        summary = "",
        print = ""
    ) # no print method

    if (is.na (checks$checks$on_cran)) {
        out$summary <- "Could not check package name on CRAN"
        out$check_pass <- FALSE
        out$check_type <- "pass_watch"
    } else if (checks$checks$pkgname_available & !checks$checks$on_cran) {
        out$summary <- "Package name is available"
    } else if (checks$checks$on_cran) {
        out$summary <- "Package is already on CRAN."
    } else {
        out$check_pass <- FALSE
        out$summary <- "Package name is not available (on CRAN)."
    }

    return (out)
}
