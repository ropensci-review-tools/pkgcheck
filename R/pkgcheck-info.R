#' Population the 'info' part of the main return object.
#'
#' This function is (only) called within the main `pkgcheck()` function.
#'
#' @param checks `pkgcheck` object with values for "check" only. This function
#' populates the "info" component.
#' @param path Local path to repo.
#' @param stats Object returned from `pkgstats_info()` function.
#'
#' @return Modified version of input parameters `checks` with populated "info"
#' component.
#'
#' @noRd
pkgcheck_fill_info <- function (checks, path, stats) {

    info_items <- c ("fn_names", "git", "pkgstats")
    if ("srr" %in% names (stats$out)) {
        info_items <- c (info_items, "srr")
    }
    checks$info <- stats$out [info_items]

    checks$info$pkgdown_concepts <- pkginfo_pkgdown (path)
    checks$info$network_file <- fn_call_network (stats)
    checks$info$renv_activated <- pkginfo_renv_activated (path)

    return (checks)
}
