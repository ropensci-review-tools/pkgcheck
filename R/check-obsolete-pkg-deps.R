
#' Check whether the package depends on any obsolete packages for which better
#' alternative should be used.
#'
#' The list of obsolete packages is
#' \url{https://devguide.ropensci.org/building.html#recommended-scaffolding},
#' defined below.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Character vector of any obsolete dependencies; otherwise
#' a character(0) if license(s) is/are acceptable.
#' @noRd
pkgchk_obsolete_pkg_deps <- function (checks) {

    obs_pkgs <- c ("RCurl", "rjson", "RJSONIO", "XML")

    deps <- checks$pkg$dependencies$package
    deps <- deps [which (!deps == "NA")]

    deps [which (deps %in% obs_pkgs)]
}

output_pkgchk_obsolete_pkg_deps <- function (checks) {

    out <- list (
        check_pass = length (checks$checks$obsolete_pkg_deps) == 0L,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- paste0 (
            "Package depends on the following obsolete packages: [",
            checks$checks$obsolete_pkg_deps, "]"
        )
    }

    return (out)
}
