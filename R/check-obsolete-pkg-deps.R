
#' Check whether the package depends on any obsolete packages for which better
#' alternative should be used.
#'
#' The list of obsolete packages is given in [our *Packaging
#' Guide*](https://devguide.ropensci.org/building.html#recommended-scaffolding).
#' Some of these are truly obsolete, the use of which raises a red cross in the
#' summary of checks; while others are only potentially obsolete, thus use of
#' which merely raises a note in the detailed check output.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Character vector of any obsolete dependencies; otherwise
#' a character(0) if license(s) is/are acceptable.
#' @noRd
pkgchk_obsolete_pkg_deps <- function (checks) {

    obs_pkgs <- c (
        "RCurl", "rjson", "RJSONIO", "XML", # truly obselete
        "sp", "rgdal", "maptools", "rgeos" # potentially obsolete
    )

    deps <- checks$pkg$dependencies$package
    deps <- deps [which (!deps == "NA")]

    deps [which (deps %in% obs_pkgs)]
}

output_pkgchk_obsolete_pkg_deps <- function (checks) { # nolint

    # https://github.com/ropensci/software-review-meta/issues/47
    # potential <- paste0 (c ("sp", "rgdal", "maptools", "rgeos"), collapse = "|")
    # Elevate "sp" to "truly obsolete"
    potential <- paste0 (c ("rgdal", "maptools", "rgeos"), collapse = "|")
    potential <- grep (potential, checks$checks$obsolete_pkg_deps, value = TRUE)

    if (length (potential) == 0) {
        index <- seq_along (checks$checks$obsolete_pkg_deps)
    } else {
        index <- which (!grepl (
            paste0 (potential, collapse = "|"),
            checks$checks$obsolete_pkg_deps
        ))
    }
    obs_pkg_deps <- checks$checks$obsolete_pkg_deps [index]

    out <- list (
        check_pass = length (obs_pkg_deps) == 0L,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- paste0 (
            "Package depends on the following obsolete packages: [",
            paste0 (obs_pkg_deps, collapse = ","), "]"
        )
    }

    if (length (potential) > 0L || !out$check_pass) {

        ptl <- ifelse (length (potential) > 0L, "(potentially) ", "")
        obs_pkgs <- unique (c (obs_pkg_deps, potential))

        out$print <- list (
            msg_pre = paste0 (
                "Package contains the following ",
                ptl,
                "obsolete packages:"
            ),
            obj = obs_pkgs,
            msg_post = paste0 (
                "See our ",
                "[Recommended Scaffolding](https://devguide.ropensci.org/",
                "building.html?q=scaffol#recommended-scaffolding)",
                " for alternatives."
            )
        )
    }

    return (out)
}
