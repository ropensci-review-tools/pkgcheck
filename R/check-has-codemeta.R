
#' Check whether a package has a `codemeta.json` file
#'
#' @noRd
pkgchk_has_codemeta <- function (path) {

    "codemeta.json" %in% list.files (path, recursive = FALSE)
}

output_pkgchk_has_codemeta <- function (checks) {

    out <- list (check_pass = checks$checks$has_codemeta,
                summary = "",
                print = "") # no print method

    out$summary <- paste0 (ifelse (out$check_pass, "has", "does not have"),
                           " a 'codemeta.json' file.")

    return (out)
}
