
#' Check whether a package has a `codemeta.json` file
#'
#' @noRd
pkgchk_has_codemeta <- function (path) {

    "codemeta.json" %in% list.files (path, recursive = FALSE)
}

#' Check presence of codemeta.json
#' @param checks Result of main \link{pkgcheck} function
#' @return Test output with formatted check items as tick or cross.
#' @noRd
summarise_has_codemeta <- function (checks) {

    has_this (checks, "has_codemeta",
              "has", "does not have", "a 'codemeta.json' file.")
}
