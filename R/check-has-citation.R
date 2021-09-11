

#' Check whether a package has a `inst/CITATION` file
#'
#' This does no check the contents of that file in any way.
#' @noRd
pkgchk_has_citation <- function (path) {

    "CITATION" %in% list.files (file.path (path, "inst"))
}

#' Check presence of citation
#' @param checks Result of main \link{pkgcheck} function
#' @return Logical flag
#' @noRd
summarise_has_citation <- function (checks) {

    has_this (checks, "has_citation",
              "has", "does not have", "a 'CITATION' file")
}
