
#' Check whether all functions have examples
#'
#' @inheritParams pkg_uses_roxygen2
#' @return Vector of named logical values, one for each '.Rd' file indicating
#' whether or not it has example lines.
#' @noRd
pkgchk_pkg_fns_have_exs <- function (path) {

    rd <- list.files (file.path (path, "man"),
                      pattern = "\\.Rd$",
                      full.names = TRUE)

    has_ex <- vapply (rd, function (i) {
                          rd_i <- tools::parse_Rd (i)
                          ex <- get_Rd_meta (rd_i, "examples")
                          length (ex) > 0
                      },
                      logical (1),
                      USE.NAMES = TRUE)

    names (has_ex) <-
        vapply (names (has_ex), function (i)
                utils::tail (decompose_path (i) [[1]], 1L),
                character (1))

    has_ex <- has_ex [which (!grepl ("\\-package\\.Rd$", names (has_ex)))]

    return (has_ex)
}

#' Summarise checks that all functions have examples
#'
#' @return tick or cross
#' @noRd
summarise_fns_have_exs <- function (checks) {

    no_ex <- which (!checks$info$fns_have_exs)

    ifelse (all (checks$info$fns_have_exs),
            paste0 ("- ", symbol_tck (),
                    " All functions have examples."),
            paste0 ("- ", symbol_crs (),
                    " These functions do not have examples: [",
                    paste0 (names (checks$info$fns_have_exs) [no_ex]),
                    "]."))
}
