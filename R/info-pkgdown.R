#' Get list of all 'concepts' used to group man entries.
#'
#' @param path Location of local repository to report on
#'
#' @noRd
pkginfo_pkgdown <- function (path) {

    rd_files <- list_rd_files (path) # utils.R

    concepts <- vapply (rd_files, function (i) {
        rd_i <- tools::parse_Rd (i, permissive = TRUE)
        get_Rd_meta (rd_i, "concept") [1]
    },
    character (1),
    USE.NAMES = TRUE
    )

    # Files with no concepts are NA, which is okay:
    unique (concepts [which (!is.na (concepts))])
}
