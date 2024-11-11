#' Check and convert path arguments
#'
#' Mostly called for side-effects of erroring when path is not root directory of
#' an R package.
#' @noRd
convert_path <- function (path = ".") {

    path <- fs::path_norm (path)

    # see also https://github.com/r-lib/usethis/blob/master/R/proj.R
    proj_crit <- function () {
        rprojroot::is_r_package |
            rprojroot::is_git_root
    }
    proj_root <- tryCatch (
        rprojroot::find_root (proj_crit (), path = path),
        error = function (e) NULL
    )

    if (is.null (proj_root)) {
        stop (paste0 ("[", path, "] does not appear to be an R package"))
    }

    proj_root <- tryCatch (
        rprojroot::find_package_root_file (path = path),
        error = function (e) NULL
    )
    if (is.null (proj_root)) {
        subdirs <- fs::dir_ls (path, type = "directory")
        proj_root <- unlist (lapply (subdirs, function (d) {
            tryCatch (
                rprojroot::find_package_root_file (path = d),
                error = function (e) NULL
            )
        }))
    }

    if (length (proj_root) != 1L) {
        cli::cli_abort (
            "Could not find unambiguous project root from {proj_root}"
        )
    }

    return (proj_root)
}
