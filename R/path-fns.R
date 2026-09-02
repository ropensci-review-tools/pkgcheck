#' Check and convert path arguments
#'
#' Mostly called for side-effects of erroring when path is not root directory of
#' an R package.
#'
#' @param max_subdirs Maximum number of sub-directories to recurse through to
#' find package, if it is not in root directory.
#' @noRd
convert_path <- function (path = ".", max_subdirs = 2L) {

    path <- fs::path_norm (path)

    proj_root <- tryCatch (
        rprojroot::find_package_root_file (path = path),
        error = function (e) NULL
    )
    if (is.null (proj_root)) {
        subdirs <- fs::dir_ls (path, type = "directory", recurse = max_subdirs)
        proj_root <- lapply (subdirs, function (d) {
            tryCatch (
                rprojroot::find_root (rprojroot::is_r_package, path = d),
                error = function (e) NULL
            )
        })
        proj_root <- unique (unlist (unname (proj_root)))
    }

    if (length (proj_root) != 1L) {
        cli::cli_abort (
            "Could not find unambiguous project root from {proj_root}"
        )
    }

    return (fs::path (proj_root))
}
