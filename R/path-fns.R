#' Check and convert path arguments
#'
#' Mostly called for side-effects of erroring when path is not root directory of
#' an R package.
#' @noRd
convert_path <- function (path = ".") {

    path <- fs::path_norm (path)

    parent <- tryCatch (
        rprojroot::find_package_root_file (path = path),
        error = function (e) NULL
    )

    path <- if (is.null(parent)) unlist (lapply (
        fs::dir_ls (path, type = "directory"),
        function (child) {
            tryCatch (
                rprojroot::find_root (rprojroot::is_r_package, path = child),
                error = function (e) NULL
            )
        }
    )) else parent

    if (length (path) != 1L) {
        cli::cli_abort (
            "Could not find unambiguous project root from {path}"
        )
    }

    return (fs::path (path))
}
