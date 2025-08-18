#' Check and convert path arguments
#'
#' Mostly called for side-effects of erroring when path is not root directory of
#' an R package.
#' @noRd
convert_path <- function (path = ".") {
    x_if_not_null <- function(x, y) if (is.null(x)) y else x

    path <- fs::path_norm (path)

    # see also https://github.com/r-lib/usethis/blob/master/R/proj.R
    path <- x_if_not_null(tryCatch (
        rprojroot::find_root (rprojroot::is_git_root, path = path),
        error = function (e) NULL
    ), path)

    path <- x_if_not_null(
        tryCatch (
            rprojroot::find_package_root_file (path = path),
            error = function (e) NULL
        ),
        unlist (lapply (fs::dir_ls (path, type = "directory"), function (d) {
            tryCatch (
                rprojroot::find_root (rprojroot::is_r_package, path = d),
                error = function (e) NULL
            )
        }))
    )

    if (length (path) != 1L) {
        cli::cli_abort (
            "Could not find unambiguous project root from {path}"
        )
    }

    return (fs::path (path))
}
