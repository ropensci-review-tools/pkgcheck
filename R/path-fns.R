#' Check and convert path arguments
#'
#' Mostly called for side-effects of erroring when path is not root directory of
#' an R package.
#' @noRd
convert_path <- function (path = ".") {

    path <- fs::path_norm (path)

    # see also https://github.com/r-lib/usethis/blob/master/R/proj.R
    git_root <- tryCatch (
        rprojroot::find_root (rprojroot::is_git_root, path = path),
        error = function (e) NULL
    )

    if (!is.null (git_root)) {
        path <- git_root
    }

    r_root <- tryCatch (
        rprojroot::find_root (rprojroot::is_r_package, path = path),
        error = function (e) NULL
    )
    if (is.null (r_root)) {

        subdirs <- fs::dir_ls (path, type = "directory")
        r_root <- vapply (subdirs, function (d) {
            tryCatch (
                rprojroot::find_root (rprojroot::is_r_package, path = d),
                error = function (e) ""
            )
        }, character (1L), USE.NAMES = FALSE)
        r_root <- r_root [which (nzchar (r_root))]

        if (length (r_root) != 1L) {
            cli::cli_abort (paste0 (
                "Unable to determine unambiguous ",
                "R source directory from [{r_root}]"
            ))
        }
    }



    return (r_root)
}
