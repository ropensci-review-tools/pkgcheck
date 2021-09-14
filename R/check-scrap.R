
#' Check whether the package contains any useless files like `.DS_Store`.
#'
#' @inheritParams pkg_has_no_scrap
#' @return Names of any items which should not be present; otherwise an empty
#' character.
#' @noRd
pkgchk_has_scrap <- function (path) {

    # Have to tryCatch because gert errors anywhere other than a git repo. This
    # means scrap can only be detected in git repos.
    all_contents <- tryCatch (gert::git_ls ()$path,
                              error = function (e) NULL)

    if (is.null (all_contents))
        return (character (0)) # not NULL!

    all_contents <- vapply (decompose_path (all_contents),
                            function (i) utils::tail (i, 1L),
                            character (1))

    scrap <- function() paste0 (c ("^\\.DS_Store$",
                                   "^Thumbs.db$",
                                   "^\\.vscode$",
                                   "\\.o$"),
                                collapse = "|")

    return (grep (scrap (), all_contents, value = TRUE))
}

#' @return cross only
#' @noRd
summarise_scrap_checks <- function (checks) {

    ret <- NULL

    if (length (checks$scrap) > 0L) {

        ret <- paste0 ("- ", symbol_crs (),
                       " Package contains unexpected files.")
    }

    return (ret)
}

print_scrap <- function (x) {

    if (length (x$scrap) == 0L)
        return (NULL)

    cli::cli_alert_danger (" Package contains the following unexpected files:")
    cli::cli_ul ()
    cli::cli_li (x$scrap)
    cli::cli_end ()
}

scrap_checks_md <- function (checks) {

    if (length (checks$scrap) == 0L)
        return (NULL)

    c ("",
       paste0 (symbol_crs (),
               " Package contains the following unexpected files:"),
       "",
       paste0 ("- ", checks$scrap),
       "")
}
