
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

output_pkgchk_has_scrap <- function (checks) {

    out <- list (check_pass = length (checks$checks$scrap) == 0L,
                summary = "",
                print = "")

    if (!out$check_pass) {

        out$summary <- "Package contains unexpected files."
        out$print <- list (message = paste0 ("Package contains the ",
                                             "following unexpected files:"),
                           obj = checks$checks$scrap)
    }

    return (out)
}
