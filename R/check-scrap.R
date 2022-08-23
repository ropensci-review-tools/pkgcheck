# https://github.com/yihui/knitr-examples/blob/master/113-externalization.Rmd

# ---- pkgchk-scrap ----
#' Check whether the package contains any useless files like `.DS_Store`.
#'
#' Files currently considered "scrap" are:
#'
#' 1. ".DS_Store"
#' 2. "Thumbs.db"
#' 3. ".vscode"
#' 4. ".o" files
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Names of any items which should not be present; otherwise an empty
#' character.
#' @noRd
pkgchk_has_scrap <- function (checks) {

    # Have to tryCatch because gert errors anywhere other than a git repo. This
    # means scrap can only be detected in git repos.
    contents <- tryCatch (gert::git_ls (repo = checks$pkg$path)$path,
        error = function (e) NULL
    )

    if (is.null (contents)) {
        return (character (0))
    } # not NULL!

    contents_short <- vapply (
        decompose_path (contents),
        function (i) utils::tail (i, 1L),
        character (1)
    )

    scrap <- function () {
        paste0 (c (
            "^\\.DS_Store$",
            "^Thumbs.db$",
            "^\\.vscode$",
            "\\.o$"
        ),
        collapse = "|"
        )
    }

    return (contents [grep (scrap (), contents_short)])
}

# ---- output-pkgchk-scrap ----
output_pkgchk_has_scrap <- function (checks) {

    out <- list (
        check_pass = length (checks$checks$has_scrap) == 0L,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- "Package contains unexpected files."
        out$print <- list (
            msg_pre = paste0 (
                "Package contains the ",
                "following unexpected files:"
            ),
            obj = checks$checks$has_scrap,
            msg_post = character (0)
        )
    }

    return (out)
}
