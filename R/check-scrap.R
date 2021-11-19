# https://github.com/yihui/knitr-examples/blob/master/113-externalization.Rmd

# ---- pkgchk-scrap ----
#' Check whether the package contains any useless files like `.DS_Store`.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Names of any items which should not be present; otherwise an empty
#' character.
#' @noRd
pkgchk_has_scrap <- function (checks) {

    # Have to tryCatch because gert errors anywhere other than a git repo. This
    # means scrap can only be detected in git repos.
    all_contents <- tryCatch (gert::git_ls (repo = checks$pkg$path)$path,
        error = function (e) NULL
    )

    if (is.null (all_contents)) {
        return (character (0))
    } # not NULL!

    all_contents <- vapply (
        decompose_path (all_contents),
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

    return (grep (scrap (), all_contents, value = TRUE))
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
