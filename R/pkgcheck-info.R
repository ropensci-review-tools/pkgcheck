#' Population the 'info' part of the main return object.
#'
#' This function is (only) called within the main `pkgcheck()` function.
#'
#' @param checks `pkgcheck` object with values for "check" only. This function
#' populates the "info" component.
#' @param path Local path to repo.
#' @param stats Object returned from `pkgstats_info()` function.
#'
#' @return Modified version of input parameters `checks` with populated "info"
#' component.
#'
#' @noRd
pkgcheck_fill_info <- function (checks, path, stats) {

    info_items <- c ("fn_names", "git", "pkgstats")
    if ("srr" %in% names (stats$out)) {
        info_items <- c (info_items, "srr")
    }
    checks$info <- stats$out [info_items]

    checks$info$pkgdown_concepts <- pkginfo_pkgdown (path)
    checks$info$network_file <- fn_call_network (stats)
    checks$info$renv_activated <- pkginfo_renv_activated (path)

    u <- pkginfo_url_from_desc (path, type = "URL")
    # hard-code to extract github URLs only:
    if (!grepl ("github", u, ignore.case = TRUE) |
        grepl ("github\\.io", u, ignore.case = TRUE)) {
        u <- pkginfo_url_from_desc (path, type = "BugReports")
        if (grepl ("issues(\\/?)$", u)) {
            u <- gsub ("issues(\\/?)$", "", u)
        }
    }

    checks$info$badges <- list ()
    has_token <- length (get_gh_token ()) > 0L
    if (nzchar (u) & has_token) {
        checks$info$badges <- pkgchk_ci_badges (u)
        if (grepl ("github", u)) { # now redundant - remove!
            checks$info$github$workflows <- suppressWarnings (
                tryCatch (ci_results_gh (path), error = function (e) NULL)
            )
        }
    }

    return (checks)
}
