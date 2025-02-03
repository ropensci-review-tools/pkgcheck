#' Ensure that default GitHub branch is not "master"
#'
#' The `$info$git$branch` value is taken by default from GitHub as long as
#' "DESCRIPTION" has a remote URL. It is only taken from local git if not
#' remote GitHub URL can be identified.`
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return TRUE if default GitHub branch is "master"; otherwise FALSE
#' @noRd
pkgchk_branch_is_master <- function (checks) {


    ret <- FALSE
    if (length (checks$info$git) > 0L) {
        ret <- checks$info$git$branch == "master"
    }

    # Return true in test environments; see #215
    ret <- ret || is_test_env () # In R/utils.R

    return (ret)
}

output_pkgchk_branch_is_master <- function (checks) {

    out <- list (
        check_pass = !checks$checks$branch_is_master,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- "Default GitHub branch of 'master' is not acceptable."
    }

    return (out)
}
