
output_pkgchk_ci <- function (checks) {

    check_pass <- has_badges <- length (checks$info$badges) > 0L
    # There should really be badges, but if not, accept passing workflow results
    # regardless (see #87):
    if (!check_pass & length (checks$info$github_workflows) > 0L) {
        wf <- checks$info$github_workflows
        i <- grep ("check|cmd", wf$name, ignore.case = TRUE)
        check_pass <- any (wf$conclusion [i] == "success")
    }

    out <- list (
        check_pass = check_pass,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {

        if (!checks$checks$has_url) {
            out$summary <- paste0 (
                "Continuous integration checks ",
                "unavailable (no URL in 'DESCRIPTION')."
            )
        } else {
            out$summary <- " Package has no continuous integration checks."
        }

    } else {

        out$summary <- "Package has continuous integration checks."
        if (has_badges) {
            has_badges <- !is.na (checks$info$badges [1])
        }
        if (!has_badges) {
            checks$info$badges <- "(There do not appear to be any)"
        }

        out$print <- c (
            "#### 3a. Continuous Integration Badges",
            "",
            unlist (checks$info$badges),
            ""
        )

        if (!is.null (checks$info$github_workflows)) {
            out$print <- c (
                out$print,
                "**GitHub Workflow Results**",
                "",
                knitr::kable (checks$info$github_workflows)
            )
        }
    }


    return (out)
}
