output_pkgchk_ci <- function (checks) {

    # Relevant issues: #87, #255

    has_badges <- length (checks$info$badges) > 0L
    check_pass <- has_workflows <- FALSE
    if (length (checks$info$github_workflows) > 0L) {
        wf <- checks$info$github_workflows
        i <- grep ("check|cmd", wf$name, ignore.case = TRUE)
        has_workflows <- length (i) > 0L
        check_pass <- any (wf$conclusion [i] == "success")
        check_pass <- ifelse (is.na (check_pass), FALSE, check_pass)
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
        } else if (!has_workflows) {
            out$summary <- " Package has no continuous integration checks."
        } else if (has_workflows && !check_pass) {
            out$summary <- " Package fails continuous integration checks."
            if (!has_badges) {
                out$summary <-
                    gsub ("\\.$", ", and has no badges on README", out$summary)
            }
        }

    } else {

        out$summary <- " Package has continuous integration checks."
        if (!has_badges) {
            out$summary <-
                gsub ("\\.$", ", but no badges on README", out$summary)
            checks$info$badges <- "(There do not appear to be any)"
        } else {
            has_badges <- !is.na (checks$info$badges [1])
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
