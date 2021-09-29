
output_pkgchk_ci <- function (checks) {

    out <- list (check_pass = length (checks$info$badges) > 0L,
                summary = "",
                print = "")

    if (!out$check_pass) {

        if (!checks$checks$has_url)
            out$summary <- paste0 ("Continuous integration checks ",
                                   "unavailable (no URL in 'DESCRIPTION').")
        else
            out$summary <- " Package has no continuous integration checks."

    } else {

        out$summary <- "Package has continuous integration checks."
        if (is.na (checks$info$badges [1]))
            checks$info$badges <- "(There do not appear to be any)"

        out$print <- c ("### 3a. Continuous Integration Badges",
                        "",
                        unlist (checks$info$badges),
                        "")

        if (!is.null (checks$info$github_workflows)) {

            out$print <- c (out$print,
                            "**GitHub Workflow Results**",
                            "",
                            knitr::kable (checks$info$github_workflows))
        }
    }


    return (out)
}
