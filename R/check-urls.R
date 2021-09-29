
pkgchk_has_url <- function (checks) {

    is.na (checks$package$url)
}

pkgchk_has_bugs <- function (checks) {

    !is.na (s$stats$desc$bugs)
}

output_pkgchk_has_url <- function (checks) {

    out <- list (check_pass = checks$checks$has_url,
                summary = "",
                print = "") # no print method

    out$summary <- paste0 ("'DESCRIPTION' ",
                           ifelse (out$check_pass, "has", "does not have"),
                           " a URL field.")

    return (out)
}

output_pkgchk_has_bugs <- function (checks) {

    out <- list (check_pass = checks$checks$has_bugs,
                summary = "",
                print = "") # no print method

    out$summary <- paste0 ("'DESCRIPTION' ",
                           ifelse (out$check_pass, "has", "does not have"),
                           " a BugReports field.")

    return (out)
}
