
#' Check whether the package depends on discouraged dependencies.
#'
#' @inheritParams pkg_has_no_scrap
#' @return Names of any packages which should not be depended upon; otherwise an empty
#' character.
#' @noRd
pkgchk_has_superseded_deps <- function (s) {

    deps <- c (s$desc$depends, s$desc$imports, s$desc$suggests)

    if (! any (deps %in% superseded_pkgs()$superseded))
        return (character (0)) # not NULL!

    present_superseded <- superseded_pkgs()[deps %in% superseded_pkgs()$superseded,]
    superseded_string <- sprintf (
        "%s (recommended: %s)",
        present_superseded$superseded,
        present_superseded$new
    )

    return(paste(superseded_string, collapse = ", "))
}

superseded_pkgs <- function () {
    data.frame(
        superseded = c("RCurl", "XML", "rjson", "RJSONIO"),
        new = c("curl, crul, httr or httr2", "xml2", "jsonlite", "jsonlite"),
        row.names = NULL,
        stringAsFactors = FALSE
    )
}

#' @return cross only
#' @noRd
summarise_superseded_checks <- function (checks) {

    ret <- NULL

    if (length (checks$superseded) > 0L) {

        ret <- paste0 ("- ", symbol_crs (),
                       " Package contains dependencies that are not recommended.")
    }

    return (ret)
}

print_superseded <- function (x) {

    if (length (x$superseded) == 0L)
        return (NULL)

    cli::cli_alert_danger (" Package contains the following unexpected dependencies:")
    cli::cli_ul ()
    cli::cli_li (x$superseded)
    cli::cli_end ()
}

superseded_checks_md <- function (checks) {

    if (length (checks$superseded) == 0L)
        return (NULL)

    c ("",
       paste0 (symbol_crs (),
               " Package contains the following unexpected dependencies:"),
       "",
       paste0 ("- ", checks$superseded),
       "")
}
