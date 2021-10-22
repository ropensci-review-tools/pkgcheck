#' Check whether the package depends on superseded dependencies.
#'
#' @inheritParams pkgchk_has_citation
#' @noRd
pkgchk_has_superseded_deps <- function (checks) {

  dependencies <- checks$pkg$dependencies$package

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

output_pkgchk_has_superseded_deps <- function (checks) {

    out <- list (
        check_pass = length(checks$checks$has_superseded_deps) > 0,
        summary = "",
        print = ""
    )

    out$summary <- paste0 (
        ifelse (out$check_pass, "has", "does not have"),
        " no longer recommended dependencies."
    )
    if (!out$checl_pass) {
      out$print <- toString(checks$checks$has_superseded_deps)
    }

    return (out)
}
