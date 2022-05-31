
#' Check for renv files in package directory
#'
#' @param path Location of local repository
#'
#' @noRd
pkginfo_uses_renv <- function (path) {

    files <- list.files (
        path,
        pattern = "renv\\.lock$",
        full.names = TRUE
    )

    return (length (files) > 0L)
}

#' Internal implementation of `renv::deactivate()`
#'
#' Source is
#' \url{https://github.com/rstudio/renv/blob/main/R/deactivate.R),
#' which directly calls
#' `renv_infrastructure_remove_rprofile()` in
#' \url{https://github.com/rstudio/renv/blob/main/R/infrastructure.R}.
#' @noRd
renv_deactivate <- function (path) {

    file <- list.files (
        path,
        all.files = TRUE,
        pattern = "\\.Rprofile$",
        full.names = TRUE
    )
    if (!file.exists (file)) {
        return ()
    }

    contents <- readLines (file)

    # Note this is a simplified pattern, and not the original!
    pattern <- sprintf ("^\\s*\\Q%s\\E\\s*", "source(\"renv")
    matches <- grepl (pattern, contents, perl = TRUE)

    rest <- contents [!matches]
    if (all (grepl ("^\\s*$", rest))) {
        return (unlink (file))
    }

    replacement <- gsub ("^(\\s*)", "\\1# ", contents [matches], perl = TRUE)
    contents [matches] <- replacement

    writeLines (contents, file)
}
