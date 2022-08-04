
#' Check whether renv is activated
#'
#' The actual check is whether \package{renv} is actually activated, which is
#' done by adding a line to a local `.Rprofile` file, `source("renv/init.R")`.
#'
#' @param path Location of local repository
#'
#' @noRd
pkginfo_renv_activated <- function (path) {

    files <- list.files (
        path,
        pattern = "renv\\.lock$",
        full.names = TRUE
    )

    if (length (files) == 0L) {
        return (FALSE)
    }

    rprof <- list.files (
        path,
        all.files = TRUE,
        pattern = "^\\.Rprofile$",
        full.names = TRUE
    )
    if (length (rprof) == 0L) {
        return (FALSE)
    }

    rprof <- readLines (rprof)
    # from renv/R/infrastructure.R + json.R
    renv_path <- encodeString ("renv/activate.R", quote = "\"", justify = "none")
    ptn <- sprintf ("source(%s)", renv_path)
    source_line <- grep (ptn, rprof, fixed = TRUE, value = TRUE)

    if (length (source_line) != 1L) { # only ever 0 or 1
        return (FALSE)
    }

    return (!grepl ("^(\\s*?)#", source_line))
}

#' Internal implementation of `renv::deactivate()`
#'
#' Source is
#' \url{https://github.com/rstudio/renv/blob/main/R/deactivate.R},
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
