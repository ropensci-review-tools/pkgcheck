# cache latest commit oids from github.R functions


#' check_cache
#'
#' Check whether a package has been cached, and if so, whether commits have been
#' added to the github repo since cached version.
#'
#' @note This function is not intended to be called directly, and is only
#' exported to enable it to be used within the \pkg{plumber} API.
#'
#' @param org Github organization
#' @param repo Github repository
#' @param cache_dir Directory in which packages are cached
#' @return FALSE If a package has previously been cached, and the github repo
#' has not changed; otherwise TRUE.
#' @export
check_cache <- function (org, repo, cache_dir = tempdir ()) {
    cmt <- get_latest_commit (org = org, repo = repo)
    f <- file.path (cache_dir, "commit_oids.Rds")
    if (file.exists (f)) {
        dat <- readRDS (f)
    } else {
        dat <- data.frame (matrix (nrow = 0, ncol = 3))
        names (dat) <- c ("orgrepo", "oid", "authoredDate")
    }

    orgrepo <- paste0 (org, "/", repo)
    updated <- FALSE
    if (orgrepo %in% dat$orgrepo) {
        if (dat$oid [dat$orgrepo == orgrepo] != cmt$oid) {
            dat$oid [dat$orgrepo == orgrepo] <- cmt$oid
            dat$authoredDate [dat$orgrepo == orgrepo] <- cmt$authoredDate
            saveRDS (dat, f)
            updated <- TRUE
        }
    } else {
        dat <- rbind (dat, data.frame (orgrepo = orgrepo,
                                       oid = cmt$oid,
                                       authoredDate = cmt$authoredDate))
        saveRDS (dat, f)
        updated <- TRUE
    }
    return (updated)
}
