#' Get 'goodpractice' report on local source package
#'
#' This uses a caching system to only generate new report if repository has been
#' updated, otherwise it returns locally cached version.
#'
#' @param local_repo Path to local source of repository
#' @return A \pkg{goodpractice} report
#' @noRd
get_gp_report <- function (local_repo) {

    u <- url_from_desc (local_repo)

    repo <- utils::tail (strsplit (u, "/") [[1]], 1)
    org <- utils::tail (strsplit (u, "/") [[1]], 2) [1]

    # check whether gp is cached:
    cmt <- get_latest_commit (org = org, repo = repo)
    fname <- paste0 (repo, "_", substring (cmt$oid, 1, 8))
    gp_cache_dir <- file.path (getOption ("pkgcheck.cache_dir"),
                               "gp_reports")
    gp_cache_file <- file.path (gp_cache_dir, fname)

    if (file.exists (gp_cache_file)) {

        gp <- readRDS (gp_cache_file)

    } else {

        if (!file.exists (gp_cache_dir))
            dir.create (gp_cache_dir, recursive = TRUE)

        gp <- goodpractice::goodpractice (local_repo)

        saveRDS (gp, gp_cache_file)
    }

    return (gp)
}
