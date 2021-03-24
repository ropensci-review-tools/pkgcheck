#' Download a GitHub repo to local cache
#'
#' @param u URL of GitHub repository
#' @return Path to locally cached '.zip' version of repository
#' @export
dl_gh_repo <- function (u) {

    repo <- tail (strsplit (u, "/") [[1]], 1)
    org <- tail (strsplit (u, "/") [[1]], 2) [1]

    cache_dir <- Sys.getenv ("cache_dir")
    repo_updated <- pkgreport::check_cache (org, repo, cache_dir)
    branch <- pkgreport::get_default_branch (org, repo)

    download_repo <- function (u, repo, branch) {

        if (substring (u, nchar (u), nchar (u)) == "/")
            u <- substring (u, 1, nchar (u) - 1)
        u <- paste0 (u, "/archive/", branch, ".zip")

        f <- file.path (cache_dir, paste0 (repo, "-", branch, ".zip"))
        download.file (u, destfile = f)
        return (f)
    }

    local_repo <- file.path (cache_dir, paste0 (repo, "-", branch, ""))
    local_zip <- paste0 (local_repo, ".zip")

    if (!file.exists (local_zip) | repo_updated) {

        message ("downloading repo ...")
        f <- download_repo (u, repo, branch)
    }

    return (local_repo)
}
