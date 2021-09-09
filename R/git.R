
repo_is_git <- function (path) {

    g <- tryCatch (gert::git_find (path),
                   error = function (e) e)

    return (!methods::is (g, "libgit2_error"))
}

#' Return the $git item of main pkgcheck return result
#' @noRd
get_git_info <- function (path) {

    u <- pkgchk_url_from_desc (path)

    branch <- NULL

    if (!is.null (u)) {

        repo <- utils::tail (strsplit (u, "/") [[1]], 1)
        org <- utils::tail (strsplit (u, "/") [[1]], 2) [1]
        branch <- get_default_branch (org, repo)
    }

    ret <- list ()

    if (repo_is_git (path)) {

        gitlog <- gert::git_log (repo = path, max = 1e6)

        # use email addresses to identify unique authors
        auts <- gsub ("^.*<|>$", "", unique (gitlog$author))

        if (is.null (branch)) { # no remote, so assume local head

            branch <- gert::git_info (path)$shorthand
        }

        ret <- list (HEAD = gitlog$commit [1],
                     branch = branch,
                     num_commits = nrow (gitlog),
                     since = min (gitlog$time),
                     num_authors = length (unique (auts)))
    }

    return (ret)
}
