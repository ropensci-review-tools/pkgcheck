
repo_is_git <- function (path) {

    path <- convert_path (path)

    g <- tryCatch (
        gert::git_find (path),
        error = function (e) e
    )

    return (!methods::is (g, "libgit2_error"))
}

#' Return the $git item of main pkgcheck return result
#'
#' Note the prefix is `pkgcheck`, not `pkgchk_`: This is not a check, just a
#' function to return summary data.
#' @noRd
pkginfo_git_info <- function (path) {

    path <- convert_path (path)

    u <- pkginfo_url_from_desc (path)

    branch <- NULL

    if (length (u) > 0L) {

        repo <- utils::tail (strsplit (u, "/") [[1]], 1)
        org <- utils::tail (strsplit (u, "/") [[1]], 2) [1]
        has_token <- length (get_gh_token ()) > 0L
        if (curl::has_internet () & has_token) {
            branch <- get_default_branch (org, repo)
        }
    }

    ret <- list ()

    if (repo_is_git (path)) {

        gitlog <- gert::git_log (repo = path, max = 1e6)

        # use email addresses to identify unique authors
        auts <- gsub ("^.*<|>$", "", unique (gitlog$author))

        if (is.null (branch)) { # no remote, so assume local head

            branch <- gert::git_info (path)$shorthand
        }

        ret <- list (
            HEAD = gitlog$commit [1],
            branch = branch,
            num_commits = nrow (gitlog),
            since = min (gitlog$time),
            num_authors = length (unique (auts))
        )
    }

    return (ret)
}
