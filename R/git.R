
#' Return the $git item of main pkgcheck return result
#' @noRd
get_git_info <- function (path) {

    u <- url_from_desc (path)

    branch <- NULL

    if (!is.null (u)) {

        repo <- utils::tail (strsplit (u, "/") [[1]], 1)
        org <- utils::tail (strsplit (u, "/") [[1]], 2) [1]
        branch <- get_default_branch (org, repo)
    }

    g <- tryCatch (gert::git_find (path),
                   error = function (e) e)

    ret <- list ()

    if (!methods::is (g, "libgit2_error")) { # is a git repo

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
