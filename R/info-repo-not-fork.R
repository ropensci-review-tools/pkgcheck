pkginfo_repo_not_fork <- function (checks) {

    checks$info$github$repo_not_fork <- TRUE

    u <- checks$pkg$url
    if (length (u) == 0L || !nzchar (u)) {
        return (checks)
    }

    or <- utils::tail (strsplit (u, "\\/") [[1]], 2L)
    org <- or [1L]
    repo <- or [2L]

    qry <- repo_is_fork_qry (org = org, repo = repo)
    x <- gh::gh_gql (qry)

    if (is.logical (x$data$repository$isFork)) {
        checks$info$github$repo_not_fork <- !x$data$repository$isFork
    }

    return (checks)
}
