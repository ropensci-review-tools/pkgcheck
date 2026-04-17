pkginfo_github <- function (checks) {

    checks$info$github$repo_not_fork <- TRUE
    checks$info$github$repo_has_website <- TRUE

    u <- checks$pkg$url
    if (length (u) == 0L || !nzchar (u)) {
        return (checks)
    }

    or <- utils::tail (strsplit (u, "\\/") [[1]], 2L)
    org <- or [1L]
    repo <- or [2L]

    qry <- repo_info_qry (org = org, repo = repo)
    x <- gh::gh_gql (qry)

    if (is.logical (x$data$repository$isFork)) {
        checks$info$github$repo_not_fork <- !x$data$repository$isFork
    }
    website <- x$data$repository$homepageUrl
    if (length (website) == 0L) {
        checks$info$github$repo_has_website <- FALSE
    } else {
        checks$info$github$repo_has_website <-
            grepl ("^http(s?)\\:\\/", website)
    }

    return (checks)
}
