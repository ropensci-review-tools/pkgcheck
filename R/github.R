default_branch_qry <- function (org, repo) {

    paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
            defaultBranchRef {
                name
            }
        }
    }")
}

commits_qry <- function (org, repo, branch = "main") {

    paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
            branch0: ref(qualifiedName: \"", branch, "\") {
                target {
                    ... on Commit {
                        id
                        history (first: 1) {
                            nodes {
                            ... on Commit {
                                oid
                                additions
                                deletions
                                authoredDate
                                }
                            }
                        }
                    }
                }
            }
        }
    }")
}

repo_info_qry <- function (org, repo) {

    paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
            isFork
            homepageUrl
        }
    }")
}

#' get_default_github_branch
#'
#' @note This function is not intended to be called directly, and is only
#' exported to enable it to be used within the \pkg{plumber} API.
#'
#' @param org Github organization
#' @param repo Github repository
#' @return Name of default branch on GitHub
#' @family github
#' @export
#' @examples
#' org <- "ropensci-review-tools"
#' repo <- "pkgcheck"
#' \donttest{
#' branch <- get_default_github_branch (org, repo)
#' }
get_default_github_branch <- function (org, repo) {

    qry <- default_branch_qry (org = org, repo = repo)
    x <- gh::gh_gql (qry)
    branch <- x$data$repository$defaultBranchRef$name

    # Then also check if repo has pkgcheck action yaml file:

    return (branch)
}

#' get_latest_commit
#'
#' @note This returns the latest commit from the default branch as specified on
#' GitHub, which will not necessarily be the same as information returned from
#' `gert::git_info` if the `HEAD` of a local repository does not point to the
#' same default branch.
#'
#' @param org Github organization
#' @param repo Github repository
#' @param branch Branch from which to get latest commit
#' @return Details of latest commit including OID hash
#' @family github
#' @export
#' @examples
#' org <- "ropensci-review-tools"
#' repo <- "pkgcheck"
#' \donttest{
#' commit <- get_latest_commit (org, repo)
#' }
get_latest_commit <- function (org, repo, branch = NULL) {

    if (is.null (branch)) {
        branch <- get_default_github_branch (org, repo)
    }

    qry <- commits_qry (org = org, repo = repo, branch = branch)
    x <- gh::gh_gql (qry)

    return (x$data$repository$branch0$target$history$nodes)
}
