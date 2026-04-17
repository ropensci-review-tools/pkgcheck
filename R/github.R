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

repo_is_fork_qry <- function (org, repo) {

    paste0 ("{
        repository(owner:\"", org, "\", name:\"", repo, "\") {
            isFork
        }
    }")
}

#' Get GitHub token
#'
#' @param token_name Optional name of token to use
#' @return The value of the GitHub access token extracted from environment
#' variables.
#' @family github
#' @export
#' @examples
#' \dontrun{
#' token <- get_gh_token ()
#' }
get_gh_token <- function (token_name = "") {

    e <- Sys.getenv ()

    if (token_name != "") {

        toks <- unique (e [grep (token_name, names (e))])
    } else {

        toks <- e [grep ("GITHUB", names (e))]
        if (length (unique (toks)) > 1) {
            toks <- toks [grep ("TOKEN|PAT", names (toks))]
        }
        # GitHub runners have "GITHUB_PATH" and "GITHUB_EVENT_PATH"
        if (length (unique (toks)) > 1) {
            toks <- toks [grep ("TOKEN$|PAT$", names (toks))]
        }
    }

    if (length (unique (toks)) > 1) {

        stop (
            "There are ",
            length (unique (toks)),
            " possible tokens named [",
            paste0 (names (toks), collapse = ", "),
            "]; please ensure one distinct ",
            "token named 'GITHUB_TOKEN' or similar."
        )
    }

    return (unique (toks))
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
#' \dontrun{
#' org <- "ropensci-review-tools"
#' repo <- "pkgcheck"
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
#' \dontrun{
#' org <- "ropensci-review-tools"
#' repo <- "pkgcheck"
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
