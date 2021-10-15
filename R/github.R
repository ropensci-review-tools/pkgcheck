
default_branch_qry <- function (gh_cli, org, repo) {

    q <- paste0 ("{
            repository(owner:\"", org, "\", name:\"", repo, "\") {
                       defaultBranchRef {
                           name
                       }
                    }
            }")

    qry <- ghql::Query$new ()
    qry$query ("default_branch", q)

    return (qry)
}

commits_qry <- function (gh_cli, org, repo, branch = "main") {

    q <- paste0 ("{
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

    qry <- ghql::Query$new ()
    qry$query ("get_commits", q)

    return (qry)
}

#' Get GitHub token
#'
#' @param token_name Optional name of token to use
#' @return The value of the GitHub access token extracted from environment
#' variables.
#' @family github
#' @export
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

#' get_default_branch
#'
#' @note This function is not intended to be called directly, and is only
#' exported to enable it to be used within the \pkg{plumber} API.
#'
#' @param org Github organization
#' @param repo Github repository
#' @return Name of default branch on GitHub
#' @family github
#' @export
get_default_branch <- function (org, repo) {

    token <- get_gh_token ()

    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )

    qry <- default_branch_qry (gh_cli, org = org, repo = repo)
    x <- gh_cli$exec (qry$queries$default_branch) %>%
        jsonlite::fromJSON ()
    branch <- x$data$repository$defaultBranchRef$name

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
#' @return Details of latest commit including OID hash
#' @family github
#' @export
get_latest_commit <- function (org, repo) {

    token <- get_gh_token ()

    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )

    branch <- get_default_branch (org, repo)

    qry <- commits_qry (gh_cli, org = org, repo = repo, branch = branch)
    x <- gh_cli$exec (qry$queries$get_commits) %>%
        jsonlite::fromJSON ()

    return (x$data$repository$branch0$target$history$nodes)
}
