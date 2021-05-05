
get_gh_token <- function (token = "") {

    e <- Sys.getenv ()
    if (token != "") {

        toks <- e [grep (token, names (e))]

    } else {

        toks <- e [grep ("GITHUB", names (e))]
        if (length (toks) > 1)
            toks <- toks [grep ("QL", names (toks))]
    }

    if (length (unique (toks)) > 1)
        stop (paste0 ("No unambiguous token found; please use ",
                      "Sys.setenv() to set a github graphQL tokan ",
                      "named 'GITHUB', 'GITHUBQL', or similar"))
    return (unique (toks))
}

default_branch_qry <- function (gh_cli, org, repo) {

    q <- paste0 ("{
            repository(owner:\"", org, "\", name:\"", repo, "\") {
                       defaultBranchRef {
                           name
                       }
                    }
            }")

    qry <- ghql::Query$new()
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

    qry <- ghql::Query$new()
    qry$query ("get_commits", q)

    return (qry)
}

#' get_default_branch
#'
#' @note This function is not intended to be called directly, and is only
#' exported to enable it to be used within the \pkg{plumber} API.
#'
#' @param org Github organization
#' @param repo Github repository
#' @return Name of default branch on GitHub
#' @export
get_default_branch <- function (org, repo) {

    token <- get_gh_token ()

    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )

    qry <- default_branch_qry (gh_cli, org = org, repo = repo)
    x <- gh_cli$exec(qry$queries$default_branch) %>%
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
#' @export
get_latest_commit <- function (org, repo) {

    token <- get_gh_token ()

    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )

    branch <- get_default_branch (org, repo)

    qry <- commits_qry (gh_cli, org = org, repo = repo, branch = branch)
    x <- gh_cli$exec(qry$queries$get_commits) %>%
        jsonlite::fromJSON ()

    return (x$data$repository$branch0$target$history$nodes)
}
