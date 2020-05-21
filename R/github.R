
get_gh_token <- function (token = "") {
    e <- Sys.getenv ()
    if (token != "")
        toks <- e [grep (token, names (e))]
    else {
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

get_qry <- function (gh_cli, org, repo, endCursor = NULL, branch = "master") {
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
    qry$query('get_commits', q)

    return (qry)
}

get_latest_commit <- function (org, repo) {
    token <- get_gh_token ()

    gh_cli <- ghql::GraphqlClient$new (
        url = "https://api.github.com/graphql",
        headers = list (Authorization = paste0 ("Bearer ", token))
    )


    qry <- get_qry (gh_cli, org = org, repo = repo)
    x <- gh_cli$exec(qry$queries$get_commits) %>%
        jsonlite::fromJSON ()

    return (x$data$repository$branch0$target$history$nodes)
}

