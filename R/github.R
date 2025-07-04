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

#' Use pkgcheck Github Action
#'
#' Creates a Github workflow file in `dir` integrate [pkgcheck()] into your CI.
#'
#' @param dir Directory the file is written to.
#' @param overwrite Overwrite existing file?
#' @param file_name Name of the workflow file.
#' @param branch Name of git branch for checks to run; defaults to currently
#' active branch.
#' @param inputs Named list of inputs to the
#'   `ropensci-review-tools/pkgcheck-action`. See details below.
#' @return The path to the new file, invisibly.
#' @details For more information on the action and advanced usage visit the
#' action
#' [repository](https://github.com/ropensci-review-tools/pkgcheck-action).
#' @section Inputs:
#' Inputs with description and default values. Pass all values as strings, see
#' examples.
#'
#' ```yaml
#' inputs:
#'   ref:
#'     description: "The ref to checkout and check. Set to empty string to skip checkout."
#'     default: "${{ github.ref }}"
#'     required: true
#'   post-to-issue:
#'     description: "Should the pkgcheck results be posted as an issue?"
#'     # If you use the 'pull_request' trigger and the PR is from outside the repo
#'     # (e.g. a fork), the job will fail due to permission issues
#'     # if this is set to 'true'. The default will prevent this.
#'     default: ${{ github.event_name != 'pull_request' }}
#'     required: true
#'   issue-title:
#'     description: "Name for the issue containing the pkgcheck results. Will be created or updated."
#'     # This will create a new issue for every branch, set it to something fixed
#'     # to only create one issue that is updated via edits.
#'     default: "pkgcheck results - ${{ github.ref_name }}"
#'     required: true
#'   summary-only:
#'     description: "Only post the check summary to issue. Set to false to get the full results in the issue."
#'     default: true
#'     required: true
#'   append-to-issue:
#'     description: "Should issue results be appended to existing issue, or posted in new issues."
#'     default: true
#'     required: true
#' ```
#' @examples
#' \dontrun{
#' use_github_action_pkgcheck (inputs = list (`post-to-issue` = "false"))
#' use_github_action_pkgcheck (branch = "main")
#' }
#' @family github
#' @export
use_github_action_pkgcheck <- function (dir = ".github/workflows",
                                        overwrite = FALSE,
                                        file_name = "pkgcheck.yaml",
                                        branch = gert::git_branch (),
                                        inputs = NULL) {

    if (!is.character (file_name)) {
        cli::cli_abort ("{.arg file_name} must be a character argument")
    }
    if (length (file_name) != 1L) {
        cli::cli_abort ("{.arg file_name} must be a single value")
    }
    dir <- normalizePath (dir, mustWork = FALSE)
    if (!dir.exists (dir)) {
        dir.create (dir, recursive = TRUE)
    }
    path <- fs::path (dir, file_name)
    if (file.exists (path) && !overwrite) {
        cli::cli_abort (
            c (
                "The file {.file {path}} already exists!",
                i = "Use {.arg overwrite = TRUE} to replace the existing file."
            )
        )
    }

    yaml <- system.file (
        "pkgcheck.yaml",
        package = "pkgcheck",
        mustWork = TRUE
    ) %>% readLines ()

    if (!is.null (inputs)) {
        if (!is.list (inputs) || is.null (names (inputs))) {
            cli::cli_abort ("{.arg inputs} must be a named list!")
        }

        valid_inputs <- c (
            "ref",
            "post-to-issue",
            "issue-title",
            "summary-only"
        )
        broken_inputs <- !(names (inputs) %in% valid_inputs)

        if (any (broken_inputs)) {
            cli::cli_abort (
                c (
                    paste0 (
                        "The following {.arg inputs} are not valid: ",
                        "{ names (inputs)[broken_inputs] }"
                    ),
                    i = "Please check {.code ?use_github_check} for valid inputs."
                )
            )
        }

        # YAML indentation uses space not tabs
        with <- glue::glue ("        with:")
        inputs <- glue::glue ("          {names (inputs)}: {inputs}")
        inputs <- c (with, inputs)
    }

    yaml <- add_branch_to_yaml (yaml, branch)

    yaml <- c (yaml, inputs)

    writeLines (yaml, path)

    cli::cli_alert_success (
        "File {.file {file_name}} succesfully writen to {.path {dir}}!"
    )

    invisible (path)
}

#' Modify yaml action to specified branch.
#'
#' This does not work in covr on github, so need to disable there.
#' @param yaml The YAML template of the pkgcheck action
#' @inheritParams use_github_action_pkgcheck
#' @return Same as input with branch (potentially) modified to current branch.
#' @noRd
add_branch_to_yaml <- function (yaml, branch = gert::git_branch ()) {

    if (branch != "main") {

        i <- grep ("^\\s+branches\\:$", yaml)
        j <- grep ("^\\s+\\-\\s*\\w+$", yaml)
        j <- j [which (j > i)]
        j <- j [which (c (1L, diff (j)) == 1L)]
        branches <- gsub ("^\\s+\\-\\s+", "", yaml [j])
        if (!branch %in% branches) {
            j <- max (j)
            yaml <- c (
                yaml [seq (j)],
                paste0 (
                    paste0 (rep (" ", 6), collapse = ""),
                    "- ",
                    branch
                ),
                yaml [(j + 1):length (yaml)]
            )
        }
    }

    return (yaml)
}

#' Get branch specified in pkgcheck GitHub workflow, if one exists.
#'
#' @noRd
pkgcheck_workflow_branch <- function (org, repo) {

    default_branch <- get_default_github_branch (org, repo)

    branches <- ""

    u <- paste0 (
        "https://api.github.com/repos/",
        org,
        "/",
        repo,
        "/git/trees/",
        default_branch,
        "?recursive=1"
    )
    req <- httr2::request (u) %>%
        httr2::req_perform ()
    x <- httr2::resp_body_json (req)

    paths <- vapply (x$tree, function (i) i$path, character (1))
    workflows <- grep ("^\\.github\\/workflows", paths, value = TRUE)

    if (any (grepl ("pkgcheck", workflows))) {

        this <- workflows [grep ("pkgcheck", workflows) [1]]
        u_wf <- paste0 (
            "https://raw.githubusercontent.com/",
            org,
            "/",
            repo,
            "/",
            default_branch,
            "/",
            this
        )
        f <- file.path (tempdir (), basename (this))
        utils::download.file (u_wf, f, quiet = TRUE)
        yaml <- readLines (f)

        chk <- file.remove (f)

        i <- grep ("^\\s+branches\\:$", yaml)
        j <- grep ("^\\s+\\-\\s*\\w+$", yaml)
        j <- j [which (j > i)]
        j <- j [which (c (1L, diff (j)) == 1L)]
        branches <- gsub ("^\\s+\\-\\s+", "", yaml [j])
    }

    return (branches)
}
