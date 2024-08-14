#' rOpenSci-internal function to update \pkg{pkgstats} data.
#'
#' Calling this function will create an updated version of this repository in
#' the R temporary directory, and have no other effect for unauthorized users.
#'
#' @export
pkgcheck_update_pkgstats_data <- function () {

    pkgstats_remote <- "https://github.com/ropensci-review-tools/pkgstats/"
    path <- dl_pkgstats_current (pkgstats_remote)
    pkgstats_data <- readRDS (path)

    repo_path <- clone_pkgcheck_repo ()
    data_path <- fs::path (repo_path, "data", "pkgstats_data.rda")
    stopifnot (fs::file_exists (data_path))

    save (pkgstats_data, file = data_path, compress = "bzip2")

    gert::git_add (files = "data/pkgstats_data.rda", repo = repo_path)
    msg <- paste0 (
        "pkgstats_data update [",
        format (Sys.time (), "%Y-%m-%d %H:%M:%S"),
        "]"
    )
    gert::git_commit (message = msg, repo = repo_path)
    gert::git_push (remote = pkgstats_remote)
}

dl_pkgstats_current <- function (pkgstast_remote) {

    u_tag <- pkgstats:::RELEASE_TAG
    u_base <- paste0 (pkgstats_remote, "releases/download/", u_tag, "/")
    f_name <- "pkgstats-CRAN-current.Rds"
    url <- paste0 (u_base, f_name)

    path <- fs::path (fs::path_temp (), f_name)

    if (fs::file_exists (path)) {
        return (path)
    }

    req <- httr2::request (url) |>
        httr2::req_headers ("Accept" = "application/octet-stream")
    resp <- httr2::req_perform (req)

    if (httr2::resp_is_error (resp)) {
        return (NULL)
    }

    writeBin (httr2::resp_body_raw (resp), path)

    return (path)
}

clone_pkgcheck_repo <- function () {

    repo <- "https://github.com/ropensci-review-tools/pkgcheck"
    path <- fs::path (fs::path_temp (), "pkgcheck")
    gert::git_clone (url = repo, path = path)
}
