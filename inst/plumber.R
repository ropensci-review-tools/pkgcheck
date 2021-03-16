# plumber.R

#* Return report from package URL
#* @param u The URL for a repo
#* @post /report
function (u) {

    pkg_name <- tail (strsplit (u, "/") [[1]], 1)
    org <- tail (strsplit (u, "/") [[1]], 2) [1]

    cache_dir <- Sys.getenv ("cache_dir")
    repo_updated <- pkgreport::check_cache (org, pkg_name, cache_dir)
    branch <- pkgreport::get_default_branch (org, pkg_name)

    download_repo <- function (u, pkg_name, branch) {

        if (substring (u, nchar (u), nchar (u)) == "/")
            u <- substring (u, 1, nchar (u) - 1)
        u <- paste0 (u, "/archive/", branch, ".zip")

        f <- file.path (cache_dir, paste0 (pkg_name, "-", branch, ".zip"))
        download.file (u, destfile = f)
        return (f)
    }

    local_repo <- file.path (cache_dir, paste0 (pkg_name, "-", branch, ""))
    local_zip <- paste0 (local_repo, ".zip")

    if (!file.exists (local_zip) | repo_updated) {

        message ("downloading repo ...")
        f <- download_repo (u, pkg_name, branch)
    }

    flist <- unzip (local_zip, exdir = cache_dir)
    g <- packgraph::pg_graph (local_repo, plot = FALSE)
    res <- packgraph::pg_report (g, exported_only = FALSE)

    message ("unlinking ", local_repo)
    chk <- unlink (local_repo, recursive = TRUE)

    return (res)
}

#* Return goodpractice results from a package URL
#* @param u The URL for a repo
#* @post /gp
function (u) {

    pkg_name <- tail (strsplit (u, "/") [[1]], 1)
    org <- tail (strsplit (u, "/") [[1]], 2) [1]

    cache_dir <- Sys.getenv ("cache_dir")
    repo_updated <- pkgreport::check_cache (org, pkg_name, cache_dir)
    branch <- pkgreport::get_default_branch (org, pkg_name)

    download_repo <- function (u, pkg_name, branch) {

        if (substring (u, nchar (u), nchar (u)) == "/")
            u <- substring (u, 1, nchar (u) - 1)
        u <- paste0 (u, "/archive/", branch, ".zip")

        f <- file.path (cache_dir, paste0 (pkg_name, "-", branch, ".zip"))
        download.file (u, destfile = f)
        return (f)
    }

    local_repo <- file.path (cache_dir, paste0 (pkg_name, "-", branch, ""))
    local_zip <- paste0 (local_repo, ".zip")

    if (!file.exists (local_zip) | repo_updated) {

        message ("downloading repo ...")
        f <- download_repo (u, pkg_name, branch)
    }

    flist <- unzip (local_zip, exdir = cache_dir)

    gp <- goodpractice::goodpractice (local_repo)

    message ("unlinking ", local_repo)
    chk <- unlink (local_repo, recursive = TRUE)

    # -------------- extract components:
    covr <- capture.output (print (gp$covr),
                            type = "message")

    cyc <- gp$cyclocomp [which (gp$cyclocomp$cyclocomp > 1), ] # data.frame

    lint_file <- vapply (gp$lintr, function (i) i$filename, character (1))
    lint_line <- vapply (gp$lintr, function (i) i$line_number, integer (1))
    lint_type <- vapply (gp$lintr, function (i) i$type, character (1))
    lint_message <- vapply (gp$lintr, function (i) i$message, character (1))
    lints <- data.frame (file = lint_file,
                         line = lint_line,
                         type = lint_type,
                         message = lint_message,
                         stringsAsFactors = FALSE)

    res <- list (covr = covr,
                 cyclocomp = cyc,
                 lint = lints)

    return (jsonlite::toJSON (res))
}

#* @get /mean
function (n = 10) {

    return (mean (rnorm (n)))
}
