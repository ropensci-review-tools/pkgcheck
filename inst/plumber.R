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

    # check whether gp is cached:
    cmt <- get_latest_commit (org = org, repo = pkg_name)
    fname <- paste0 (pkg_name, "_", substring (cmt$oid, 1, 8))
    gp_cache_dir <- file.path (cache_dir, "gp_reports")
    gp_cache_file <- file.path (gp_cache_dir, fname)

    if (file.exists (gp_cache_file)) {

        gp <- readRDS (gp_cache_file)

    } else {

        if (!file.exists (gp_cache_dir))
            dir.create (gp_cache_dir, recursive = TRUE)

        withr::with_temp_libpaths ({
            remotes::install_local (local_repo,
                                    upgrade = "never",
                                    dependencies = TRUE)
            gp <- goodpractice::goodpractice (local_repo)
        })

        saveRDS (gp, gp_cache_file)
    }

    message ("unlinking ", local_repo)
    chk <- unlink (local_repo, recursive = TRUE)

    # -------------- extract components:
    # -------------- covr:
    covr <- capture.output (print (gp$covr),
                            type = "message")

    # -------------- cyclocomp:
    cyc <- gp$cyclocomp [which (gp$cyclocomp$cyclocomp > 1), ] # data.frame

    # -------------- lintr:
    lint_file <- vapply (gp$lintr, function (i) i$filename, character (1))
    lint_line <- vapply (gp$lintr, function (i) i$line_number, integer (1))
    lint_type <- vapply (gp$lintr, function (i) i$type, character (1))
    lint_message <- vapply (gp$lintr, function (i) i$message, character (1))
    lints <- data.frame (file = lint_file,
                         line = lint_line,
                         type = lint_type,
                         message = lint_message,
                         stringsAsFactors = FALSE)

    # -------------- rcmdcheck:
    r <- gp$rcmdcheck
    rcmd <- list ()
    if (length (r$errors) > 0)
        rcmd$error <- r$error
    if (length (r$warnings) > 0)
        rcmd$warnings <- r$warnings
    if (length (r$notes) > 0)
        rcmd$notes <- r$notes
    if (length (r$test_fail) > 0)
        rcmd$test_fail <- r$test_fail

    res <- list (rcmd = rcmd,
                 covr = covr,
                 cyclocomp = cyc,
                 lint = lints)

    return (jsonlite::toJSON (res))
}

#* @get /mean
function (n = 10) {

    return (mean (rnorm (n)))
}

#* Get log
#* @param n Get n latest log entries
#* @get /log
function (n = 10) {

    log_dir <- Sys.getenv ("log_dir")
    log_file <- Sys.getenv ("log_file")

    ret <- "no log file present"

    if (file.exists (log_file)) {
        ret <- readLines (log_file)
        ret <- rev (tail (ret, n))
    }

    return (ret)
}
