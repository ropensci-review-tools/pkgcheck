# plumber.R

# --------------------------------------------------
# ------------------   pkgstats   ------------------
# --------------------------------------------------

#* Return report from package URL
#* @param u The URL for a repo
#* @post /report
function (u) {

    cache_dir <- Sys.getenv ("cache_dir")
    local_repo <- pkgreport::dl_gh_repo (u)
    local_zip <- paste0 (local_repo, ".zip")
    flist <- unzip (local_zip, exdir = cache_dir)

    # check and modify permissions of configure files
    if (any (list.files (local_repo) == "configure")) {
        cmd <- paste0 ("chmod +755 ", file.path (local_repo, "configure"))
        system (cmd)
    }

    visjs_dir <- file.path (cache_dir, "static") # in api.R
    repo <- tail (strsplit (u, "/") [[1]], 1)
    org <- tail (strsplit (u, "/") [[1]], 2) [1]
    commit <- pkgreport::get_latest_commit (org, repo)
    oid <- substring (commit$oid, 1, 8)
    visjs_file <- paste0 (repo, "_", oid, ".html")

    # pg_graph directly calls pkgapi::map_package which requires all system deps
    pkgreport::pkgrep_install_deps (
                        local_repo = local_repo,
                        os = Sys.getenv ("pkgreport_os"),
                        os_release = Sys.getenv ("pkgreport_os_release"))

    g <- packgraph::pg_graph (local_repo,
                              vis_save = file.path (visjs_dir, visjs_file))
    res <- packgraph::pg_report (g)


    visjs_url <- paste0 ("http://127.0.0.1:8000/assets/", visjs_file)

    res <- c (res,
              "",
              paste0 ("[Click here](",
                      visjs_url,
                      ") for interactive visualisation of network"))

    message ("unlinking ", local_repo)
    chk <- unlink (local_repo, recursive = TRUE)

    res <- paste0 (res, collapse = "\n")

    return (res)
}

# --------------------------------------------------
# ---------------------   gp   ---------------------
# --------------------------------------------------

#* Return goodpractice results from a package URL
#* @param u The URL for a repo
#* @post /gp
function (u) {

    cache_dir <- Sys.getenv ("cache_dir")
    local_repo <- pkgreport::dl_gh_repo (u)
    local_zip <- paste0 (local_repo, ".zip")
    flist <- unzip (local_zip, exdir = cache_dir)

    # check and modify permissions of configure files
    if (any (list.files (local_repo) == "configure")) {
        cmd <- paste0 ("chmod +755 ", file.path (local_repo, "configure"))
        system (cmd)
    }

    # pkgrep_install_deps is called in get_gp_report
    gp <- pkgreport::get_gp_report (u, local_repo)

    message ("unlinking ", local_repo)
    chk <- unlink (local_repo, recursive = TRUE)

    control <- list (cyclocomp_threshold = 15,
                     covr_threshold = 70,
                     digits = 2)

    res <- pkgreport::process_gp (gp,
                                  control = control)

    res <- paste0 (res, collapse = "\n")

    return (res)
}

# --------------------------------------------------
# -------------------   check   --------------------
# --------------------------------------------------

#* Return srr results from a package URL
#* @param u The URL for a repo
#* @post /check
function (u) {

    cache_dir <- Sys.getenv ("cache_dir")
    local_repo <- pkgreport::dl_gh_repo (u)
    local_zip <- paste0 (local_repo, ".zip")
    flist <- unzip (local_zip, exdir = cache_dir)

    chk <- pkgreport::editor_check (path, u)

    junk <- unlink (local_repo, recursive = TRUE)

    return (chk)
}

# --------------------------------------------------
# --------------------   srr   ---------------------
# --------------------------------------------------

#* Return srr results from a package URL
#* @param u The URL for a repo
#* @post /srr
function (u) {

    cache_dir <- Sys.getenv ("cache_dir")
    local_repo <- pkgreport::dl_gh_repo (u)
    local_zip <- paste0 (local_repo, ".zip")
    flist <- unzip (local_zip, exdir = cache_dir)

    res <- tryCatch (
                srr::srr_stats_pre_submit (local_repo, quiet = TRUE),
                error = function (e) e)
    if (methods::is (res, "error")) {
        res <- paste0 ("- ", res$message)
    }

    res <- paste0 (res, collapse = "\n")

    junk <- unlink (local_repo, recursive = TRUE)

    return (res)
}

# --------------------------------------------------
# ----------------   editorcheck   -----------------
# --------------------------------------------------

#* Return srr results from a package URL
#* @param u The URL for a repo
#* @post /editorcheck
function (u) {

    cache_dir <- Sys.getenv ("cache_dir")
    local_repo <- pkgreport::dl_gh_repo (u)
    local_zip <- paste0 (local_repo, ".zip")
    flist <- unzip (local_zip, exdir = cache_dir)

    srr <- tryCatch (
                srr::srr_stats_pre_submit (local_repo, quiet = TRUE),
                error = function (e) e)
    if (methods::is (srr, "error")) {
        srr <- paste0 ("- ", srr$message)
    }

    srr <- paste0 (srr, collapse = "\n")

    srr_okay <- TRUE

    if (sum (nchar (srr)) > 0L) {

        srr <- strsplit (srr, "\n") [[1]]

        srr <- c ("", "", "### srr", "", "", srr)
        i <- grep ("standards are missing from your code", srr)
        if (length (i) > 0) {

            srr_head <- srr [seq (i)]
            srr <- srr [-seq (i)]
            blank <- which (nchar (srr) == 0)
            srr_tail <- NULL
            if (length (blank) > 1) {
                stds_end <- blank [which (diff (blank) > 1) + 1]
                srr_tail <- srr [seq (stds_end, length (srr))]
                srr <- srr [-seq (stds_end, length (srr))]
            }
            srr <- srr [which (nchar (srr) > 0)]

            srr <- c (srr_head,
                      "",
                      paste0 (srr, collapse = ", "),
                      "",
                      srr_tail)
        }

        srr_okay <- !any (grepl ("can not be submitted", srr)) &
            !any (grepl ("block should only contain", srr))
    }

    gp <- check <- NULL

    if (srr_okay) {

        # check and modify permissions of configure files
        if (any (list.files (local_repo) == "configure")) {
            cmd <- paste0 ("chmod +755 ", file.path (local_repo, "configure"))
            system (cmd)
        }

        # pkgrep_install_deps is called in get_gp_report
        gp <- pkgreport::get_gp_report (u, local_repo)

        control <- list (cyclocomp_threshold = 15,
                         covr_threshold = 70,
                         digits = 2)

        gp <- pkgreport::process_gp (gp, control = control)

        gp <- c ("",
                 "### goodpractice results",
                 "",
                 "",
                 gp,
                 "")

        gp <- paste0 (gp, collapse = "\n")


        check <- pkgreport::editor_check (local_repo, u)

    } else {

        srr <- c (srr,
                  "",
                  "## Editor-in-Chief Instructions:",
                  "",
                  paste0 ("Processing may not proceed until the 'srr' ",
                          "issues identified above have been adressed."))
    }

    message ("unlinking ", local_repo)
    junk <- unlink (local_repo, recursive = TRUE)

    out <- paste0 (c (check, gp, srr), collapse = "\n")

    return (out)
}


# --------------------------------------------------
# --------------   other endpoints   ---------------
# --------------------------------------------------

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
