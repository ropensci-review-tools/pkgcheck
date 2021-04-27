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

    chk <- pkgreport::editor_check (path)

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

    return (res)
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
