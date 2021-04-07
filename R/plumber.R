# plumber.R

#* Return report from package URL
#* @param u The URL for a repo
#* @post /report
function (u) {

    cache_dir <- Sys.getenv ("cache_dir")
    local_repo <- pkgreport::dl_gh_repo (u)
    local_zip <- paste0 (local_repo, ".zip")
    flist <- unzip (local_zip, exdir = cache_dir)

    visjs_dir <- file.path (cache_dir, "static") # in api.R
    repo <- tail (strsplit (u, "/") [[1]], 1)
    org <- tail (strsplit (u, "/") [[1]], 2) [1]
    commit <- pkgreport::get_latest_commit (org, repo)
    oid <- substring (commit$oid, 1, 8)
    visjs_file <- paste0 (repo, "_", oid, ".html")

    # pg_graph directly calls pkgapi::map_package which requires all system deps
    pkgreport::pkgrep_install_deps (os = Sys.getenv ("os"),
                                    os_release = Sys.getenv ("os_release"),
                                    path = local_repo)

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

#* Return goodpractice results from a package URL
#* @param u The URL for a repo
#* @post /gp
function (u) {

    cache_dir <- Sys.getenv ("cache_dir")
    local_repo <- pkgreport::dl_gh_repo (u)
    local_zip <- paste0 (local_repo, ".zip")
    flist <- unzip (local_zip, exdir = cache_dir)

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
