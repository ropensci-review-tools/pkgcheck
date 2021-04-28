# plumber.R

# --------------------------------------------------
# ------------------   network   ------------------
# --------------------------------------------------

#* Return report from package URL
#* @param u The URL for a repo
#* @post /network
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
    visjs_file <- paste0 (repo, "_pkgstats", oid, ".html")

    # clean up any older ones
    flist <- list.files (visjs_dir,
                         pattern = paste0 (repo, "_pkgstats"),
                         full.names = TRUE)
    unlink (flist, recursive = TRUE)

    s <- pkgstats::pkgstats (local_repo)
    visjs_path <- file.path (visjs_dir, visjs_file)
    pkgstats::plot_network (s, vis_save = visjs_path)

    visjs_url <- paste0 (Sys.getenv ("pkgreport_url"), "/assets/", visjs_file)

    out <- c ("### Network visualisation",
              "",
              paste0 ("[Click here](",
                      visjs_url,
                      ") for interactive network visualisation ",
                      "of calls between objects in package."))

    return (paste0 (out, collapse = "\n"))
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

    chk <- pkgreport::editor_check (local_repo, u)

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

        srr <- paste0 ("- ",
                       pkgreport::symbol_crs (),
                       " ",
                       srr$message)
    } else if (any (grepl ("ready to submit", srr))) {

        srr <- paste0 ("- ",
                       pkgreport::symbol_tck (),
                       " ",
                       srr)
    } else if (!is.null (srr)) {

        srr <- paste0 ("- ",
                       pkgreport::symbol_crs (),
                       " ",
                       srr)
    }

    srr <- paste0 (srr, collapse = "\n")

    srr_okay <- TRUE
    srr_rep <- NULL

    if (sum (nchar (srr)) > 0L) {

        srr_rep <- srr::srr_report (path = local_repo,
                                    view = FALSE)

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

        # cp report file to static dir:
        srr_file_from <- attr (srr_rep, "file")
        repo <- tail (strsplit (u, "/") [[1]], 1)
        org <- tail (strsplit (u, "/") [[1]], 2) [1]
        commit <- pkgreport::get_latest_commit (org, repo)
        oid <- substring (commit$oid, 1, 8)
        static_dir <- file.path (normalizePath (cache_dir),
                                 "static")
        srr_file_to <- file.path (static_dir,
                                  paste0 (repo, "_srr", oid, ".html"))
        # rm old files
        flist <- list.files (static_dir,
                             pattern = paste0 (repo, "\\_srr"),
                             full.names = TRUE)
        if (length (flist) > 0)
            file.remove (flist)

        if (!file.copy (srr_file_from, srr_file_to))
            warning ("file not copied!")
        else
            warning ("file successfully copied to [", srr_file_to, "]")

        srr_url <- paste0 (Sys.getenv ("pkgreport_url"),
                           "/assets/",
                           basename (srr_file_to))

        srr <- c (srr,
                  "",
                  paste0 ("[Click here to view output of 'srr_report'](",
                          srr_url,
                          ")"),
                  "")

        srr_okay <- !any (grepl ("can not be submitted", srr)) &
            !any (grepl ("block should only contain", srr))
    }

    gp <- check <- NULL
    eic_instr <- c ("", "## Editor-in-Chief Instructions:", "")

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

        if (any (grepl (pkgreport::symbol_crs (), check))) {

            eic_instr <- c (eic_instr,
                            paste0 ("Processing may not proceed until the ",
                                    "items marked with ",
                                    pkgreport::symbol_crs (),
                                    " have been resolved."))
        } else {

            eic_instr <- c (eic_instr,
                            paste0 ("This package is in top shape and may ",
                                    "be passed on to a handling editor"))
        }

    } else {

        eic_instr <- c (eic_instr,
                        paste0 ("Processing may not proceed until the 'srr' ",
                                "issues identified above have been adressed."))
    }

    message ("unlinking ", local_repo)
    junk <- unlink (local_repo, recursive = TRUE)

    # collapse core of check and gp into a details section:
    chk_gp <- c (strsplit (check, "\n") [[1]],
                 strsplit (gp, "\n") [[1]])
    i <- grep ("\\#\\#\\# Package Statistics", chk_gp) - 1
    chk_gp <- c (chk_gp [seq (i)],
                 "",
                 "### Details",
                 "",
                 "<details>",
                 "<summary>Click to see</summary>",
                 "<p>",
                 "",
                 chk_gp [-seq (i)],
                 "",
                 "</p></details>",
                 "")


    out <- paste0 (c (chk_gp, srr, eic_instr), collapse = "\n")


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
