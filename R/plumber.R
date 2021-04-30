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

#* Run full range of editor checks
#* @param u The URL for a repo
#* @post /editorcheck
function (u) {

    cache_dir <- Sys.getenv ("cache_dir")
    local_repo <- pkgreport::dl_gh_repo (u)
    local_zip <- paste0 (local_repo, ".zip")
    flist <- unzip (local_zip, exdir = cache_dir)

    srr <- pkgreport::pkgrep_srr_report (local_repo)
    srr_okay <- attr (srr, "srr_okay")

    gp <- check <- NULL
    is_noteworthy <- FALSE
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
        is_noteworthy <- attr (check, "is_noteworthy")

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

    noteworthy <- ifelse (is_noteworthy,
                          c (paste0 ("This package has some noteworthy properties, ",
                                     "see 'Package Statistics' details below"),
                             ""),
                          "")
    i <- grep ("\\#\\#\\# Package Statistics", chk_gp) - 1
    chk_gp <- c (chk_gp [seq (i)],
                 "",
                 "### Details",
                 "",
                 noteworthy,
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
# ---------------   goodtoreview   -----------------
# --------------------------------------------------

#* Check whether a package is good to go out to review (intended to be called by
#8 handling editors).
#* @param u The URL for a repo
#* @post /goodtoreview
function (u) {

    repo <- tail (strsplit (u, "/") [[1]], 1)
    org <- tail (strsplit (u, "/") [[1]], 2) [1]

    cache_dir <- Sys.getenv ("cache_dir")
    updated <- pkgreport::check_cache (org, repo, cache_dir)

    oids <- readRDS (file.path (cache_dir, "commit_oids.Rds"))
    oid <- oids$oid [oids$orgrepo == paste0 (org, "/", repo)]
    oid_short <- substring (oid, 1, 8)

    local_repo <- pkgreport::dl_gh_repo (u)
    local_zip <- paste0 (local_repo, ".zip")
    flist <- unzip (local_zip, exdir = cache_dir)

    s <- suppressWarnings (pkgstats::pkgstats (local_repo))
    check <- pkgreport::pkgstats_checks (s)
    is_noteworthy <- attr (check, "is_noteworthy")
    check <- check [which (!grepl ("^<|^---", check))]

    out <- note <- NULL

    if (is_noteworthy) {

        where <- ifelse (updated,
                         "immediately above. ",
                         paste0 ("in the 'details' section at the ",
                                 "top of this issue. "))
        note <- paste0 ("**Note** This package features some ",
                        "noteworthy statistical properties, as ",
                        "detailed ",
                        where,
                        "Reasons for the features flagged in that ",
                        "section as noteworthy should be clarified ",
                        "by a handling editor prior to progressing.")

        if (updated) {

            i <- grep ("\\#\\#\\# Package Statistics", check) + 1
            out <- c (check [seq (i)],
                      "",
                      paste0 ("Since the previous statistics were generated, ",
                              "the repository has been updated to:"),
                      paste0 ("git hash: [",
                              oid_short,
                              "](https://github.com/",
                              org,
                              "/",
                              repo,
                              "/tree/",
                              oid,
                              ")"),
                      "",
                      check [-seq (i)],
                      "")
        }
    }

    out <- c (out,
              note,
              "",
              "## Handling Editor Instructions:",
              "")

    if (is_noteworthy) {

        out <- c (out,
                  paste0 ("Please address the above noteworthy statistical ",
                          "properties prior to assigning reviewers."))
    } else {

        out <- c (out,
                  paste0 ("This package is a great shape! Please proceed ",
                          "to finding reviewers."))
    }


    message ("unlinking ", local_repo)
    junk <- unlink (local_repo, recursive = TRUE)


    return (paste0 (out, collapse = "\n"))
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
