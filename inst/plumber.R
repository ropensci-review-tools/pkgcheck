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

    tck <- ":heavy_check_mark:"
    crs <- ":heavy_multiplication_x:"

    uses_roxy <- ifelse (pkgreport::pkg_uses_roxygen2 (local_repo),
                         paste0 ("- ", tck, " Package uses 'roxygen2'"),
                         paste0 ("- ", crs, " Package does not use 'roxygen2'"))

    has_contrib <- unname (pkgreport::pkg_has_contrib_md (local_repo))
    has_lifecycle <- ifelse (has_contrib [2],
                             paste0 ("- ", tck,
                                     " Package has a life cycle statement"),
                             paste0 ("- ", crs,
                                     " Package does not have a ",
                                     "life cycle statement"))
    has_contrib <- ifelse (has_contrib [1],
                             paste0 ("- ", tck,
                                     " Package has a 'contributing.md' file"),
                             paste0 ("- ", crs,
                                     " Package does not have a ",
                                     "'contributing.md' file"))

    fn_exs <- pkgreport::all_pkg_fns_have_exs (local_repo)
    fn_exs <- ifelse (all (fn_exs),
                      paste0 ("- ", tck,
                              " All functions have examples"),
                      paste0 ("- ", crs,
                              " These funtions do not have examples: [",
                              paste0 (names (fn_exs) [which (!fn_exs)]),
                              "]"))

    la <- pkgreport::left_assign (local_repo) # tallies of "<-", "<<-", "="
    la_out <- NULL
    if (la [names (la) == "<<-"] > 0) {
        la_out <- paste0 ("- ", crs,
                          " Package uses global assignment operator ('<<-')")
    }
    la <- la [which (names (la) != "<<-")] # ohly "<-", "="
    if (length (which (la == 0)) == 0) {
        la_out <- c (la_out,
                     paste0 ("- ", crs,
                             " Package uses inconsistent ",
                             "assignment operators (",
                             la [names (la) == "<-"], " '<-' and ",
                             la [names (la) == "="], " '=')"))
    }

    s <- suppressWarnings (pkgstats::pkgstats (local_repo))

    has_url <- ifelse (!is.na (s$desc$urls),
                       paste0 ("- ", tck,
                               " Package 'DESCRIPTION' has a URL field"),
                       paste0 ("- ", crs,
                               " Package 'DESCRIPTION' does not ",
                               "have a URL field"))
    has_bugs <- ifelse (!is.na (s$desc$bugs),
                       paste0 ("- ", tck,
                               " Package 'DESCRIPTION' has a BugReports field"),
                       paste0 ("- ", crs,
                               " Package 'DESCRIPTION' does not ",
                               "have a BugReports field"))
    lic <- s$desc$license
    pkg_ver <- paste0 (s$desc$package, "_", s$desc$version)

    # stats_checks against all CRAN pkgs
    s_summ <- pkgstats::pkgstats_summary (s)
    stat_chks <- pkgreport::stats_checks (s_summ)
    stats_rep <- NULL
    if (any (grepl ("^loc_", rownames (stat_chks)))) {

        # loc in directories:
        index <- grep ("^loc\\_(r|s|v|i|t)",
                       rownames (stat_chks),
                       ignore.case = TRUE)
        loc_chks <- rownames (stat_chks) [index]

        for (i in loc_chks) {
            stats_rep <- c (stats_rep,
                            paste0 ("- Package has very few ",
                                    "lines of code in the ",
                                    gsub ("^loc\\_", "", i),
                                    " directory"))

        }

        # loc per function:
        index <- grep ("loc\\_per\\_", rownames (stat_chks),
                       ignore.case = TRUE)
        loc_chks <- gsub ("\\_mn$|\\_md$", "", rownames (stat_chks) [index])
        loc_chks <- loc_chks [which (!grepl ("\\_not\\_exp$", loc_chks))]
        for (i in loc_chks) {
            stats_rep <- c (stats_rep,
                            paste0 ("- ", crs,
                                    " Package has very few lines of ",
                                    "code per function in the ",
                                    gsub ("^loc\\_per\\_fn\\_", "", i),
                                    " directory"))
        }
    }

    badges <- pkgreport::ci_badges (u)
    if (is.null (badges)) {

        ci_txt <- paste0 ("- ", crs,
                          " Package has no continuous integration checks")
        badges <- NA_character_
    } else {

        ci_txt <- paste0 ("- ", tck,
                          " Package has continuous integration checks")
    }

    eic_chks <- c (uses_roxy,
               has_contrib,
               fn_exs,
               la_out,
               has_url,
               has_bugs,
               ci_txt)
    if (any (grepl (crs, eic_chks))) {
        eic_chks <- c (eic_chks,
                       "",
                       paste0 ("**Important:** All failing checks above ",
                               "must be addressed prior to proceeding"))
    }

    res <- c (paste0 ("## Checks for [", s$desc$package,
                      " (v", s$desc$version, ")](",
                      u, ")"),
              "",
              eic_chks,
              "",
              paste0 ("Package License: ", lic),
              "")

    if (!is.null (stats_rep)) {

        res <- c (res,
                  "### Statistically Noteworthy Features",
                  "",
                  paste0 ("Statistical analyses of package structure ",
                          "reveal the following noteworthy features ",
                          "in comparison to the statistical properties ",
                          "of CRAN packages:"),
                  "",
                  stats_rep,
                  "")
    }

    if (!is.null (badges)) {

        if (is.na (badges [1]))
            badges <- "(There do not appear to be any)"

        res <- c (res,
                  "**Continuous Integration Badges**",
                  "",
                  badges,
                  "")

        if (any (grepl ("github", badges))) {

            ci <- pkgreport::ci_results_gh (local_repo)

            res <- c (res,
                      "**GitHub Workflow Results**",
                      "",
                      knitr::kable (ci))
        }
    }

    return (paste0 (res, collapse = "\n"))
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

    res <- srr::srr_stats_pre_submit (local_repo, quiet = TRUE)

    if (any (grepl ("can not be submitted", res)))
        res <- c (res,
                  "",
                  paste0 ("**Important:** The preceding 'srr' issues ",
                          "must be addressed prior to proceeding"))

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
