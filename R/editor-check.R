
#' Body of main 'editorcheck' response
#'
#' @inheritParams pkg_uses_roxygen2
#' @param u URL of package repository
#' @return Markdown-formatted response body from static package checks.
#' @export
editor_check <- function (path, u) {

    uses_roxy <- ifelse (pkgreport::pkg_uses_roxygen2 (path),
                         paste0 ("- ", symbol_tck (),
                                 " Package uses 'roxygen2'"),
                         paste0 ("- ", symbol_crs (),
                                 " Package does not use 'roxygen2'"))

    has_contrib <- unname (pkgreport::pkg_has_contrib_md (path))
    has_lifecycle <- ifelse (has_contrib [2],
                             paste0 ("- ", symbol_tck (),
                                     " Package has a life cycle statement"),
                             paste0 ("- ", symbol_crs (),
                                     " Package does not have a ",
                                     "life cycle statement"))
    has_contrib <- ifelse (has_contrib [1],
                             paste0 ("- ", symbol_tck (),
                                     " Package has a 'contributing.md' file"),
                             paste0 ("- ", symbol_crs (),
                                     " Package does not have a ",
                                     "'contributing.md' file"))

    fn_exs <- pkgreport::all_pkg_fns_have_exs (path)
    fn_exs <- ifelse (all (fn_exs),
                      paste0 ("- ", symbol_tck (),
                              " All functions have examples"),
                      paste0 ("- ", symbol_crs (),
                              " These funtions do not have examples: [",
                              paste0 (names (fn_exs) [which (!fn_exs)]),
                              "]"))

    la <- pkgreport::left_assign (path) # tallies of "<-", "<<-", "="
    la_out <- NULL
    if (la [names (la) == "<<-"] > 0) {
        la_out <- paste0 ("- ", symbol_crs (),
                          " Package uses global assignment operator ('<<-')")
    }
    la <- la [which (names (la) != "<<-")] # ohly "<-", "="
    if (length (which (la == 0)) == 0) {
        la_out <- c (la_out,
                     paste0 ("- ", symbol_crs (),
                             " Package uses inconsistent ",
                             "assignment operators (",
                             la [names (la) == "<-"], " '<-' and ",
                             la [names (la) == "="], " '=')"))
    }

    s <- suppressWarnings (pkgstats::pkgstats (path))

    has_url <- ifelse (!is.na (s$desc$urls),
                       paste0 ("- ", symbol_tck (),
                               " Package 'DESCRIPTION' has a URL field"),
                       paste0 ("- ", symbol_crs (),
                               " Package 'DESCRIPTION' does not ",
                               "have a URL field"))
    has_bugs <- ifelse (!is.na (s$desc$bugs),
                       paste0 ("- ", symbol_tck (),
                               " Package 'DESCRIPTION' has a BugReports field"),
                       paste0 ("- ", symbol_crs (),
                               " Package 'DESCRIPTION' does not ",
                               "have a BugReports field"))
    lic <- s$desc$license
    #pkg_ver <- paste0 (s$desc$package, "_", s$desc$version) # nolint

    # ------------------------------------------------------------
    # ---------------   PKGSTATS + NETWORK GRAPH   ---------------
    # ------------------------------------------------------------

    # stats_checks against all CRAN pkgs
    s_summ <- pkgstats::pkgstats_summary (s)
    stat_chks <- pkgreport::stats_checks (s_summ)
    # ignore large numbers of files:
    stat_chks$noteworthy [grepl ("^files\\_", stat_chks$measure) &
                          stat_chks$percentile > 0.5] <- FALSE
    is_noteworthy <- any (stat_chks$noteworthy)
    stat_chks$percentile <- 100 * stat_chks$percentile
    stat_chks$noteworthy [which (!stat_chks$noteworthy)] <- ""
    stats_rep <- c ("",
                    "### Package Statistics",
                    "",
                    "<details>",
                    "<summary>click to see</summary>",
                    "<p>",
                    "",
                    "---",
                    "",
                    paste0 ("Statistical properties of package structure as ",
                            "distributional percentiles in relation to all ",
                            "current CRAN packages"),
                    "The following terminology is used:",
                    "- `loc` = \"Lines of Code\"",
                    "- `fn` = \"function\"",
                    "- `exp`/`not_exp` = exported / not exported",
                    "",
                    paste0 ("The final measure (`fn_call_network_size`) is ",
                            "the total number of calls between functions (in ",
                            "R), or more abstract relationships between code ",
                            "objects in other languages. Values are flagged ",
                            "as \"noteworthy\" when they lie in the upper or ",
                            "lower 5th percentile."),
                    "",
                    knitr::kable (stat_chks,
                                  row.names = FALSE,
                                  digits = c (NA, 0, 1, NA)),
                    "",
                    "---",
                    "</p></details>"
                    )

    if (is_noteworthy) {

        stats_rep <- c (stats_rep,
                        "",
                        paste0 ("**Note** This package features some ",
                                "noteworthy statistical properties, as ",
                                "detailed in the preceding ",
                                "*Package Statistics* section. ",
                                "Reasons for the features flagged in that ",
                                "section as noteworthy should be clarified ",
                                "prior to progressing."))
    }

    # function call network
    cache_dir <- Sys.getenv ("cache_dir")
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
    visjs_path <- file.path (visjs_dir, visjs_file)
    pkgstats::plot_network (s, vis_save = visjs_path)

    visjs_url <- paste0 (Sys.getenv ("pkgreport_url"), "/assets/", visjs_file)

    network_vis <- c ("",
                      "### Network visualisation",
                      "",
                      paste0 ("[Click here](",
                              visjs_url,
                              ") for interactive network visualisation ",
                              "of calls between objects in package."))

    # ------------------------------------------------------------
    # -----------------   BADGES + OTHER STUFF   -----------------
    # ------------------------------------------------------------

    badges <- pkgreport::ci_badges (u)
    if (is.null (badges)) {

        ci_txt <- paste0 ("- ", symbol_crs (),
                          " Package has no continuous integration checks")
        badges <- NA_character_
    } else {

        ci_txt <- paste0 ("- ", symbol_tck (),
                          " Package has continuous integration checks")
    }

    eic_chks <- c (uses_roxy,
               has_contrib,
               fn_exs,
               la_out,
               has_url,
               has_bugs,
               ci_txt)
    if (any (grepl (symbol_crs (), eic_chks))) {
        eic_chks <- c (eic_chks,
                       "",
                       paste0 ("**Important:** All failing checks above ",
                               "must be addressed prior to proceeding"))
    }

    res <- c (paste0 ("## Checks for [", s$desc$package,
                      " (v", s$desc$version, ")](",
                      u, ")"),
              "",
              paste0 ("git hash: [",
                      oid,
                      "](",
                      u,
                      "/tree/",
                      commit$oid,
                      ")"),
              "",
              eic_chks,
              "",
              paste0 ("Package License: ", lic),
              "",
              stats_rep,
              network_vis,
              "")

    if (!is.null (badges)) {

        if (is.na (badges [1]))
            badges <- "(There do not appear to be any)"

        res <- c (res,
                  "**Continuous Integration Badges**",
                  "",
                  badges,
                  "")

        if (any (grepl ("github", badges))) {

            ci <- pkgreport::ci_results_gh (path)

            res <- c (res,
                      "**GitHub Workflow Results**",
                      "",
                      knitr::kable (ci))
        }
    }

    return (paste0 (res, collapse = "\n"))
}
