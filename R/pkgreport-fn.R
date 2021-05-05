
#' Generate report on package compliance with rOpenSci Statistical Software
#' requirements
#'
#' @param path Path to local repository
#' @return A `pkgreport` object detailing all package assessments automatically
#' applied to packages submitted for peer review.
#' @export
pkgreport <- function (path) {

    u <- url_from_desc (path)

    s <- suppressWarnings (pkgstats::pkgstats (path))

    out <- list ()
    out$package <- s$desc$package
    out$version <- s$desc$version
    out$license <- s$desc$license

    repo <- utils::tail (strsplit (u, "/") [[1]], 1)
    org <- utils::tail (strsplit (u, "/") [[1]], 2) [1]
    commit <- get_latest_commit (org, repo)

    gitlog <- gert::git_log (repo = path, max = 1e6)
    # use email addresses to identify unique authors
    auts <- gsub ("^.*<|>$", "", unique (gitlog$author))

    out$git <- list (HEAD = commit$oid,
                     num_commits = nrow (gitlog),
                     since = min (gitlog$time),
                     num_authors = length (unique (auts)))

    out$srr <- pkgrep_srr_report (path)

    out$file_list <- list ()
    out$file_list$uses_roxy <- pkg_uses_roxygen2 (path)
    has_contrib <- unname (pkg_has_contrib_md (path))
    out$file_list$has_lifecycle <- has_contrib [2]
    out$file_list$has_contrib <- has_contrib [1]

    out$fns_have_exs <- all_pkg_fns_have_exs (path)

    la <- left_assign (path) # tallies of "<-", "<<-", "="
    out$left_assigns <- list (global = la [["<<-"]] > 0)
    la <- la [names (la) != "<<-"]
    out$left_assigns$usage <- la

    out$file_list$has_url <- !is.na (s$desc$urls)
    out$file_list$has_bugs <- !is.na (s$desc$bugs)

    out$pkgstats <- pkgstats_checks (s)

    # ------------------------------------------------------------
    # -----------------   FUNCTION CALL NETWORK   ----------------
    # ------------------------------------------------------------

    cache_dir <- Sys.getenv ("cache_dir")
    visjs_dir <- file.path (cache_dir, "static") # in api.R
    visjs_file <- paste0 (repo,
                          "_pkgstats",
                          substring (commit$oid, 1, 8),
                          ".html")

    # clean up any older ones
    flist <- list.files (visjs_dir,
                         pattern = paste0 (repo, "_pkgstats"),
                         full.names = TRUE)
    unlink (flist, recursive = TRUE)
    visjs_path <- file.path (visjs_dir, visjs_file)
    pkgstats::plot_network (s, vis_save = visjs_path)
    out$network_file <- visjs_path


    # ------------------------------------------------------------
    # -----------------   BADGES + OTHER STUFF   -----------------
    # ------------------------------------------------------------

    out$badges <- ci_badges (u)
    if (!is.null (out$badges)) {
        if (any (grepl ("github", out$badges))) {
            out$github_workflows <- ci_results_gh (path)
        }
    }

    out$gp <- get_gp_report (path)

    return (out)
}

#' Format \pkg{pkgstats} data
#' @param s Output of \pkg{pkgstats} call.
#' @return Report as formatted string
#' @export
pkgstats_checks <- function (s) {

    s_summ <- pkgstats::pkgstats_summary (s)
    stat_chks <- stats_checks (s_summ)
    # ignore large numbers of files:
    stat_chks$noteworthy [grepl ("^files\\_", stat_chks$measure) &
                          stat_chks$percentile > 0.5] <- FALSE
    is_noteworthy <- any (stat_chks$noteworthy)
    stat_chks$percentile <- 100 * stat_chks$percentile
    stat_chks$noteworthy [which (!stat_chks$noteworthy)] <- ""

    return (stat_chks)
}
