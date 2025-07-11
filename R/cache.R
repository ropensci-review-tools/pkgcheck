#' Set up stdout & stderr cache files for `r_bg` process
#'
#' @param path Path to local repository
#' @return Vector of two strings holding respective local paths to `stdout` and
#' `stderr` files for `r_bg` process controlling the main \link{pkgcheck}
#' function when executed in background mode.
#'
#' @note These files are needed for the \pkg{callr} `r_bg` process which
#' controls the main \link{pkgcheck}. The `stdout` and `stderr` pipes from the
#' process are stored in the cache directory so they can be inspected via their
#' own distinct endpoint calls.
#' @family extra
#' @export
#' @examples
#' \dontrun{
#' logfiles <- logfiles_namnes ("/path/to/my/package")
#' print (logfiles)
#' }
logfile_names <- function (path) {

    temp_dir <- fs::path (Sys.getenv ("PKGCHECK_CACHE_DIR"), "templogs")
    if (!dir.exists (temp_dir)) {
        dir.create (temp_dir, recursive = TRUE)
    }

    pkg_hash <- current_hash (path)
    pkg_hash_fmt <- paste0 (pkg_hash, collapse = "_")

    sout <- fs::path (temp_dir, paste0 (pkg_hash_fmt, "_stdout"))
    serr <- fs::path (temp_dir, paste0 (pkg_hash_fmt, "_stderr"))

    otherlogs <- list.files (
        temp_dir,
        pattern = pkg_hash [1],
        full.names = TRUE
    )
    otherlogs <- otherlogs [which (!grepl (pkg_hash [2], otherlogs))]
    if (length (otherlogs) > 0) {
        file.remove (otherlogs)
    }

    return (list (stdout = sout, stderr = serr))
}

#' Get hash of last git commit
#'
#' @param path Path to local source directory
#' @return Vector of 2 values: (package name, hash)
#' @noRd
current_hash <- function (path) {

    if (!fs::file_exists (fs::path (path, "DESCRIPTION"))) {
        stop ("path [", path, "] does not appear to be an R package")
    }

    desc <- data.frame (read.dcf (file.path (path, "DESCRIPTION")),
        stringsAsFactors = FALSE
    )
    pkg <- desc$Package

    if (repo_is_git (path)) {
        g <- gert::git_info (path)
        hash <- substring (g$commit, 1, 8)
    } else { # not a git repo, so use mtime as hash

        flist <- list.files (path, recursive = TRUE, full.names = TRUE)
        mt <- file.info (flist)$mtime
        if (any (!is.na (mt))) {
            mt <- max (mt, na.rm = TRUE)
        } else {
            mt <- ""
        }
        hash <- gsub ("\\s+", "-", paste0 (mt))
    }

    c (pkg, hash)
}

cache_pkgcheck_component <- function (path,
                                      use_cache,
                                      renv_activated,
                                      gp_full = TRUE,
                                      what = "goodpractice") {

    what <- match.arg (what, c ("goodpractice", "pkgstats"))

    dir_name <- ifelse (
        what == "goodpractice",
        "gp_reports",
        "pkgstats_results"
    )
    this_fn <- ifelse (
        what == "goodpractice",
        goodpractice::goodpractice,
        pkgstats::pkgstats
    )

    args <- list (path = path)
    if (what == "goodpractice" && !gp_full) {
        index <- grep ("^(covr|cyclocomp|rcmdcheck)", goodpractice::all_checks ())
        chks <- goodpractice::all_checks () [-index]
        args <- c (args, list (checks = chks))
    }


    pkg_hash <- current_hash (path)
    fname <- paste0 (pkg_hash [1], "_", pkg_hash [2])
    cache_dir <- file.path (
        Sys.getenv ("PKGCHECK_CACHE_DIR"),
        dir_name
    )
    if (!dir.exists (cache_dir)) {
        dir.create (cache_dir, recursive = TRUE)
    }

    cache_file <- fs::path (cache_dir, fname)

    # rm old components:
    flist <- fs::dir_ls (cache_dir, regexp = paste0 (pkg_hash [1], "\\_"))
    flist <- flist [which (!basename (flist) == fname)]
    if (length (flist) > 0) {
        chk <- fs::file_delete (flist)
    }

    if (fs::file_exists (cache_file) & use_cache) {

        out <- readRDS (cache_file)

    } else {

        # this envvar is for goodpractice, but no harm setting for other
        # components too
        Sys.setenv ("_R_CHECK_FORCE_SUGGESTS_" = FALSE)
        if (renv_activated) {
            renv_deactivate (path) # in R/info-renv.R
            message (
                "'renv' has been de-activated; to reactivate, run ",
                "`renv::activate()` in your project directory after ",
                "`pkgcheck` has finished"
            )
        }

        out <- suppressWarnings (do.call (this_fn, args))

        Sys.unsetenv ("_R_CHECK_FORCE_SUGGESTS_")

        # writing to cache_dir fails on some GHA windows machines.
        if (fs::dir_exists (cache_dir)) {
            chk <- tryCatch (
                saveRDS (out, cache_file),
                error = function (e) NULL
            )
        }
    }

    return (out)
}
