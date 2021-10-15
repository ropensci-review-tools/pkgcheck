
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
logfile_names <- function (path) {

    temp_dir <- file.path (Sys.getenv ("PKGCHECK_CACHE_DIR"), "templogs")
    if (!dir.exists (temp_dir)) {
        dir.create (temp_dir, recursive = TRUE)
    }

    pkg_hash <- current_hash (path)
    pkg_hash_fmt <- paste0 (pkg_hash, collapse = "_")

    sout <- file.path (temp_dir, paste0 (pkg_hash_fmt, "_stdout"))
    serr <- file.path (temp_dir, paste0 (pkg_hash_fmt, "_stderr"))

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

current_hash <- function (path) {

    if (!file.exists (file.path (path, "DESCRIPTION"))) {
        stop ("path [", path, "] does not appear to be an R package")
    }

    desc <- data.frame (read.dcf (file.path (path, "DESCRIPTION")))
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
