#' Get 'goodpractice' report on local source package
#'
#' This uses a caching system to only generate new report if repository has been
#' updated, otherwise it returns locally cached version.
#'
#' @param u URL
#' @param local_repo Path to local source of repository
#' @return A \pkg{goodpractice} report
#' @export
get_gp_report <- function (u, local_repo) {

    repo <- tail (strsplit (u, "/") [[1]], 1)
    org <- tail (strsplit (u, "/") [[1]], 2) [1]

    cache_dir <- Sys.getenv ("cache_dir")

    # check whether gp is cached:
    cmt <- pkgreport::get_latest_commit (org = org, repo = repo)
    fname <- paste0 (repo, "_", substring (cmt$oid, 1, 8))
    gp_cache_dir <- file.path (cache_dir, "gp_reports")
    gp_cache_file <- file.path (gp_cache_dir, fname)

    if (file.exists (gp_cache_file)) {

        gp <- readRDS (gp_cache_file)

    } else {

        if (!file.exists (gp_cache_dir))
            dir.create (gp_cache_dir, recursive = TRUE)

        withr::with_temp_libpaths ({
            remotes::install_local (local_repo,
                                    upgrade = "never",
                                    dependencies = TRUE)
            gp <- goodpractice::goodpractice (local_repo)
        })

        saveRDS (gp, gp_cache_file)
    }

    return (gp)
}

#' process 'goodpractice' report
#'
#' Extract various components from 'goodpractice' report and convert into format
#' to be returned by endpoint
#' @param gp A 'goodpractice' object as returned by \pkg{goodpractice}
#' @return Markdown-formatted report
#' @export
process_gp <- function (gp) {

    gp <- extract_gp_components (gp)

    return (gp)
}


extract_gp_components <- function (gp) {

    # -------------- covr:
    covr <- capture.output (print (gp$covr),
                            type = "message")

    # -------------- cyclocomp:
    cyc_threshold <- 5 # report all fns >= this value
    cyc <- gp$cyclocomp [which (gp$cyclocomp$cyclocomp > cyc_threshold), ] # data.frame

    # -------------- lintr:
    lint_file <- vapply (gp$lintr, function (i) i$filename, character (1))
    lint_line <- vapply (gp$lintr, function (i) i$line_number, integer (1))
    lint_type <- vapply (gp$lintr, function (i) i$type, character (1))
    lint_message <- vapply (gp$lintr, function (i) i$message, character (1))
    lints <- data.frame (file = lint_file,
                         line = lint_line,
                         type = lint_type,
                         message = lint_message,
                         stringsAsFactors = FALSE)
    # lintr reports library calls in tests dir, which are okay
    index <- which (grepl ("^tests", lints$file) &
                    grepl ("^Avoid library", lints$message))
    lints <- lints [-index, ]
    if (nrow (lints) == 0)
        lints <- list ()

    # -------------- rcmdcheck:
    r <- gp$rcmdcheck
    rcmd <- list ()
    if (length (r$errors) > 0)
        rcmd$error <- r$error
    if (length (r$warnings) > 0)
        rcmd$warnings <- r$warnings
    if (length (r$notes) > 0)
        rcmd$notes <- r$notes
    if (length (r$test_fail) > 0)
        rcmd$test_fail <- r$test_fail

    # -------------- any other components which fail:
    checks <- vapply (gp$checks, function (i) {
                         ret <- TRUE
                         if (is.logical (i) & length (i) == 1)
                             ret <- i
                         return (ret)   },
                         logical (1))
    fails <- names (checks [which (!checks)])

    # return result
    res <- list (rcmd = rcmd,
                 covr = covr,
                 cyclocomp = cyc,
                 lint = lints,
                 check_fails = fails)
    res <- res [which (vapply (res, length, integer (1)) > 0)]
}
