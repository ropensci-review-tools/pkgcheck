
#' Format the \pkg{srr} reporting section of the general editorial report.
#'
#' @param path Location of local repository to report on
#'
#' @noRd
pkgchk_srr_report <- function (path) {

    srr <- tryCatch (
                srr::srr_stats_pre_submit (path, quiet = TRUE),
                error = function (e) e)

    srr_okay <- FALSE
    if (!methods::is (srr, "error") &
        any (grepl ("^All applicable standards have been documented", srr))) {
        srr_okay <- TRUE
    }


    categories <- stds <- NULL

    if (srr_okay) {

        # get path to report in cache dir:
        u <- url_from_desc (path)
        repo <- utils::tail (strsplit (u, "/") [[1]], 1)
        org <- utils::tail (strsplit (u, "/") [[1]], 2) [1]
        commit <- get_latest_commit (org, repo)
        oid <- substring (commit$oid, 1, 8)
        static_dir <- file.path (Sys.getenv ("pkgcheck_cache_dir"),
                                 "static")
        srr_report_file <- file.path (static_dir,
                                      paste0 (repo, "_srr", oid, ".html"))

        flist <- list.files (static_dir,
                             pattern = paste0 (repo, "\\_srr"),
                             full.names = TRUE)
        if (!srr_report_file %in% flist) {

            if (length (flist) > 0)
                file.remove (flist)

            srr_rep <- srr::srr_report (path = path,
                                        view = FALSE)
            srr_file_from <- attr (srr_rep, "file")
            if (!file.copy (srr_file_from, srr_report_file))
                warning ("file not copied!")
        }

        categories <- srr_categories_from_report (srr_rep)
        stds <- NULL # missing standards

        i <- grep ("standards are missing from your code", srr)
        if (length (i) > 0) {

            #srr_head <- srr [seq (i)]
            stds <- srr [-seq (i)]
            blank <- which (nchar (stds) == 0)
            #srr_tail <- NULL
            if (length (blank) > 1) {
                stds_end <- blank [which (diff (blank) > 1) + 1]
                #srr_tail <- stds [seq (stds_end, length (stds))]
                stds <- stds [-seq (stds_end, length (stds))]
            }
            stds <- paste0 (stds [which (nchar (stds) > 0)],
                            collapse = ", ")
        }

        srr_okay <- length (i) == 0
    }

    list (message = srr,
          categories = categories,
          missing_stds = stds,
          report_file = srr_report_file,
          okay = srr_okay)
}

url_from_desc <- function (path) {

    desc <- file.path (path, "DESCRIPTION")
    if (!file.exists (desc))
        return (NULL)

    d <- data.frame (read.dcf (desc))
    if (!"URL" %in% names (d))
        return (NULL)

    u <- strsplit (d$URL, "\\s+") [[1]]
    u <- grep ("^https", u, value = TRUE)
    if (length (u) > 1)
        u <- grep ("git", u, value = TRUE)
    if (length (u) > 1)
        u <- u [which (!grepl ("\\.io", u))]

    return (u [1])
}

srr_categories_from_report <- function (s) {

    cats <- regmatches (s, gregexpr ("\\*\\*.*\\*\\*", s))
    cats <- unlist (lapply (cats, function (i)
                            if (length (i) > 0)
                                strsplit (i, ", ") [[1]]))
    cats <- gsub ("\\*\\*", "", cats)
    cats <- unique (gsub ("[0-9].*$", "", cats))
    cats <- cats [which (!cats == "G")]

    prefixes <- c ("BS", "EA", "ML", "RE", "SP", "TS", "UL")
    categories <- c ("Bayesian and Monte Carlo",
                     "Exploratory Data Analysis",
                     "Machine Learning",
                     "Regression and Supervised Learning",
                     "Spatial",
                     "Time Series",
         "Dimensionality Reduction, Clustering and Unsupervised Learning")
    cats <- cats [which (cats %in% prefixes)]
    # TODO: error handling on that one

    return (categories [match (cats, prefixes)])
}
