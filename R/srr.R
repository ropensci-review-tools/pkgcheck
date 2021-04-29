
#' Format the \pkg{srr} reporting section of the general editorial report.
#'
#' @param local_repo Location of local repository to report on
#'
#' @note This uses two of the environmental variables set in \link{serve_api} of
#' `pkgreport_cache' and 'pkgreport_url'.
#' @export
pkgrep_srr_report <- function (local_repo) {

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

    if (sum (nchar (srr)) > 0L) {

        srr_rep <- srr::srr_report (path = local_repo,
                                    view = FALSE)

        categories <- srr_categories_from_report (srr_rep)
        cat_pl <- ifelse (length (categories) == 1,
                          "category",
                          "categories")

        srr <- strsplit (srr, "\n") [[1]]

        srr <- c ("", "", "### srr", "",
                  paste0 ("This package is in the following ",
                          cat_pl, ":"),
                  paste0 ("*", categories, "*"),
                  "", srr)
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
        u <- url_from_desc (local_repo)
        srr_file_from <- attr (srr_rep, "file")
        repo <- utils::tail (strsplit (u, "/") [[1]], 1)
        org <- utils::tail (strsplit (u, "/") [[1]], 2) [1]
        commit <- pkgreport::get_latest_commit (org, repo)
        oid <- substring (commit$oid, 1, 8)
        cache_dir <- Sys.getenv ("cache_dir")
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

        srr_url <- paste0 (Sys.getenv ("pkgreport_url"),
                           "/assets/",
                           basename (srr_file_to))

        srr <- c (srr,
                  "",
                  paste0 ("[Click here to view output of 'srr_report'](",
                          srr_url,
                          "), which can be re-generated locally by ",
                          "running the [`srr_report() function](",
                          "https://ropenscilabs.github.io/srr/",
                          "reference/srr_report.html) from within a ",
                          "local clone of the repository."),
                  "")

        srr_okay <- !any (grepl ("can not be submitted", srr)) &
            !any (grepl ("block should only contain", srr))
    }

    attr (srr, "srr_okay") <- srr_okay

    return (srr)
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
