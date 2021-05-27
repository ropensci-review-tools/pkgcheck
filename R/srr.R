
#' Format the \pkg{srr} reporting section of the general editorial report.
#'
#' @param path Location of local repository to report on
#'
#' @noRd
pkgchk_srr_report <- function (path) {

    srr <- tryCatch (
                srr::srr_stats_pre_submit (path, quiet = TRUE),
                error = function (e) e)

    if (is.null (srr))
        return (NULL) # Not an srr package

    srr_okay <- FALSE
    if (!methods::is (srr, "error") &
        any (grepl ("^All applicable standards have been documented", srr))) {
        srr_okay <- TRUE
    }

    categories <- stds <- NULL

    # get path to report in cache dir:
    pkg_hash <- current_hash (path)
    static_dir <- file.path (Sys.getenv ("PKGCHECK_CACHE_DIR"),
                             "static")
    f <- paste0 (pkg_hash [1], "_srr", pkg_hash [2])
    srr_report_file <- file.path (static_dir, paste0 (f, ".html"))
    srr_md_file <- file.path (static_dir, paste0 (f, ".md"))

    flist <- list.files (static_dir,
                         full.names = TRUE)
    ptn <- paste0 (.Platform$file.sep, f)
    flist <- grep (ptn, flist, value = TRUE)

    if (srr_md_file %in% flist) {

        srr_rep <- readLines (srr_md_file)

    } else {

        if (length (flist) > 0)
            file.remove (flist)

        srr_rep <- srr::srr_report (path = path,
                                    view = FALSE)
        srr_file_from <- attr (srr_rep, "file")
        if (!file.copy (srr_file_from, srr_report_file))
            warning ("srr html file not copied!")
        # srr_report stored the .md as a .Rmd in tempdir():
        srr_rmd <- tools::file_path_sans_ext (srr_file_from)
        srr_rmd <- paste0 (srr_rmd, ".Rmd")
        if (!file.copy (srr_rmd, srr_md_file))
            warning ("srr md file not copied!")
    }

    categories <- srr_categories_from_report (srr_rep)
    stds <- NULL # missing standards

    i <- grep ("standards are missing from your code", srr)
    if (length (i) > 0) {

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

    list (message = srr,
          categories = categories,
          missing_stds = stds,
          report_file = srr_report_file,
          okay = srr_okay)
}

srr_categories_from_report <- function (s) {

    cats <- regmatches (s, gregexpr ("[A-Z]+[0-9]+\\.[0-9]+([a-z]?)", s))
    cats <- sort (unique (unlist (cats)))
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

    return (categories [match (cats, prefixes)])
}
