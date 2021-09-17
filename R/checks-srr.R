
#' Format the \pkg{srr} reporting section of the general editorial report.
#'
#' This is a complex multi-stage check which returns a list of sub-checks,
#' including:
#'
#' \enumerate{
#'
#' \item message: The main message from srr_stats_pre_submit confirming whether
#'     all standards have been documented or not
#' \item categories: List of statistical categories addressed in the software
#' \item missing_stds: List of any missing standards
#' \item report_file: Link to the HTML version of the `srr` report file
#' \item okay: Logical flag indicating whether or not all `srr` requirements
#'     have been met.
#'
#' }
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

#' Extract the statistical categories from the `srr` report
#' @param s Result of main `srr:srr_report()` function
#' @noRd
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

#' @return tick or cross (for 'srr' packages only)
#' @noRd
summarise_srr_checks <- function (checks) {

    res <- NULL

    if (!is.null (checks$srr)) {

        m <- checks$srr$message
        i <- which (nchar (m) > 0 & grepl ("[^\\s]*", m))

        res <- paste0 ("- ",
                       ifelse (checks$srr$okay,
                               symbol_tck (),
                               symbol_crs ()),
                       " ",
                       m [i [1]],
                       ".")
        res <- gsub (paste0 ("Package can not be submitted because ",
                             "the following standards are missing"),
                     "Statistical standards are missing",
                     res)
        res <- gsub (":$", ".", res)
    }

    return (res)
}

print_srr <- function (x) {

    cli::cli_h2 ("rOpenSci Statistical Standards")
    ncats <- length (x$srr$categories) # nolint
    cli::cli_alert_info ("The package is in the following {ncats} categor{?y/ies}:") # nolint
    cli::cli_li (x$srr$categories)
    cli::cli_text ("")
    cli::cli_alert_info ("Compliance with rOpenSci statistical standards:")

    if (x$srr$okay) {
        cli::cli_alert_success (x$srr$message)
    } else {
        cli::cli_alert_danger (x$srr$message [1])
        if (length (x$srr$message) > 1) {
            m <- x$srr$message [-1]
            if (grepl ("missing from your code", m [1])) {
                cli::cli_text (m [1])
                cli::cli_text ("")
                m <- paste0 (m [which (m != "")] [-1], collapse = ", ")
                cli::cli_text (paste0 (m, "."))
            }
        }
        return ()
    }

    if (!is.null (x$srr$missing_stds)) {
        cli::cli_alert_warning ("The following standards are missing:")
        cli::cli_li (x$srr$missing_stds)
    }

    cli::cli_alert_info ("'srr' report is at [{x$srr$report_file}].")
    message ("")
}

#' Format `srr` checks in markdown
#' @param checks Result of main \link{pkgcheck} function
#' @noRd
srr_checks_to_md <- function (checks) {

    if (is.null (checks$srr))
        return (NULL)

    cat_plural <- ifelse (length (checks$srr$categories == 1),
                          "category",
                          "categories")
    srr_msg <- ifelse (checks$srr$okay,
                       paste0 (symbol_tck (), " ", checks$srr$message),
                       paste0 (symbol_crs (), " ", checks$srr$message))

    c (paste0 ("## 1. rOpenSci Statistical Standards ",
               "([`srr` package]",
               "(https://github.com/ropensci-review-tools/srr))"),
       "",
       paste0 ("This package is in the following ", cat_plural, ":"),
       "",
       paste0 ("- *", checks$srr$categories, "*"),
       "",
       srr_msg,
       "",
       paste0 ("Click [here to see the report of author-reported ",
               "standards compliance of the package with links to ",
               "associated lines of code](",
               checks$srr$report_file,
               "), which can be re-generated locally by running the ",
               "[`srr_report()` function]",
               "(https://docs.ropensci.org/srr/reference/srr_report.html) ",
               "from within a local clone of the repository."),
       "",
       "---",
       "")

}
