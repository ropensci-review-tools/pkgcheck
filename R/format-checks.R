
#' Convert checks to markdown-formatted report
#'
#' @param checks Result of main \link{pkgcheck} function
#' @param render If `TRUE`, render output as `html` document and open in
#' browser.
#' @return Markdown-formatted version of check report
#' @family extra
#' @export
checks_to_markdown <- function (checks, render = FALSE) {

    md_chks <- collate_checks (checks)

    md_out <- c (paste0 ("## Checks for [", checks$package,
                         " (v", checks$version, ")](",
                         checks$url, ")"),
                 "",
                 paste0 ("git hash: [",
                         substring (checks$git$HEAD, 1, 8),
                         "](",
                         checks$url,
                         "/tree/",
                         checks$git$HEAD,
                         ")"),
                 "",
                 md_chks,
                 "",
                 paste0 ("Package License: ", checks$license),
                 "",
                 "---",
                 "")

    md_out <- c (md_out,
                 srr_checks (checks))

    # sec_nun is (1, 2) for (srr, non-srr) packages
    sec_num <- as.integer (!is.null (checks$srr)) + 1
    stats_rep <- pkgstats_format (checks, sec_num)

    md_out <- c (md_out,
                 stats_rep,
                 "",
                 pkg_network (checks, sec_num),
                 "",
                 "---",
                 "",
                 paste0 ("## ",
                         sec_num + 1,
                         ". `goodpractice` and other checks"),
                 "",
                 "<details>",
                 paste0 ("<summary>Details of goodpractice and other ",
                         "checks (click to open)</summary>"),
                 "<p>",
                 "",
                 ci_checks (checks),
                 "",
                 "---",
                 "",
                 goodpractice_checks (checks),
                 "",
                 "</p>",
                 "</details>")

    # Insert additional checks here, ensuring function name is unanbiguous,
    # generally by appending `_md` to indicate conversion to markdown format.
    md_out <- c (md_out,
                 scrap_checks_md (checks))
    # End insert additional checks sections

    # add package version info
    v <- data.frame (package = names (checks$pkg_versions),
                     version = checks$pkg_versions,
                     row.names = NULL)
    md_out <- c (md_out,
                 "",
                 "---",
                 "",
                 "<details>",
                 "<summary>Package Versions</summary>",
                 "<p>",
                 "",
                 knitr::kable (v),
                 "",
                 "</p>",
                 "</details>")

    if (render) {
        render_markdown (md_out, open = TRUE)
    }

    attr (md_out, "checks_okay") <- attr (md_chks, "checks_okay")
    attr (md_out, "is_noteworthy") <- attr (stats_rep, "is_noteworthy")
    attr (md_out, "network_file") <- checks$network_file
    attr (md_out, "srr_report_file") <- checks$srr$report_file

    return (md_out)
}

#' Report on \package{srr} compliance
#' @param checks Result of main \link{pkgcheck} function
#' @noRd
srr_checks <- function (checks) {

    if (is.null (checks$srr))
        return (NULL)

    cat_plural <- ifelse (length (checks$srr$categories == 1),
                          "category",
                          "categories")
    srr_msg <- ifelse (checks$srr$okay,
                       paste0 (symbol_tck (), " ", checks$srr$message),
                       paste0 (symbol_crs (), " ", checks$srr$message))

    c (paste0 ("## 1. rOpenSci Statistical Standards ",
               "([`srr` package](https://github.com/ropensci-review-tools/srr))"),
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

#' Format \pkg{pkgstats} data
#' @param checks Result of main \link{pkgcheck} function
#' @param sec_num Section numbering to use (1 for non-srr packages; otherwise
#' 2).
#' @return Report as formatted string
#' @noRd
pkgstats_format <- function (checks, sec_num) {

    is_noteworthy <- any (checks$pkgstats$noteworthy == "TRUE")
    note <- ifelse (is_noteworthy,
                    paste0 ("This package features some noteworthy ",
                            "statistical properties which may need to be ",
                            "clarified by a handling editor prior to ",
                            "progressing."),
                    paste0 ("The statistical properties of this package are ",
                            "all within expected ranges."))

    stats_rep <- c ("",
                    paste0 ("## ", sec_num, ". Statistical Properties"),
                    "",
                    note,
                    "",
                    "<details>",
                    paste0 ("<summary>Details of statistical properties ",
                            "(click to open)</summary>"),
                    "<p>",
                    "",
                    "The package has:",
                    "",
                    pkg_stat_desc (checks),
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
                    knitr::kable (checks$pkgstats,
                                  row.names = FALSE,
                                  digits = c (NA, 0, 1, NA)),
                    "",
                    "---",
                    "",
                    "</p></details>"
                    )

    attr (stats_rep, "is_noteworthy") <- is_noteworthy

    return (stats_rep)
}

#' Initial description of structural properties of package
#' @param checks Result of main \link{pkgcheck} function
#' @noRd
pkg_stat_desc <- function (checks) {

    stats <- checks$pkgstats
    loc <- attr (stats, "language")
    files <- attr (stats, "files")

    loc_pc <- gsub (".*\\:\\s?", "", loc)
    langs <- gsub ("\\:.*$", "", loc)
    files <- gsub (".*\\:\\s?", "", files)

    langs <- paste0 (langs, " (", loc_pc, " in ", files, " files)")

    code <- paste0 ("- code in ", langs [1])
    langs <- langs [-1]
    langs_first <- ""
    langs_last <- langs [length (langs)]
    if (length (langs) > 1) {
        langs_first <- paste0 (", ",
                               paste0 (langs [-length (langs)],
                                       collapse = ", "))
    }
    out <- paste0 (code, langs_first, " and ", langs_last)

    s <- checks$summary
    summarise_one <- function (s, what, pre_text, type) {
        ifelse (s [[what]] == 0L,
                paste0 ("- no ", pre_text, " ", type),
                paste0 ("- ", s [[what]], " ", pre_text, " ",
                        ifelse (s [[what]] == 1L,
                                type,
                                paste0 (type, "s"))))
    }

    out <- c (out,
              paste0 ("- ", s$num_authors, " authors"),
              summarise_one (s, "num_vignettes", "", "vignette"),
              summarise_one (s, "num_data", "internal", "data file"),
              summarise_one (s, "imported_pkgs", "imported", "package"),
              summarise_one (s, "num_exported_fns", "exported", "function"))

    if (length (s$loc_exported_fns) > 0L) {
        out [length (out)] <- paste0 (out [length (out)],
                                      " (median ",
                                      s$loc_exported_fns,
                                      " lines of code)")
    }

    out <- c (out,
              summarise_one (s, "num_non_exported_fns",
                             "non-exported",
                             "function"))
    out [length (out)] <- paste0 (out [length (out)], " in R")

    if (length (s$num_non_exported_fns) > 0L) {
        out [length (out)] <- paste0 (out [length (out)],
                                      " (median ",
                                      s$loc_non_exported_fns,
                                      " lines of code)")
    }

    if (s$num_src_fns > 0L) {
        lang_names <- gsub ("\\s.*$", "", langs)
        out <- c (out,
                  paste0 (summarise_one (s,
                                         "num_src_fns",
                                         lang_names,
                                         "function"),
                          " (median ",
                          s$loc_src_fns,
                          " lines of code)"))
    }

    return (out)
}

#' Output text and URL link to function call network as 'vis.js' file.
#' @param checks Result of main \link{pkgcheck} function
#' @param sec_num Section numbering to use (1 for non-srr packages; otherwise
#' 2).
#' @noRd
pkg_network <- function (checks, sec_num) {

    out <- c ("",
              paste0 ("### ", sec_num, "a. Network visualisation"),
              "")

    if (!"network_file" %in% names (checks))
        return (c (out,
                   paste0 ("This package contains no internal function calls, ",
                           "and therefore no function call network")))

    cache_dir <- Sys.getenv ("PKGCHECK_CACHE_DIR")
    visjs_dir <- file.path (cache_dir, "static") # in api.R

    flist <- list.files (visjs_dir,
                         pattern = paste0 (checks$package, "_pkgstats"),
                         full.names = TRUE)

    if (!checks$network_file %in% flist) {

        unlink (flist, recursive = TRUE)
        visjs_ptn <- basename (checks$network_file)
        visjs_ptn <- tools::file_path_sans_ext (visjs_ptn)
        flist <- list.files (dirname (checks$network_file),
                             pattern = visjs_ptn,
                             full.names = TRUE)

        file.copy (flist, visjs_dir, recursive = TRUE)
    }

    return (c (out,
               paste0 ("Interactive network visualisation of calls ",
                       "between objects in package can be viewed by ",
                       "[clicking here](",
                       checks$network_file,
                       ")")))
}

#' Report on continuous integration checks
#' @param checks Result of main \link{pkgcheck} function
#' @noRd
ci_checks <- function (checks) {

    out <- NULL

    if (length (checks$badges) > 0) {

        if (is.na (checks$badges [1]))
            checks$badges <- "(There do not appear to be any)"

        out <- c (out,
                  "### 3a. Continuous Integration Badges",
                  "",
                  unlist (checks$badges),
                  "")

        if (!is.null (checks$github_workflows)) {

            out <- c (out,
                      "**GitHub Workflow Results**",
                      "",
                      knitr::kable (checks$github_workflows))
        }
    }

    return (out)
}

#' Report on goodpractice checks
#' @param checks Result of main \link{pkgcheck} function
#' @param control A named list of parameters potentially including
#' `cyclocomp_threshold`, `covr_threshold`, and `covr_digits`, where
#' reports are generated for cyclocomplexity values above threshold, and
#' coverage values below threshold (given as percentage). `digits` controls the
#' number of digits printed in coverage reports.
#' @noRd
goodpractice_checks <- function (checks,
                                 control = list (cyclocomp_threshold = 15,
                                                 covr_threshold = 70,
                                                 digits = 2)) {

    gp <- extract_gp_components (checks$gp)


    c ("",
       "### 3b. `goodpractice` results",
       "",
       "",
       convert_gp_components (gp, control = control),
       "")
}

scrap_checks_md <- function (checks) {

    if (length (checks$scrap) == 0L)
        return (NULL)

    c ("",
       ":heavy_check_mark: Package contains the following unexpected files:",
       "",
       paste0 ("- ", checks$scrap),
       "")
}

#' render markdown-formatted input into 'html'
#'
#' @param md Result of \link{checks_to_markdown} function.
#' @param open If `TRUE`, open `hmtl`-rendered version in web browser.
#' @return (invisible) Location of `.html`-formatted version of input.
#' @family extra
#' @export
render_markdown <- function (md, open = TRUE) {

    md <- gsub ("\\:heavy\\_check\\_mark\\:", "&#9989;", md)
    md <- gsub ("\\:heavy\\_multiplication\\_x\\:", "&#10060;", md)

    fmd <- tempfile (pattern = "pkgcheck", fileext = ".Rmd")
    writeLines (md, con = fmd)
    f <- tempfile (pattern = "pkgcheck", fileext = ".html")
    rmarkdown::render (fmd, output_file = f)

    if (open)
        utils::browseURL (f)

    invisible (f)
}
