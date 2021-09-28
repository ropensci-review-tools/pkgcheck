# CI checks have no print method, as they are currently only put into the full
# html report, and otherwise only appear in summary.

#' Get all CI badges from a repository
#'
#' @param u URL of repo
#' @return Character vector of hyperlinked badge images
#' @noRd
pkgchk_ci_badges <- function (u) {

    if (!curl::has_internet ())
        return (NULL)

    orgrepo <- strsplit (u, "\\/") [[1]]
    org <- utils::tail (orgrepo, 2) [1]
    repo <- utils::tail (orgrepo, 1)
    # note: default branch is github only, so will only work if repo is also
    # mirrored on github!
    branch <- get_default_branch (org, repo)

    if (grepl ("github", u)) {

        u_readme <- paste0 ("https://raw.githubusercontent.com/",
                            org,
                            "/",
                            repo,
                            "/",
                            branch,
                            "/README.md")

    } else if (grepl ("gitlab", u)) {

        u_readme <- paste0 ("https://gitlab.com/",
                            org,
                            "/",
                            repo,
                            "/-/raw/",
                            branch,
                            "/README.md")
    }

    if (!url_exists (u_readme, quiet = TRUE))
        return (NULL)

    f <- tempfile (fileext = ".md")
    chk <- utils::download.file (u_readme, destfile = f, quiet = TRUE) # nolint
    readme <- readLines (f, encoding = "UTF-8")

    badges <- unlist (regmatches (readme,
                                  gregexpr ("https.*\\.svg", readme)))
    if (length (badges) == 0)
        return (NULL)
    platforms <- c ("github", "travis", "gitlab")
    badges <- badges [grep (paste0 (platforms, collapse = "|"),
                            badges)]
    for (p in platforms) {

        index <- grep (p, badges)
        p_u <- p

        if (p == "github") {

            wf_nms <- vapply (badges [index], function (i)
                              utils::tail (strsplit (i, "/") [[1]], 2) [1],
                              character (1),
                              USE.NAMES = FALSE)
            p_u <- paste0 ("https://github.com/",
                           org,
                           "/",
                           repo,
                           "/actions")
        } else if (p == "travis") {

            p_u <- gsub ("\\.svg$", "", badges [index])
        }

        badges [index] <- paste0 ("[![",
                                  p,
                                  "](",
                                  badges [index],
                                  ")](",
                                  p_u,
                                  ")")
    }

    return (badges)
}


#' Summarise checks from continuous integration
#'
#' @return tick or cross
#' @noRd
summarise_ci_checks <- function (checks) {

    if (length (checks$badges) == 0) {

        if (!checks$file_list$has_url) {

            res <- paste0 ("- ", symbol_crs (),
                           " Continuous integration checks unavailable ",
                           "(no URL in 'DESCRIPTION').")
        } else {

            res <- paste0 ("- ", symbol_crs (),
                           " Package has no continuous integration checks.")
        }
    } else {

        res <- paste0 ("- ", symbol_tck (),
                       " Package has continuous integration checks.")
    }

    return (res)
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


#' CI results for GitHub only
#' @inheritParams pkg_uses_roxygen2
#' @return A 'data.frame' with one row for each GitHub workflow, and columns for
#' name, the 'conclusion' status, the git 'sha', and the date.
#' @noRd
ci_results_gh <- function (path) {

    d <- data.frame (read.dcf (file.path (path, "DESCRIPTION")))
    if (!"URL" %in% names (d))
        return ("Error: Description has no URL")

    u <- strsplit (d$URL, "\\s+") [[1]]
    u <- u [grep ("^https://github\\.com", u)]
    url <- strsplit (u, "\\/") [[1]]
    org <- utils::tail (url, 2) [1]
    repo <- utils::tail (url, 1)

    url <- paste0 ("https://api.github.com/repos/",
                   org,
                   "/",
                   repo,
                   "/actions/runs")

    runs <- httr::GET (url) %>% httr::content ()

    if (!"total_count" %in% names (runs))
        return (NULL)

    if (runs$total_count == 0)
        return (NULL)

    dat <- lapply (runs$workflow_runs, function (i) {
                       # in-progress runs have no conclusion entry:
                       concl <- i$conclusion
                       if (is.null (concl))
                           concl <- ""
                   data.frame (name = i$name,
                               status = i$status,
                               conclusion = concl,
                               sha = i$head_sha,
                               time = i$created_at)
                   })
    dat <- do.call (rbind, dat)
    dat$time <- strptime (dat$time, "%Y-%m-%dT%H:%M:%SZ")
    dat$time_dbl <- as.double (dat$time)
    # non-dply group_by %>% summarise:
    dat <- lapply (split (dat, f = as.factor (dat$name)),
                   function (i)
                       i [which.max (i$time_dbl), ])
    dat <- do.call (rbind, dat)

    dat$sha <- substring (dat$sha, 1, 6)
    dat$date <- strftime (dat$time, "%Y-%m-%d")
    rownames (dat) <- dat$time_dbl <- dat$time <- dat$status <- NULL

    return (dat)
}
