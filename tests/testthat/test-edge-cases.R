skip_on_cran ()

local_pkg_dir <- function (desc = list (), env = parent.frame ()) {
    path <- withr::local_tempdir (.local_envir = env)
    desc <- c (
        list (
            Package = "testpkg", Version = "0.0.1", Title = "Test",
            Description = "Test.", License = "GPL-3"
        ),
        desc
    )
    desc <- desc [which (!vapply (desc, is.null, logical (1L)))]
    write.dcf (
        as.data.frame (desc, check.names = FALSE),
        fs::path (path, "DESCRIPTION"),
        width = 10000L
    )
    return (path)
}

test_that ("URLs from DESCRIPTION", {

    url <- function (u, bugs = NULL) {
        path <- local_pkg_dir (list (URL = u, BugReports = bugs), env = environment ())
        pkginfo_url_from_desc (path, type = "URL")
    }

    expect_equal (url ("http://example.org/p"), "http://example.org/p")
    expect_equal (
        url ("https://github.com/a/p https://gitlab.com/a/p"),
        "https://github.com/a/p"
    )
    expect_equal (
        url ("https://a.r-universe.dev/p, https://codeberg.org/a/p"),
        "https://a.r-universe.dev/p"
    )
    expect_equal (
        url ("https://a.github.io/p, https://github.com/a/p"),
        "https://github.com/a/p"
    )
    expect_equal (
        url ("https://a.github.io/p", "https://gitlab.com/o/r/-/issues"),
        "https://gitlab.com/o/r"
    )
    expect_equal (url ("ftp://example.org"), "")

    expect_equal (rm_issues_from_url ("https://github.com/o/r/issues/"), "https://github.com/o/r")
    expect_equal (rm_issues_from_url ("https://gitlab.com/o/r/-/issues"), "https://gitlab.com/o/r")
})

test_that ("ORCID and ROR checks", {

    auts <- function (x, env = parent.frame ()) {
        path <- local_pkg_dir (list ("Authors@R" = x), env = env)
        list (pkg = list (path = path))
    }

    checks <- auts (paste0 (
        "c (person (c ('Kelli', 'F.'), 'Johnson', role = c ('aut', 'cre'), ",
        "comment = c (ORCID = '0000-0002-5149-451X')), ",
        "person ('Ada', 'Lovelace', role = 'ctb'), ",
        "person (family = 'Wellcome Trust', role = 'fnd'), ",
        "person (c ('Big', 'Funder'), role = 'fnd', comment = c (ROR = '012345678')))"
    ))
    expect_identical (
        pkgchk_has_orcid (checks),
        c ("Kelli F. Johnson" = TRUE, "Ada Lovelace" = FALSE)
    )
    expect_identical (
        pkgchk_has_ror (checks),
        c ("Wellcome Trust" = FALSE, "Big Funder" = TRUE)
    )

    # No 'Authors@R' field:
    path <- local_pkg_dir (list (Author = "A B", Maintainer = "A B <a@b.c>"))
    checks <- list (pkg = list (path = path))
    expect_length (pkgchk_has_orcid (checks), 0L)
    expect_length (pkgchk_has_ror (checks), 0L)
})

test_that ("CI check needs CI workflows", {

    wf <- data.frame (name = c ("pkgdown", "lint"), conclusion = "success")
    checks <- list (
        checks = list (has_url = TRUE),
        info = list (badges = list (), github = list (workflows = wf))
    )
    out <- output_pkgchk_ci (checks)
    expect_false (out$check_pass)
    expect_equal (out$summary, " Package has no continuous integration checks.")
})

test_that ("package names when CRAN can not be checked", {

    path <- local_pkg_dir (list (Package = "my_pkg"))
    checks <- list (pkg = list (path = path))

    local_mocked_bindings (get_available_packages = function () NULL)
    expect_true (is.na (pkgchk_pkgname_available (checks)))
    expect_true (is.na (pkgchk_on_cran (checks)))

    local_mocked_bindings (
        get_available_packages = function () data.frame (Package = "ggplot2")
    )
    # Invalid name:
    expect_false (pkgchk_pkgname_available (checks))
})

test_that ("percentiles with ties", {

    expect_equal (mid_rank_percentile (c (0, 0, 0, 1), 0), 0.375)
    expect_equal (mid_rank_percentile (c (1, 2, 3, 4), 5), 1)
    expect_equal (mid_rank_percentile (c (1, 2, 3, 4), 0), 0)
    expect_true (is.na (mid_rank_percentile (1:4, NA)))
})

test_that ("number of imported packages", {

    expect_equal (num_imported_pkgs (NA_character_), 0L)
    expect_equal (num_imported_pkgs ("NA"), 0L)
    expect_equal (num_imported_pkgs ("a, b"), 2L)
})

test_that ("examples in Rd files with multiple keywords", {

    path <- withr::local_tempdir ()
    rd <- function (nm, extra) {
        f <- fs::path (path, paste0 (nm, ".Rd"))
        writeLines (c (
            paste0 ("\\name{", nm, "}"), paste0 ("\\alias{", nm, "}"),
            paste0 ("\\title{", nm, "}"), "\\description{x}", extra
        ), f)
        return (f)
    }
    f <- c (
        rd ("f", c ("\\keyword{misc}", "\\keyword{utilities}")),
        rd ("g", c ("\\keyword{misc}", "\\examples{g ()}")),
        rd ("h", c ("\\keyword{utilities}", "\\keyword{internal}"))
    )
    expect_identical (fns_have_exs (f), c (f = FALSE, g = TRUE))
})

test_that ("contributing files in renv are ignored", {

    path <- withr::local_tempdir ()
    renv <- fs::path (path, "renv", "library", "pkg")
    fs::dir_create (renv)
    writeLines ("x", fs::path (renv, "CONTRIBUTING.md"))
    checks <- list (pkg = list (path = path))
    expect_false (pkgchk_has_contrib_md (checks))

    writeLines ("x", fs::path (path, "CONTRIBUTING.md"))
    expect_true (pkgchk_has_contrib_md (checks))
})

test_that ("default branch", {

    expect_equal (git_default_branch (list (branch = "master", branch_default = "main")), "main")
    expect_equal (git_default_branch (list (branch = "master")), "master")
    expect_equal (git_default_branch (list (branch = "HEAD", branch_default = "master")), "master")

    local_mocked_bindings (is_test_env = function () FALSE)
    checks <- list (info = list (git = list (branch = "master", branch_default = "main")))
    expect_false (pkgchk_branch_is_master (checks))
    checks$info$git <- list (branch = "feature", branch_default = "master")
    expect_true (pkgchk_branch_is_master (checks))
})

test_that ("srr probability distributions", {

    cats <- srr_categories_from_report (c ("PD1.0", "PD2.1a", "RE1.1", "G1.0"))
    expect_identical (
        cats,
        c ("Probability Distributions", "Regression and Supervised Learning")
    )
})

test_that ("extra env checks", {

    e <- new.env ()
    e$pkgchk_bad_files <- function (checks) c ("R/a.R", "R/b.R")
    e$pkgchk_flag <- function (checks) TRUE
    e$output_pkgchk_flag <- function (checks) {
        list (check_pass = TRUE, summary = "", print = "")
    }
    withr::local_options (pkgcheck_extra_env = e)

    chks <- collate_extra_env_checks (list ())
    expect_identical (chks, list (bad_files = c ("R/a.R", "R/b.R"), flag = TRUE))

    checks <- list (checks = chks)
    expect_null (summarise_extra_env_checks (checks))
})

test_that ("pkgdown config in sub-directory", {

    path <- withr::local_tempdir ()
    fs::dir_create (fs::path (path, "pkgdown"))
    writeLines (
        c ("reference:", "- title: x"),
        fs::path (path, "pkgdown", "_pkgdown.yml")
    )
    out <- pkginfo_pkgdown (path)
    expect_true (out$has_reference)
    expect_equal (basename (out$yaml_path), "_pkgdown.yml")

    out <- pkginfo_pkgdown (withr::local_tempdir ())
    expect_false (out$has_reference)
})

test_that ("left assign checks in non-interactive sessions", {

    withr::local_options (keep.source = FALSE)
    path <- withr::local_tempdir ()
    fs::dir_create (fs::path (path, "R"))
    writeLines (
        c ("f <- function (x) {", "    y = x", "    y", "}"),
        fs::path (path, "R", "f.R")
    )
    checks <- list (pkg = list (path = path))
    out <- pkgchk_left_assign (checks)
    expect_equal (unname (out$usage), c (1L, 1L))
})

test_that ("summary language text", {

    mk_checks <- function (langs) {
        stats <- data.frame ()
        attr (stats, "language") <- paste0 (names (langs), ": ", langs, "%")
        attr (stats, "files") <- paste0 (names (langs), ": 2")
        list (
            info = list (pkgstats = stats),
            pkg = list (summary = list (
                num_authors = 1L, num_vignettes = 0L, num_data = 0L,
                imported_pkgs = 1L, num_exported_fns = 1L,
                loc_exported_fns = 3L, num_non_exported_fns = 1L,
                loc_non_exported_fns = 3L, num_src_fns = 2L, loc_src_fns = 5L
            ))
        )
    }

    out <- pkg_stat_desc (mk_checks (c (R = 100)))
    expect_equal (out [1], "- code in R (100% in 2 files)")
    expect_true ("- 1 author" %in% out)
    expect_true ("- no vignette" %in% out)

    out <- pkg_stat_desc (mk_checks (c (`C++` = 72, R = 28)))
    expect_equal (out [1], "- code in C++ (72% in 2 files) and R (28% in 2 files)")
    expect_true (any (grepl ("^- 2 C\\+\\+ functions", out)))

    out <- pkg_stat_desc (mk_checks (c (R = 50, C = 25, `C++` = 25)))
    expect_true (any (grepl ("^- 2 C, C\\+\\+ functions", out)))
})

test_that ("left assign checks with no R files", {

    path <- withr::local_tempdir ()
    fs::dir_create (fs::path (path, "R"))
    out <- pkgchk_left_assign (list (pkg = list (path = path)))
    expect_false (out$global)
    expect_equal (unname (out$usage), c (0L, 0L))
})

test_that ("unreadable Rd files", {

    path <- withr::local_tempdir ()
    f <- fs::path (path, "man.Rd")
    fs::dir_create (f) # a directory, not a file
    msgs <- character (0L)
    rds <- withCallingHandlers (
        parse_rd_files (f),
        warning = function (w) {
            msgs <<- c (msgs, conditionMessage (w))
            invokeRestart ("muffleWarning")
        }
    )
    expect_length (rds, 0L)
    expect_true (any (grepl ("Error parsing Rd file", msgs)))
})

test_that ("left assign checks with no R directory", {

    out <- pkgchk_left_assign (list (pkg = list (path = withr::local_tempdir ())))
    expect_false (out$global)
})

test_that ("network description on GitHub actions", {

    withr::local_envvar (list (GITHUB_ACTIONS = "true", GITHUB_REPOSITORY = "org/repo"))
    local_mocked_bindings (is_test_env = function () FALSE)
    out <- d3js_description (list ())
    expect_true (grepl ("(https://github.com/org/repo/actions)", out, fixed = TRUE))
})

test_that ("global assign checks without ctags", {

    local_mocked_bindings (
        get_ctags = function (...) stop ("no ctags"),
        .package = "pkgstats"
    )
    path <- withr::local_tempdir ()
    fs::dir_create (fs::path (path, "R"))
    writeLines (
        c ("f <- function () {", "    x <<- 1", "}"),
        fs::path (path, "R", "f.R")
    )
    out <- pkgchk_left_assign (list (pkg = list (path = path)))
    expect_true (out$global)
})
