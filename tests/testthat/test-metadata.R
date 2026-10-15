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

test_that ("network description on GitHub actions", {

    withr::local_envvar (list (GITHUB_ACTIONS = "true", GITHUB_REPOSITORY = "org/repo"))
    local_mocked_bindings (is_test_env = function () FALSE)
    out <- d3js_description (list ())
    expect_true (grepl ("(https://github.com/org/repo/actions)", out, fixed = TRUE))
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

    path <- local_pkg_dir (list (Author = "A B", Maintainer = "A B <a@b.c>"))
    checks <- list (pkg = list (path = path))
    expect_length (pkgchk_has_orcid (checks), 0L)
    expect_length (pkgchk_has_ror (checks), 0L)
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
    expect_false (pkgchk_pkgname_available (checks))
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
