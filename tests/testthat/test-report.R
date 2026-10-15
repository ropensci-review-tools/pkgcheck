skip_on_cran ()

test_that ("number of imported packages", {

    expect_equal (num_imported_pkgs (NA_character_), 0L)
    expect_equal (num_imported_pkgs ("NA"), 0L)
    expect_equal (num_imported_pkgs ("a, b"), 2L)
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
