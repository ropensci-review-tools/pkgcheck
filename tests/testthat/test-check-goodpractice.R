# This fails to find cache paths on GitHub windows runners
skip_on_os ("windows")

test_that ("check goodpractice", {

    checks <- make_check_data_srr (goodpractice = TRUE)

    make_errors <- function (checks, n_errors = 2L) {
        checks$goodpractice$rcmdcheck <- list (
            errors = letters [seq_len (n_errors)],
            warnings = letters [seq_len (n_errors)],
            notes = letters [seq_len (n_errors)],
            test_fail = letters [seq_len (n_errors)]
        )
        return (checks)
    }
    n_errors <- 3L
    checks <- make_errors (checks, n_errors = n_errors)

    x <- summarise_gp_checks (checks)
    expect_type (x, "list")
    expect_length (x, 2L)
    expect_named (x, c ("rcmd_errs", "rcmd_warns"))
    expect_true (grepl (paste0 ("found ", n_errors, " errors"), x$rcmd_errs))
    expect_true (grepl (paste0 ("found ", n_errors, " warnings"), x$rcmd_warns))

    cmps <- extract_gp_components (checks$goodpractice)
    what <- c ("errors", "warnings", "notes", "test_fails")
    expect_true (all (what %in% names (cmps$rcmd)))
    expect_length (cmps$rcmd$errors, n_errors)
    expect_length (cmps$rcmd$warnings, n_errors)
    expect_length (cmps$rcmd$notes, n_errors)
    expect_length (cmps$rcmd$test_fails, n_errors)

    # ------ rcmdcheck report
    rep <- rcmd_report (cmps)
    expect_gt (length (rep), n_errors * 4L)

    cmps$rcmd <- NULL
    rep <- rcmd_report (cmps)
    expect_length (grep ("rcmdcheck found no errors, warnings, or notes", rep), 1L)

    # ------ covr report
    checks <- make_check_data_srr (goodpractice = TRUE)
    gp <- checks$goodpractice
    rep <- covr_report (gp)
    expect_gt (length (rep), 1L)
    expect_gt (length (grepl ("test coverage", rep, ignore.case = TRUE)), 1L)

    gp$covr <- NULL
    rep <- covr_report (gp)
    expect_gt (length (rep), 1L)
    expect_gt (length (grepl ("test coverage", rep, ignore.case = TRUE)), 1L)

    gp$covr <- data.frame (
        source = "package",
        percent = 56.7
    )
    rep <- covr_report (gp)
    expect_false (any (grepl ("fail", rep, ignore.case = TRUE)))
    expect_true (any (grepl ("coverage\\:\\s[0-9]+", rep)))

    rep2 <- covr_report (gp, control = list ())
    expect_identical (rep, rep2)

    # Some of these fail on windows
    skip_on_os ("windows")

    # ------ cyclocomp report
    gp <- checks$goodpractice
    rep <- cyclo_report (gp)
    expect_type (rep, "character")
    expect_true (any (grepl ("No functions have cyclocomplexity", rep)))

    gp$cyclocomp$cyclocomp <- c (1, 15, 30)
    rep <- cyclo_report (gp)
    expect_false (any (grepl ("No functions have cyclocomplexity", rep)))
    expect_true (any (grepl ("functions have cyclocomplexity >= 15", rep)))

    # ------ lintr report
    gp <- checks$goodpractice
    rep <- lintr_report (gp)
    expect_type (rep, "character")
    expect_true (any (grepl (
        "found the following [0-9] potential issues",
        rep
    )))

    gp$lintr <- NULL
    rep <- lintr_report (gp)
    expect_type (rep, "character")
    expect_true (any (grepl ("no issues with this package", rep)))
})
