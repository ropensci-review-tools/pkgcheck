cli::test_that_cli ("use_github_action_pkgcheck", {

    dir <- fs::path (fs::file_temp (pattern = "pkgcheck"), ".github")
    # nolint start
    expect_snapshot_error (use_github_action_pkgcheck (file_name = 23))
    expect_snapshot_error (use_github_action_pkgcheck (file_name = c ("some", "files")))

    dir.create (dir, recursive = TRUE)
    path <- fs::path (dir, "pkgcheck.yaml")
    file.create (path)
    expect_error (use_github_action_pkgcheck (dir = dir), "already exists")
    file.remove (path)
    expect_snapshot_error (use_github_action_pkgcheck (dir = dir, inputs = "not a list"))
    expect_error (use_github_action_pkgcheck (dir = dir, inputs = list (notaninput = 23)), "not valid")

    # Success - but skip on windows and mac because GHA machines use completely
    # different paths
    testthat::skip_on_os ("windows")
    testthat::skip_on_os ("mac")
    gha_path <- use_github_action_pkgcheck (dir = dir)
    expect_equal (path, gha_path)

    expect_snapshot_file (path)
    expect_snapshot_file (use_github_action_pkgcheck (dir = dir, file_name = "with_inputs.yaml", inputs = list (`post-to-issue` = "true", `summary-only` = "false", ref = "main")))

})
