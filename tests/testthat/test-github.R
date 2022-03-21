test_that ("use_github_action_pkgcheck", {

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
    gha_path <- use_github_action_pkgcheck (dir = dir, branch = "main")
    expect_equal (path, gha_path)

    expect_snapshot_file (path)
    expect_snapshot_file (
        use_github_action_pkgcheck (dir = dir,
                                    file_name = "with_inputs.yaml",
                                    branch = "main",
                                    inputs = list (`post-to-issue` = "true",
                                                   `summary-only` = "false",
                                                   ref = "main")))

})

test_that ("yaml branch", {

    yaml <- system.file (
        "pkgcheck.yaml",
        package = "pkgcheck",
        mustWork = TRUE
    ) %>% readLines ()

    expect_true (any (grepl ("^\\s+\\-\\s*main$", yaml)))

    # hard-code branch to avoid gert call, which may fail on GHA:
    yaml1 <- add_branch_to_yaml (yaml, branch = "main")
    expect_identical (yaml, yaml1)

    yaml2 <- add_branch_to_yaml (yaml, branch = "new-branch")
    expect_true (length (yaml2) > length (yaml1))
    expect_equal (length (yaml2) - length (yaml1), 1L)
    expect_true (any (grepl ("^\\s+\\-\\s*new-branch$", yaml2)))

    pos1 <- grep ("^\\s+\\-\\s*main$", yaml)
    pos2 <- grep ("^\\s+\\-\\s*new-branch$", yaml2)
    expect_equal (pos2 - pos1, 1L)
})
