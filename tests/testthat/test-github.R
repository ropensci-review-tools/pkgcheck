cli::test_that_cli("use_github_action_pkgcheck", {
  dir <- file.path(tempdir(),basename(tempfile("pkgcheck")), ".github")
# nolint start
expect_snapshot_error(use_github_action_pkgcheck(file_name = 23))
expect_snapshot_error(use_github_action_pkgcheck(file_name = c("some", "files")))

  dir.create(dir, recursive = TRUE)
  path <- file.path(dir, "pkgcheck.yaml")
  file.create(path)
expect_error(use_github_action_pkgcheck(dir = dir), "already exists")
file.remove(path)
expect_snapshot_error(use_github_action_pkgcheck(dir = dir, inputs = "not a list"))
expect_error(use_github_action_pkgcheck(dir = dir, inputs = list(notaninput = 23)), "not valid")

# Success
# macOS GitHub runners append "/private" to paths when they are actually created:
gha_path <-  gsub ("^\\/private", "", use_github_action_pkgcheck(dir = dir))
expect_equal(gha_path, path)
gha_path <- gsub ("^\\/private", "", use_github_action_pkgcheck(dir = dir, overwrite = TRUE))
expect_equal(gha_path, path)
expect_snapshot_file(path)
expect_snapshot_file(use_github_action_pkgcheck(dir = dir, file_name = "with_inputs.yaml", inputs = list(`post-to-issue` = "true", `summary-only` = "false", ref = "main")))

})
