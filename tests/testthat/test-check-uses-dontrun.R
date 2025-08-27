test_that ("check examples dont use dontrun", {

  checks <- make_check_data ()

  # All examples use `\dontrun{}`
  ci_out <- output_pkgchk_uses_dontrun (checks)
  
  expect_type (ci_out, "list")
  expect_length (ci_out, 3L)
  expect_named (ci_out, c ("check_pass", "summary", "print"))
  expect_false (ci_out$check_pass)
  expect_equal (
      ci_out$summary,
      "All examples use `\\dontrun{}`."
  )
  expect_length (ci_out$print, 1L)
  expect_snapshot (ci_out$print)

  # Some examples use `\dontrun{}`
  checks$checks$uses_dontrun ["pkgstats_summary"] <- "some"
  ci_out <- output_pkgchk_uses_dontrun (checks)

  expect_false (ci_out$check_pass)
  expect_equal (
    ci_out$summary, 
    "Examples should not use `\\dontrun{}` unless really necessary."
  )
  expect_match(
    ci_out$print, 
    "The following functions have examples that use `\\dontrun{}`", 
    fixed = TRUE
  )
  
  # No examples using `\dontrun{}`
  checks$checks$uses_dontrun[] <- "none"
  ci_out <- output_pkgchk_uses_dontrun (checks)

  expect_true (ci_out$check_pass)
  expect_equal (ci_out$summary, "")
  expect_equal (ci_out$print, "")
})

test_that ("uses_dontrun works", {

  rd_files <- system.file("Rd-for-testing-dontrun.Rd", package = "pkgcheck")

  uses_dontrun_output <- rd_files_use_dontrun (rd_files)

  expect_type (uses_dontrun_output, "character")
  expect_length (uses_dontrun_output, 1L)
  expect_named (uses_dontrun_output, "example_function")
  expect_equal (uses_dontrun_output, c(example_function = "some"))
})
