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
  expect_true (nzchar (ci_out$print))

  # Some examples use `\dontrun{}`
  checks$checks$uses_dontrun ["pkgstats_summary"] <- FALSE
  ci_out <- output_pkgchk_uses_dontrun (checks)

  expect_false (ci_out$check_pass)
  expect_equal (
    ci_out$summary, 
    "Examples should not use `\\dontrun{{}}` unless really necessary."
  )
  
  # No examples using `\dontrun{}`
  checks$checks$uses_dontrun[] <- FALSE
  ci_out <- output_pkgchk_uses_dontrun (checks)

  expect_true (ci_out$check_pass)
  expect_equal (ci_out$summary, "")
  expect_equal (ci_out$print, "")
})
