test_that ("check ORCID ID", {
  checks <- make_check_data ()
  ci_out <- output_pkgchk_has_orcid  (checks)
  
  expect_true (ci_out$check_pass)
  expect_length (ci_out$summary, 1L)
  expect_equal (ci_out$summary, "All authors have ORCID IDs")
  expect_length (ci_out$print, 1L)
  expect_false (nzchar (ci_out$print))

  checks$checks$has_orcid <- c ("John" = FALSE, "Jane" = TRUE)
  ci_out <- output_pkgchk_has_orcid (checks)

  expect_false (ci_out$check_pass)
  expect_equal (ci_out$summary, "Not all authors have ORCID IDs")
  expect_equal (ci_out$print$msg_pre, "The following authors are missing ORCID IDs:")
  expect_equal (ci_out$print$obj, "John")
})

test_that ("Check ROR", {
  checks <- make_check_data ()
  ci_out <- output_pkgchk_has_ror (checks)
  
  expect_true (ci_out$check_pass)
  expect_length (ci_out$summary, 1L)
  expect_false (nzchar (ci_out$summary))
  expect_length (ci_out$print, 1L)
  expect_false (nzchar (ci_out$print))

  checks$checks$has_ror <- c ("rOpenSci" = FALSE)
  ci_out <- output_pkgchk_has_ror (checks)

  expect_false (ci_out$check_pass)
  expect_equal (ci_out$summary, "Institutions listed as authors without RORs")
  expect_equal (ci_out$print$msg_pre, "The following institutions are missing RORs:")
  expect_equal (ci_out$print$obj, "rOpenSci")
})