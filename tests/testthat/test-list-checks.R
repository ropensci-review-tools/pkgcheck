
test_that("list-checks", {

      expect_message (
          chks <- list_pkgchecks (),
          "The following checks are currently implemented"
      )

      expect_length (chks, 18L)
})
