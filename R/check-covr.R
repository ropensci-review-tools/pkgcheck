
output_pkgchk_covr <- function (checks) {

    out <- list (check_pass = TRUE,
                summary = "",
                print = "")

    if (methods::is (checks$goodpractice$covr, "try-error")) {

        out$check_pass <- FALSE
        out$summary <- "Package coverage failed"

    } else {

        coverage <- round (checks$goodpractice$covr$pct_by_line, digits = 1)

        if (coverage < 75) {

            out$check_pass <- FALSE
            out$summary <- paste0 ("Package coverage is ",
                                   coverage,
                                   "% (should be at least 75%).")

        } else {

            out$summary <- paste0 ("Package coverage is ", coverage, "%.")

        }
    }

    return (out)
}
