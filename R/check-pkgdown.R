# This check is currently active, but can be modified and switched on to check
# pkgdown-related aspects of package documentation.
output_pkgchk_pkgdown <- function (checks) {

    # Skip pkgdown checks if number of .Rd files is below this threshold.
    ref_threshold <- 10L

    # Grouped concepts must mean at least 2, so check is > 1:
    out <- list (
        check_pass = length (checks$info$pkgdown) > 1L,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        # out$summary <- paste0 (
        #    "Function documentation entries are not grouped by concept"
        # )
    }

    return (out)
}
