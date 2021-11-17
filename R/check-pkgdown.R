
output_pkgchk_pkgdown <- function (checks) {

    # Grouped concepts must mean at least 2, so check is > 1:
    out <- list (
        check_pass = length (checks$info$pkgdown_concepts) > 1L,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- paste0 (
            "Function documentation entries are not grouped by concept"
        )
    }

    return (out)
}
