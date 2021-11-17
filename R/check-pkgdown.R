
output_pkgchk_pkgdown <- function (checks) {

    out <- list (
        check_pass = length (checks$info$pkgdown_concepts) > 0L,
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
