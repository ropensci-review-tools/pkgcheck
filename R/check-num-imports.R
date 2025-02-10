#' Check numbers of package dependencies ('Imports')
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Integer value of number of package "Imports"
#' @noRd
pkgchk_num_imports <- function (checks) {

    deps <- checks$pkg$dependencies
    num_imports <- length (which (deps$type == "imports"))

    return (num_imports)
}

output_pkgchk_num_imports <- function (checks) {

    import_threshold <- 0.95

    ndeps_all <- retrieve_all_pkg_deps ()
    ndeps_pc <- match (checks$checks$num_imports, ndeps_all) / length (ndeps_all)

    out <- list (
        check_pass = ndeps_pc < import_threshold,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- paste0 (
            "Package has unusually large number of ",
            checks$checks$num_imports,
            " Imports (> ",
            floor (100 * ndeps_pc),
            "% of all packages)"
        )
    }

    return (out)
}

retrieve_all_pkg_deps <- function () {

    ap <- data.frame (utils::available.packages ())
    num_imports <- vapply (ap$Imports, function (i) {
        ifelse (is.na (i), 0L, length (strsplit (i, ",") [[1]]))
    }, integer (1L), USE.NAMES = FALSE)
    return (sort (num_imports))
}
