#' Check numbers of package dependencies ('Imports')
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Numeric vector of two values of (number of package "Imports", and
#' proportional threshold of all packages with more imports).
#' @noRd
pkgchk_num_imports <- function (checks) {

    deps <- checks$pkg$dependencies
    n <- length (which (deps$type == "imports" & !deps$package == "NA"))

    ndeps_all <- retrieve_all_pkg_deps ()
    ndeps_pc <- length (which (ndeps_all <= n)) / length (ndeps_all)
    # If available.packages() fails, then 'ndeps_pc' == NaN:
    not_a_number <- length (ndeps_pc) == 0L || is.na (ndeps_pc)
    ndeps_pc <- ifelse (not_a_number, 0L, ndeps_pc)

    return (c (n, ndeps_pc))
}

output_pkgchk_num_imports <- function (checks) {

    import_threshold <- 0.95

    out <- list (
        check_pass = checks$checks$num_imports [2] < import_threshold,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- paste0 (
            "Package has unusually large number of ",
            round (checks$checks$num_imports [1]),
            " Imports (> ",
            floor (100 * checks$checks$num_imports [2]),
            "% of all packages)"
        )
    }

    return (out)
}

retrieve_all_pkg_deps <- function () {

    ap <- get_available_packages ()
    num_imports <- vapply (ap$Imports, function (i) {
        ifelse (is.na (i), 0L, length (strsplit (i, ",") [[1]]))
    }, integer (1L), USE.NAMES = FALSE)
    return (sort (num_imports))
}
