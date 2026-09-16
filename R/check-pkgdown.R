#' If >= 10 '.Rd' files, check that '_pkdown.yml' has a reference grouping.
#'
#' @param checks A 'pkgcheck' object.
#' @return A list of 'check_pass' and 'summary' values to be passed straight to
#' the output.
#'
#' @noRd
pkgchk_pkgdown <- function (checks) {

    # Skip pkgdown checks if number of .Rd files is below this threshold.
    ref_threshold <- 10L
    check_pass <- checks$info$pkgdown$num_rd_files < ref_threshold ||
        checks$info$pkgdown$has_reference
    summary <- ""

    if (!check_pass) {
        if (!fs::file_exists (checks$info$pkgdown$yaml_path)) {
            summary <- "Package has no '_pkgdown.yml' file"
        } else if (!checks$info$pkgdown$has_reference) {
            summary <- "'_pkgdown.yml' has no 'reference' grouping"
        }
    }

    list (check_pass = check_pass, summary = summary)
}

output_pkgchk_pkgdown <- function (checks) {

    out <- list (
        check_pass = checks$checks$pkgdown$check_pass,
        summary = checks$checks$pkgdown$summary,
        print = ""
    )

    return (out)
}
