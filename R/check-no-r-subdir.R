#' Check that R directory is flat, with no sub-directories.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return TRUE if `renv` is used; otherwise FALSE
#' @noRd
pkgchk_no_r_subdir <- function (checks) {

    r_dir <- fs::path (checks$pkg$path, "R")
    if (!fs::dir_exists (r_dir)) {
        return (FALSE)
    }
    r_dir_subdirs <- fs::dir_ls (r_dir, type = "directory")
    return (length (r_dir_subdirs) == 0L)
}

output_pkgchk_no_r_subdir <- function (checks) {

    out <- list (
        check_pass = checks$checks$no_r_subdir,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- "The R directory includes sub-directories"
    }

    return (out)
}
