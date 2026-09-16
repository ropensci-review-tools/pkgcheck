#' Get list of all 'concepts' used to group man entries.
#'
#' @param path Location of local repository to report on
#'
#' @noRd
pkginfo_pkgdown <- function (path) {

    rd_files <- list_rd_files (path) # utils.R

    out <- list (
        num_rd_files = length (rd_files),
        yaml_path = NA_character_,
        has_reference = FALSE
    )

    pkgdown_yml_path <- fs::dir_ls (
        path,
        type = "file",
        regexp = "\\_pkgdown\\."
    )
    if (length (pkgdown_yml_path) != 1L) {
        return (out)
    }

    if (fs::file_exists (pkgdown_yml_path)) {
        out$yaml_path <- pkgdown_yml_path
        # Avoid extra 'yaml' dependency and just read as text:
        pkgdown_yml <- readLines (pkgdown_yml_path)
        out$has_reference <- any (grepl ("^reference\\:", pkgdown_yml))
    }

    return (out)
}
