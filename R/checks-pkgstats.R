
pkgchk_url_from_desc <- function (path) {

    desc <- file.path (path, "DESCRIPTION")
    if (!file.exists (desc))
        return (NULL)

    d <- data.frame (read.dcf (desc))
    if (!"URL" %in% names (d))
        return (NULL)

    u <- strsplit (d$URL, "\\s+") [[1]]
    u <- grep ("^https", u, value = TRUE)
    if (length (u) > 1)
        u <- grep ("git", u, value = TRUE)
    if (length (u) > 1)
        u <- u [which (!grepl ("\\.io", u))]

    u <- gsub (",|\\s+", "", u)

    return (u [1])
}

#' @param s Result of `pkgstats(path)`
#' @noRd
pkgchk_pkg_name <- function (s) {
    s$desc$package
}

pkgchk_pkg_version <- function (s) {
    s$desc$version
}

pkgchk_pkg_license <- function (s) {
    s$desc$license
}
