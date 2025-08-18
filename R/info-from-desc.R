pkginfo_url_from_desc <- function (path, type = "URL") {

    type <- match.arg (type, c ("URL", "BugReports"))

    desc <- fs::path (path, "DESCRIPTION")
    if (!file.exists (desc)) {
        return ("")
    }

    d <- data.frame (
        read.dcf (desc),
        stringsAsFactors = FALSE
    )
    if (!type %in% names (d)) {
        return ("")
    }

    u <- strsplit (d [[type]], "\\s+") [[1]]
    u <- grep ("^https", u, value = TRUE)
    if (length (u) > 1) {
        u <- grep ("git", u, value = TRUE)
    }
    if (length (u) > 1) {
        u <- u [which (!grepl ("\\.io", u))]
    }

    u <- gsub (",|\\s+", "", u)

    if (length (u) == 0L) {
        u <- ""
    }

    return (u [1])
}
