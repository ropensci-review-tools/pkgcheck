#' Parse items of the "desc" part of `pkgstats` output
#'
#' @param s Result of `pkgstats::pkgstats()` call.
#' @noRd
parse_pkg_deps <- function (s) {

    fields <- c ("depends", "imports", "suggests", "linking_to")

    d <- lapply (fields, function (i) {
        out <- cbind (
            i,
            strsplit (s$desc [[i]], ",\\s*") [[1]]
        )
        if (length (out) == 1) {
            out <- c (out, "NA")
        }
        return (out)
    })

    d <- do.call (rbind, d)

    out <- data.frame (
        type = d [, 1],
        package = d [, 2],
        ncalls = NA_integer_,
        stringsAsFactors = FALSE
    )

    # Then tally number of calls from 'external_calls' data
    ex_tab <- table (s$external_calls$package)
    index <- which (out$package %in% names (ex_tab))
    out$ncalls [index] <- ex_tab [match (out$package [index], names (ex_tab))]

    return (out)
}

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

    u <- strsplit (d [[type]], "[,[:space:]]+") [[1]]
    u <- grep ("^https?\\:\\/\\/", u, value = TRUE)
    if (length (u) > 1 && any (grepl ("git", u))) {
        u <- grep ("git", u, value = TRUE)
    }
    if (length (u) > 1 && !all (grepl ("\\.io", u))) {
        u <- u [which (!grepl ("\\.io", u))]
    }
    u <- u [1]
    if (is.na (u)) {
        return ("")
    }
    if (type == "URL" && grepl ("github\\.io", u, ignore.case = TRUE)) {
        u <- rm_issues_from_url (pkginfo_url_from_desc (path, type = "BugReports"))
    }

    u <- gsub (",|\\s+", "", u)

    if (length (u) == 0L) {
        u <- ""
    }

    return (u [1])
}

rm_issues_from_url <- function (u) {
    gsub ("(\\/\\-)?\\/issues(\\/?)$", "", u)
}
