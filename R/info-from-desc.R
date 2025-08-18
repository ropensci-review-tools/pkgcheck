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
