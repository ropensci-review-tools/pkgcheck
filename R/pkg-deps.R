
#' Format package dependencies as a single table
#'
#' @param checks Result of main \link{pkgcheck} function
#' @return A `data.frame` of internal and external dependency usage, primarily
#' derived from the "external_calls" data from \pkg{pkgstats}.
#' @noRd
pkgdeps_as_table <- function (checks) {

    deps <- checks$pkg$dependencies

    deps$package [deps$package == "NA"] <- NA_character_

    index <- which (deps$type == "depends" && is.na (deps$package))
    if (length (index) > 0L) {
        deps <- deps [-index, ]
    }

    re_exports <- get_re_exports (checks$pkg$path)
    if (nrow (re_exports) > 0L) {
        index <- which (
            deps$package %in% re_exports$package &
                is.na (deps$ncalls)
        )
        if (length (index) > 0L) {
            deps <- deps [-index, ]
        }
    }

    for (ty in unique (deps$type)) {
        index <- which (deps$type == ty)
        deps [index, ] <-
            deps [index, ] [order (deps$ncalls [index], decreasing = TRUE), ]
    }

    rownames (deps) <- NULL

    ext <- checks$pkg$external_calls
    index <- which (!names (ext) %in% deps$package)
    internal <- data.frame (
        type = "internal",
        package = names (ext [index]),
        ncalls = as.integer (ext [index])
    )

    deps <- rbind (internal, deps)

    return (deps)
}
