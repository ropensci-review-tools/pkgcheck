
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
    if (length (re_exports) > 0L) {
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

#' Get re-exported functions
#'
#' Note that import & export statements can span multiple lines, and
#' `importFrom` can accept multiple fns.
#'
#' @param path Local path to package source
#' @return A `data.frame` of [package, fn] detailing original source of all
#' re-exported functions.
#' @noRd
get_re_exports <- function (path) {

    nmsp <- readLines (file.path (path, "NAMESPACE"))

    op <- grep ("\\(", nmsp)
    cl <- grep ("\\)", nmsp)
    if (length (op) != length (cl)) {
        stop ("'NAMESPACE' file is incorrectly formatted")
    }
    index <- cbind (rev (op), rev (cl))
    if (any ((cl - op) > 1L)) {
        nmsp <- apply (index, 1, function (i) {
                paste0 (nmsp [seq (i [1], i [2])], collapse = "")
            })
    }

    imports <- grep ("^importFrom", nmsp, value = TRUE)
    import_fns <- gsub ("^importFrom\\(|\\)$", "", imports)
    import_pkgs <- vapply (import_fns, function (i) {
            strsplit (i, ",") [[1]] [1]
        },
    character (1),
    USE.NAMES = FALSE
    )
    import_pkgs <- gsub ("\"", "", import_pkgs)
    import_fns <- lapply (strsplit (import_fns, ","), function (i) i [-1])
    import_fns <- lapply (seq_along (import_pkgs), function (i) {
            cbind (
                rep (
                    import_pkgs [[i]],
                    length (import_fns [[i]])
                ),
                import_fns [[i]]
            )
        })
    import_fns <- do.call (rbind, import_fns)
    import_fns <- data.frame (
        package = import_fns [, 1],
        fn = gsub ("^\\s*|#.*$", "", import_fns [, 2])
    )

    exports <- grep ("^export\\(", nmsp, value = TRUE)
    exports <- gsub ("^export\\(|\\)", "", exports)
    exports <- unlist (strsplit (exports, ","))
    exports <- gsub ("^\\s*|\\t", "", exports)
    exports <- gsub ("#.*$", "", exports)

    re_exports <- exports [which (exports %in% import_fns$fn)]
    re_exports <- import_fns [match (re_exports, import_fns$fn), ]

    return (re_exports)
}
