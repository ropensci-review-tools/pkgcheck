#' Count number of example lines per function
#'
#' @param path Path to local source repository
#' @return A `data.frame` of function names and counts of numbers of lines of
#' example code.
#' @noRd
num_ex_lines <- function (path) {

    if (!uses_roxygen (path))
        return ("package does not use roxygen")

    exports <- exported_fns (path)

    # Then names and aliases of all documented functions:
    flist <- list.files (file.path (path, "R"),
                         pattern = ".[R|r]$",
                         full.names = TRUE)
    flist <- flist [which (!grepl ("RcppExports.R$", flist))]
    blocks <- lapply (flist, function (i) roxygen2::parse_file (i))
    names (blocks) <- flist
    blocks <- do.call (c, blocks)

    doc_fns <- lapply (blocks, function (i) {
                        ret <- c (NA_character_, NA_character_)
                        this_class <- class (i$object$value)
                        if ("function" %in% this_class &
                            !"s3method" %in% this_class) {
                            fn <- roxygen2::block_get_tag_value (i, "name")
                            fn <- unique (c (fn, i$object$alias))
                            exs <- roxygen2::block_get_tag_value (i, "examples")
                            exs <- utils::capture.output (print (exs),
                                                          type = "output")
                            ret <- cbind (fn, rep (length (exs), length (fn)))
                        }
                        return (ret)
    })
    doc_fns <- do.call (rbind, doc_fns)
    doc_fns <- data.frame (doc_fns [which (!is.na (doc_fns [, 1])), ])
    rownames (doc_fns) <- NULL
    names (doc_fns) <- c ("fn", "n")
    # non-dplyr way to group_by and summarise; much slower but that's okay here
    doc_fns <- lapply (split (doc_fns, f = as.factor (doc_fns$fn)),
                        function (i) c (i$fn [1], max (i$n)))
    doc_fns <- do.call (rbind, doc_fns)
    doc_fns <- data.frame (fn = doc_fns [, 1],
                           n = as.integer (doc_fns [, 2]),
                           row.names = NULL)

    # Add any undocumented functions to final result
    index <- which (!exports %in% doc_fns$fn)
    if (length (index) > 0) {
        doc_fns <- rbind (doc_fns,
                          data.frame (fn = exports [index],
                                      n = 0L))
    }

    return (doc_fns)
}
exported_fns <- function (path) {

    nspace <- readLines (file.path (path, "NAMESPACE"))
    exports <- grep ("^export\\s?\\(", nspace, value = TRUE)
    exports <- gsub ("^export\\s?\\(|\\)$", "", exports)
    exports <- unlist (strsplit (exports, ",\\s?"))
    exports <- gsub ("\\\"", "", exports)

    # exclude any re-exports from other packages (typically like "%>%"):
    imports <- grep ("^importFrom\\s?\\(", nspace, value = TRUE)
    imports <- vapply (imports, function (i)
                       gsub ("\\)$", "", strsplit (i, ",") [[1]] [2]),
                       character (1),
                       USE.NAMES = FALSE)
    imports <- gsub ("\\\"", "", imports)

    return (exports [which (!exports %in% imports)])
}
