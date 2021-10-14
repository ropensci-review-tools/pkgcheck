
#' Tick symbol for markdown output
#' @noRd
symbol_tck <- function () {
    ":heavy_check_mark:"
}

#' Cross symbol for markdown output
#' @noRd
symbol_crs <- function () {
    ":heavy_multiplication_x:"
}

get_Rd_meta <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

#' Decompose file paths into character vectors of named directories and final
#' file names
#'
#' @param f One of more file paths with system-dependent file separators
#' @return List of equivalent character vectors from which paths can be
#' reconstructed with \link{file.path}
#' @noRd
decompose_path <- function (f) {

    # https://github.com/r-lib/fs/blob/4cc4b56c26b9d7f177a676fbb331133bb2584b86/R/path.R # nolint
    strsplit (f, "^(?=/)(?!//)|(?<!^)(?<!^/)/", perl = TRUE)
}

#' List all checks currently implemented
#'
#' @param quiet If `TRUE`, print all checks to screen. Function invisibly
#' returns list of checks regardless.
#' @return Character vector of names of all checks (invisibly)
#' @family extra
#' @export
list_pkgchecks <- function (quiet = FALSE) {
    chks <- grep (
        "^pkgchk\\_",
        ls (envir = asNamespace ("pkgcheck"), all.names = TRUE),
        value = TRUE
    )

    if (!quiet) {
        cli::cli_alert_info (paste0 (
            "The following checks are ",
            "currently implemented in pkgcheck:"
        ))
        cli::cli_ol (chks)
        cli::cli_end ()
    }

    invisible (chks)
}

#' Modified verion of getNamespaceExports` to exclude fns re-exported from other
#' packages
#' @noRd
exported_fns <- function (path) {
    nspace <- readLines (file.path (path, "NAMESPACE"))
    exports <- grep ("^export\\s?\\(", nspace, value = TRUE)
    exports <- gsub ("^export\\s?\\(|\\)$", "", exports)
    exports <- unlist (strsplit (exports, ",\\s?"))
    exports <- gsub ("\\\"", "", exports)

    # exclude any re-exports from other packages (typically like "%>%"):
    imports <- grep ("^importFrom\\s?\\(", nspace, value = TRUE)
    imports <- vapply (imports,
        function (i) {
            gsub ("\\)$", "", strsplit (i, ",") [[1]] [2])
        },
        character (1),
        USE.NAMES = FALSE
    )
    imports <- gsub ("\\\"", "", imports)

    return (exports [which (!exports %in% imports)])
}

#' Convert anything that is not an environment into one.
#'
#' Used in `collate_extra_env_checks` to convert package names into namespace
#' environments.
#' @noRd
env2namespace <- function (e) {
    if (!is.environment (e)) {
        s <- search ()
        e <- s [grep (paste0 (e, "$"), s)]

        if (length (e) != 1L) {
            e <- NULL
        } else {
            pkg <- gsub ("package\\:", "", e)
            e <- tryCatch (asNamespace (pkg),
                error = function (err) NULL
            )
        }
    }
    return (e)
}
