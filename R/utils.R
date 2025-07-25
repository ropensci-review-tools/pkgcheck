#' Tick symbol for markdown output
#'
#' @noRd
symbol_tck <- function () {

    ":heavy_check_mark:"
}

#' Cross symbol for markdown output
#'
#' @noRd
symbol_crs <- function () {

    ":heavy_multiplication_x:"
}

get_Rd_meta <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint

#' List all checks currently implemented
#'
#' @param quiet If `TRUE`, print all checks to screen. Function invisibly
#' returns list of checks regardless.
#' @return Character vector of names of all checks (invisibly)
#'
#' @examples
#' list_pkgchecks ()
#'
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

#' List all .Rd files, but exclude any that are in `.Rbuildignore`.
#'
#' See issue # 277 for roxygen2 'devtag'
#' @noRd
list_rd_files <- function (path) {

    rd <- list.files (
        fs::path (path, "man"),
        pattern = "\\.Rd$",
        full.names = TRUE
    )
    rb <- fs::path (path, ".Rbuildignore")
    if (fs::file_exists (rb)) {
        rb <- readLines (rb)
        rb <- paste0 (grep ("^(\\^?)man", rb, value = TRUE), collapse = "|")
        if (nzchar (rb)) {
            rd_rel <- fs::path_rel (rd, path)
            rd <- rd [which (!grepl (rb, rd_rel))]
        }
    }
    return (rd)
}

#' Modified verion of getNamespaceExports` to exclude fns re-exported from other
#' packages
#'
#' @noRd
exported_fns <- function (path) {

    nspace <- readLines (fs::path (path, "NAMESPACE"))
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
#'
#' @noRd
env2namespace <- function (e) {

    if (!is.environment (e)) {

        s <- search ()
        if (any (grepl (paste0 (e, "$"), s))) {
            e <- s [grep (paste0 (e, "$"), s)] [1] # hard-code to 1st value
            e <- gsub ("package\\:", "", e)
        }

        e <- tryCatch (
            asNamespace (e),
            error = function (err) NULL
        )
    }

    return (e)
}

#' Try to get available.packages()
#'
#' That function fails when no CRAN mirror is set, which is generally the case
#' on GitHub runners, even if set as "repos" option. In those cases, this
#' returns `NULL`.
#'
#' @noRd
get_available_packages <- function () {
    op <- options ()
    if (is.null (getOption ("repos"))) {
        # Needed for GitHub runners, because avail.pkgs fails with no mirror set
        options (repos = c (CRAN = "https://cloud.r-project.org"))
    }
    ap <- tryCatch (
        data.frame (utils::available.packages (), stringsAsFactors = FALSE),
        error = function (e) NULL
    )
    options (op)

    return (ap)
}

is_test_env <- function () {

    is_gha <- identical (Sys.getenv ("GITHUB_ACTIONS", ""), "true")
    test_dir <- identical (
        Sys.getenv ("PKGCHECK_CACHE_DIR", getwd ()),
        file.path (tempdir (), "pkgcheck")
    )

    return (is_gha && test_dir)
}
