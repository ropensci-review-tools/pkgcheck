#' Check whether all function names are unique.
#'
#' Uses the database of function names from all CRAN packages associated with
#' [releases of the `pkgstats`
#' package](https://github.com/ropensci-review-tools/pkgstats/releases).
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Matrix of (package, fn_name) identifying any packages with duplicated
#' function names.
#' @noRd
pkgchk_unique_fn_names <- function (checks) {

    fn_names_cran <- read_fn_names_data ()

    index <- which (!fn_names_cran$package == checks$pkg$name)
    fn_names_cran <- fn_names_cran [index, ]

    fn_names <- checks$info$fn_names
    fn_names <- fn_names [which (fn_names$fn_name %in% fn_names_cran$fn_name), ]

    fn_names_cran [which (fn_names_cran$fn_name %in% fn_names$fn_name), ]
}

#' Check whether a function name exists in any CRAN packages
#'
#' @param fn_name Character vector of one or more function names to check.
#' @param force_update If 'TRUE', locally-cached data of all function names
#' from all CRAN packages will be updated to latest version.
#'
#' @return A `data.frame` of three columns, "package", "version", and
#' "fn_name", identifying any other packages matching specified function
#' name(s). If no matches are found, the `data.frame` will have no rows.
#'
#' @examples
#' fn_names_on_cran (c ("min", "max"))
#'
#' @family extra
#' @export
fn_names_on_cran <- function (fn_name, force_update = FALSE) {

    fn_names_cran <- read_fn_names_data (force_update = force_update)

    fn_name <- fn_name [which (fn_name %in% fn_names_cran$fn_name)]

    res <- fn_names_cran [which (fn_names_cran$fn_name %in% fn_name), ]
    rownames (res) <- NULL

    return (res)
}

read_fn_names_data <- function (force_update = FALSE) {

    # The cache_path is set to tempdir in tests. This tests is then switched off
    # to avoid downloading the database just for this test.
    cache_path <- Sys.getenv ("PKGCHECK_CACHE_DIR")
    cache_is_temp <- identical (
        normalizePath (dirname (cache_path)),
        normalizePath (tempdir ())
    )

    # Default return object:
    fn_names_cran <- data.frame (
        package = character (0),
        version = character (0),
        fn_name = character (0)
    )

    f <- NULL
    if (!cache_is_temp) {
        f <- tryCatch (
            cache_fn_name_db (force_update = force_update),
            error = function (e) NULL
        )
    }

    if (!is.null (f)) {
        fn_names_cran <- tryCatch (
            readRDS (f),
            error = function (e) NULL
        )
    }

    return (fn_names_cran)
}

cache_fn_name_db <- function (force_update = FALSE) {

    cache_path <- Sys.getenv ("PKGCHECK_CACHE_DIR")
    if (!fs::dir_exists (cache_path)) {
        fs::dir_create (cache_path)
    }

    f <- fs::path (cache_path, "pkgstats-fn-names.Rds")

    if (fs::file_exists (f) && !force_update) {
        return (f)
    }

    requireNamespace ("jsonlite", quietly = TRUE)

    u <- paste0 (
        "https://api.github.com/repos/",
        "ropensci-review-tools/pkgstats/",
        "releases/latest"
    )

    res <- curl::curl_fetch_memory (u)
    hdrs <- curl::parse_headers (res$headers)
    http_code <- as.integer (gsub (
        "^http\\/[0-9]\\s?|\\s+$",
        "",
        hdrs [1],
        ignore.case = TRUE
    ))
    if (http_code != 200L) {
        cli::cli_abort (
            "Call to GitHub failed with http error code [{http_code}]"
        )
    }

    res <- jsonlite::fromJSON (rawToChar (res$content))
    assets <- res$assets
    i <- grep ("fn\\-names", assets$name)
    dl_url <- assets$browser_download_url [i]
    f <- fs::path (cache_path, basename (dl_url))

    curl::curl_download (url = dl_url, destfile = f, quiet = TRUE)

    return (f)
}

output_pkgchk_unique_fn_names <- function (checks) { # nolint

    out <- list (
        check_pass = nrow (checks$checks$unique_fn_names) == 0L,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {
        out$summary <- "Function names are duplicated in other packages"

        obj <- checks$checks$unique_fn_names
        obj <- obj [order (obj$fn_name), ]

        nfns <- length (unique (obj$fn_name))

        txt <- ifelse (nfns == 1L,
            "function name is ",
            paste0 (nfns, " function names are ")
        )

        obj <- lapply (
            split (obj, f = as.factor (obj$fn_name)),
            function (i) {
                paste0 (
                    " - `",
                    i$fn_name [1],
                    "` from ",
                    paste0 (i$package, collapse = ", ")
                )
            }
        )

        out$print <- list (
            msg_pre = paste0 (
                "The following ",
                txt,
                "duplicated in other packages:"
            ),
            obj = obj,
            msg_post = ""
        )
    }

    out$check_type <- "none_watch" # (pass_fail)

    return (out)
}
