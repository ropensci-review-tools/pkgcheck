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

    # The cache_path is set to tempdir in tests. This tests is then switched off
    # to avoid downloading the database just for this test.
    cache_path <- Sys.getenv ("PKGCHECK_CACHE_DIR")
    cache_is_temp <- identical (
        normalizePath (dirname (cache_path)),
        normalizePath (tempdir ())
    )

    f <- fn_names_cran <- NULL
    if (!cache_is_temp) {
        f <- tryCatch (
            cache_fn_name_db (),
            error = function (e) NULL
        )
    }

    if (!is.null (f)) {
        fn_names_cran <- tryCatch (
            readRDS (f),
            error = function (e) NULL
        )
    }

    # fail to read local data:
    if (is.null (fn_names_cran)) {
        return (data.frame (
            package = character (0),
            version = character (0),
            fn_name = character (0)
        ))
    }

    index <- which (!fn_names_cran$package == checks$pkg$name)
    fn_names_cran <- fn_names_cran [index, ]

    fn_names <- checks$info$fn_names
    fn_names <- fn_names [which (fn_names$fn_name %in% fn_names_cran$fn_name), ]

    fn_names_cran [which (fn_names_cran$fn_name %in% fn_names$fn_name), ]
}

cache_fn_name_db <- function () {

    cache_path <- Sys.getenv ("PKGCHECK_CACHE_DIR")

    f <- file.path (cache_path, "pkgstats-fn-names.Rds")
    if (file.exists (f)) {
        return (f)
    }

    u <- paste0 (
        "https://github.com/ropensci-review-tools/pkgstats/",
        "releases/download/v0.1.2/pkgstats-fn-names.Rds"
    )
    utils::download.file (u, f, quiet = TRUE)

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
