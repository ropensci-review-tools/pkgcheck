
#' Check whether all functions have examples
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Vector of named logical values, one for each '.Rd' file indicating
#' whether or not it has example lines.
#' @noRd
pkgchk_fns_have_exs <- function (checks) {

    rd <- list.files (file.path (
        checks$pkg$path, "man"
    ),
    pattern = "\\.Rd$",
    full.names = TRUE
    )

    # don't check for examples in datasets (#103)
    has_ex <- vapply (rd, function (i) {
        rd_i <- tools::parse_Rd (i)
        ex <- get_Rd_meta (rd_i, "examples")
        kw <- get_Rd_meta (rd_i, "keyword")
        if (length (kw) == 0L) {
            kw <- ""
        }
        (length (ex) > 0 | kw == "datasets")
    },
    logical (1),
    USE.NAMES = TRUE
    )

    names (has_ex) <-
        vapply (
            names (has_ex), function (i) {
                utils::tail (decompose_path (i) [[1]], 1L)
            },
            character (1)
        )

    has_ex <- has_ex [which (!grepl ("\\-package\\.Rd$", names (has_ex)))]

    return (has_ex)
}

output_pkgchk_fns_have_exs <- function (checks) {

    no_ex <- which (!checks$checks$fns_have_exs)
    no_ex_fns <- names (checks$checks$fns_have_exs) [no_ex]

    out <- list (
        check_pass = length (no_ex) == 0L,
        summary = "",
        print = ""
    ) # no print method

    out$summary <- ifelse (out$check_pass,
        "All functions have examples.",
        paste0 (
            "These functions do not have ",
            "examples: [",
            paste0 (no_ex_fns, collapse = ", "),
            "]."
        )
    )

    return (out)
}
