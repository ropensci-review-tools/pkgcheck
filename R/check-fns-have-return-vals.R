
#' Check that all functions document their return values.
#'
#' The reflects a CRAN checks for all new submissions, to ensure that return
#' values are documented for all functions. This check applies even to functions
#' which are called for their side effects and return `NULL`.
#'
#' @note This is currently the only function which uses
#' `roxygen2::parse_file()`. If any further functions use this, it might be
#' better to move this to an "info" item and out of this specific "check" item.
#'
#' @noRd
pkgchk_fns_have_return_vals <- function (checks) {

    flist <- list.files (
        file.path (checks$pkg$path, "R"),
        full.names = TRUE,
        recursive = TRUE,
        pattern = "\\.(R|r)$"
    )

    suppressWarnings ({
        blocks <- lapply (flist, function (i) {
            tryCatch (roxygen2::parse_file (i, env = NULL),
                error = function (e) list ()
            )
        })
    })
    names (blocks) <- flist
    blocks <- do.call (c, blocks)

    fn_index <- which (vapply (
        blocks,
        function (i) {
            !is.null (i$call) &&
                length (roxygen2::block_get_tags (i, "export")) > 0L
        },
        logical (1L),
        USE.NAMES = FALSE
    ))
    blocks <- blocks [fn_index]


    no_ret_tags <- which (vapply (
        blocks,
        function (i) {
            length (roxygen2::block_get_tags (i, "return")) == 0L
        },
        logical (1L),
        USE.NAMES = FALSE
    ))
    if (length (no_ret_tags) == 0L) {
        return (NULL)
    }

    blocks <- blocks [no_ret_tags]

    fn_names <- vapply (blocks, function (i) {
        pd <- utils::getParseData (parse (
            text = deparse (i$call),
            keep.source = TRUE,
            encoding = "UTF-8"
        ))
        if (!any (pd$token == "SYMBOL")) {
            return ("")
        } else {
            pd$text [which (pd$token == "SYMBOL") [1]]
        }},
    character (1L),
    USE.NAMES = FALSE
    )

    return (fn_names)
}

output_pkgchk_fns_have_return_vals <- function (checks) { # nolint

    out <- list (
        check_pass = length (checks$checks$fns_have_return_vals) == 0L,
        summary = "",
        print = ""
    )

    if (!out$check_pass) {

        fns <- checks$checks$fns_have_return_vals

        nfns <- length (fns)

        txt <- ifelse (nfns == 1L, "function has", "functions have")

        out$summary <- paste0 (
            "The following ",
            txt,
            " no documented return value",
            ifelse (nfns == 1L, "", "s"),
            ": [",
            paste0 (fns, collapse = ", "),
            "]"
        )
    }

    return (out)
}
