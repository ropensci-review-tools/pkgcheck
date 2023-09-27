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
        file.path (checks$pkg$path, "man"),
        full.names = TRUE,
        recursive = FALSE,
        pattern = "\\.Rd$"
    )

    get1tag <- function (tags, rd, what = "docType") {
        index <- grep (paste0 (what, "$"), tags)
        ret <- ""
        if (length (index) > 0) {
            ret <- lapply (rd [index], function (j) {
                paste0 (unlist (j), collapse = "")
            })
            ret <- unlist (ret)
        }
        return (ret)
    }

    tag_data <- lapply (flist, function (f) {
        x <- tools::parse_Rd (f, permissive = TRUE)
        tags <- unlist (lapply (x, function (i) attr (i, "Rd_tag")))
        list (
            docType = get1tag (tags, x, "docType"),
            value = get1tag (tags, x, "value"),
            alias = get1tag (tags, x, "alias"),
            name = get1tag (tags, x, "name"),
            keyword = get1tag (tags, x, "keyword")
        )
    })

    doc_types <- vapply (tag_data, function (i) i$docType, character (1L))
    keywords <- vapply (tag_data, function (i) i$keyword, character (1L))
    value <- vapply (tag_data, function (i) i$value, character (1L))
    rd_names <- vapply (tag_data, function (i) i$name, character (1L))

    index <- which (
        !nzchar (doc_types) &
            !keywords %in% c ("datasets", "internal") &
            !nzchar (value)
    )

    return (rd_names [index])
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
