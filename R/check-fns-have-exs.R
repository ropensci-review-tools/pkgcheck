#' Check whether all functions have examples.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Vector of named logical values, one for each '.Rd' file indicating
#' whether or not it has example lines.
#' @noRd
pkgchk_fns_have_exs <- function (checks) {

    files <- list_rd_files (checks$pkg$path) # utils.R

    fns_have_exs(rd)
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

fns_have_exs <- function(rd_files) {
  # don't check for examples in datasets (#103), identified by keyword
    what <- c ("name", "docType", "keyword", "examples")
    rd_dat <- vapply (rd_files, function (i) {
        rd_i <- tools::parse_Rd (i, permissive = TRUE)
        dat <- lapply (what, function (j) {
            get_Rd_meta (rd_i, j)
        })
        if (length (dat [[2]]) == 0L) {
            dat [[2]] <- ""
        }
        if (length (dat [[3]]) == 0L) {
            dat [[3]] <- ""
        }
        dat [[4]] <- length (dat [[4]])
        unlist (dat) [1:4]
    },
    character (4),
    USE.NAMES = TRUE
    )
    rd_dat <- data.frame (
        t (rd_dat),
        row.names = NULL,
        stringsAsFactors = FALSE
    )
    names (rd_dat) <- what

    # rm internal and datasets, where all re-exported fns should be internal.
    rd_dat <- rd_dat [which (!rd_dat$keyword %in% c ("internal", "datasets")), ]
    rd_dat <- rd_dat [which (!rd_dat$docType %in% c ("package", "data")), ]

    has_ex <- rd_dat$examples > 0L
    names (has_ex) <- rd_dat$name

    return (has_ex)
}
