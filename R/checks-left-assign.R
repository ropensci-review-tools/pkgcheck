# left_assign checks only appear as cross in summary, and have neither a print
# method, nor do they appear in markdown formatted reports.

#' Check that left-assignment operators are used consistently throughout a
#' package. "LEFT_ASSIGN" tokens can also be `:=`, so these must also be
#' tallied, but are ignored.
#' Left-assign operators are: ("=", "<-", "<<-", ":=").
#' https://github.com/wch/r-source/blob/trunk/src/main/gram.y#L3283-L3290
#' https://github.com/wch/r-source/blob/trunk/src/main/gram.y#L3346-L3349
#' @inheritParams pkg_uses_roxygen2
#' @return Named vector of 2 values tallying instances of usage of `<-` and `=`.
#' @noRd
pkgchk_left_assign <- function (checks) {

    rdir <- fs::path (checks$pkg$path, "R")
    if (!file.exists (rdir)) {
        return (list (
            global = FALSE,
            usage = c (
                "<-" = 0L,
                "=" = 0L
            )
        ))
    }

    rdir <- normalizePath (rdir)
    flist <- list.files (rdir,
        full.names = TRUE,
        pattern = "\\.q$|\\.r$|\\.s$",
        ignore.case = TRUE
    )

    assigns <- vapply (flist, function (i) {

        p <- tryCatch (utils::getParseData (parse (i)),
            error = function (e) NULL
        )

        assigns <- c (
            ":=" = 0L,
            "<-" = 0L,
            "<<-" = 0L,
            "=" = 0L
        )

        if (is.null (p)) {
            return (assigns)
        }

        la <- table (p$text [which (p$token == "LEFT_ASSIGN")])

        if (":=" %in% names (la)) {
            assigns [1] <- la [which (names (la) == ":=")]
        }
        if ("<-" %in% names (la)) {
            assigns [2] <- la [which (names (la) == "<-")]
        }
        if ("<<-" %in% names (la)) {
            assigns [3] <- la [which (names (la) == "<<-")]
        }
        if ("=" %in% names (la)) {
            assigns [4] <- la [which (names (la) == "=")]
        }

        return (assigns)
    },
    integer (4),
    USE.NAMES = TRUE
    )

    assigns <- rm_global_assign_in_ref_class (assigns, checks)

    assigns <- rowSums (assigns)
    # rm `:=`:
    assigns <- assigns [which (!names (assigns) == ":=")]

    out <- list (global = assigns [["<<-"]] > 0)
    assigns <- assigns [names (assigns) != "<<-"]
    out$usage <- assigns

    return (out)
}

# Allow global assign in RefClass statement (#145)
rm_global_assign_in_ref_class <- function (assigns, checks) {

    global_row <- which (rownames (assigns) == "<<-")
    global <- assigns [global_row, ]
    global <- global [global > 0L]
    if (length (global) == 0L) {
        return (assigns)
    }

    global <- data.frame (
        file = gsub (checks$pkg$path, "", names (global)),
        n = as.integer (global)
    )
    global$file <- gsub (paste0 ("^", .Platform$file.sep), "", global$file)

    loc_stats <- utils::getFromNamespace ("loc_stats", "pkgstats")
    get_ctags <- utils::getFromNamespace ("get_ctags", "pkgstats")

    loc <- loc_stats (checks$pkg$path)
    has_tabs <- any (loc$ntabs > 0L)
    tags <- withr::with_dir (checks$pkg$path, get_ctags ("R", has_tabs))
    tags <- tags [which (tags$file %in% global$file &
        grepl ("RefClass", tags$content)), ]

    if (nrow (tags) == 0L) {
        return (assigns)
    }

    for (i in seq (nrow (tags))) {

        f <- file.path (checks$pkg$path, tags$file [i])
        if (!file.exists (f)) {
            next # should never happen
        }

        code <- suppressWarnings (readLines (f))
        code <- code [seq (tags$start [i], tags$end [i])]
        code <- tryCatch (
            utils::getParseData (parse (text = code)),
            error = function (e) NULL
        )

        la <- table (code$text [which (code$token == "LEFT_ASSIGN")])
        nglobal <- 0
        if ("<<-" %in% names (la)) {
            nglobal <- la [which (names (la) == "<<-")]
        }

        if (nglobal > 0L) {
            col_num <- match (f, colnames (assigns))
            assigns [global_row, col_num] <- assigns [global_row, col_num] - nglobal
        }
    }

    return (assigns)
}

output_pkgchk_global_assign <- function (checks) {

    out <- list (
        check_pass = !checks$checks$left_assign$global,
        summary = "",
        print = ""
    ) # no print method

    if (!out$check_pass) {
        out$summary <- " Package uses global assignment operator ('<<-')."
    }

    return (out)
}

output_pkgchk_left_assign <- function (checks) {

    la <- checks$checks$left_assign$usage # tally of [`<-`, `=`]

    out <- list (
        check_pass = any (la == 0),
        summary = "",
        print = ""
    ) # no print method

    if (!out$check_pass) {
        out$summary <- paste0 (
            "Package uses inconsistent assignment operators (",
            la [names (la) == "<-"], " '<-' and ",
            la [names (la) == "="], " '=')."
        )
    }

    return (out)
}
