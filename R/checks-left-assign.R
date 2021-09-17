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
pkgchk_left_assign <- function (path) {

    rdir <- file.path (path, "R")
    if (!file.exists (rdir))
        return (c (":=" = 0L,
                   "<-" = 0L,
                   "<<-" = 0L,
                   "=" = 0L))

    rdir <- normalizePath (rdir)
    flist <- list.files (rdir,
                         full.names = TRUE,
                         pattern = "\\.q$|\\.r$|\\.s$",
                         ignore.case = TRUE)

    assigns <- vapply (flist, function (i) {
                           p <- tryCatch (utils::getParseData (parse (i)),
                                          error = function (e) NULL)
                           assigns <- c (":=" = 0L,
                                         "<-" = 0L,
                                         "<<-" = 0L,
                                         "=" = 0L)
                           if (is.null (p))
                               return (assigns)
                           la <- table (p$text [which (p$token ==
                                                       "LEFT_ASSIGN")])
                           if (":=" %in% names (la))
                               assigns [1] <- la [which (names (la) == ":=")]
                           if ("<-" %in% names (la))
                               assigns [2] <- la [which (names (la) == "<-")]
                           if ("<<-" %in% names (la))
                               assigns [3] <- la [which (names (la) == "<<-")]
                           if ("=" %in% names (la))
                               assigns [4] <- la [which (names (la) == "=")]

                           return (assigns)
                         },
                         integer (4),
                         USE.NAMES = TRUE)
    assigns <- rowSums (assigns)
    # rm `:=`:
    assigns <- assigns [which (!names (assigns) == ":=")]

    out <- list (global = assigns [["<<-"]] > 0)
    assigns <- assigns [names (assigns) != "<<-"]
    out$usage <- assigns

    return (out)
}

#' @return cross only
#' @noRd
summarise_left_assign_chk <- function (checks) {

    res <- NULL

    if (checks$left_assign$global) {

        res <- paste0 ("- ", symbol_crs (),
                       " Package uses global assignment operator ('<<-').")
    }

    if (length (which (checks$left_assign$usage == 0)) == 0) {

        la <- checks$left_assign$usage

        res <- c (res,
                  paste0 ("- ", symbol_crs (),
                          " Package uses inconsistent ",
                          "assignment operators (",
                          la [names (la) == "<-"], " '<-' and ",
                          la [names (la) == "="], " '=')."))
    }

    return (res)
}