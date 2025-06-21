#' Check if examples use `\dontrun{}`
#'
#' This check identifies functions in the package documentation that have
#' examples using `\dontrun{}`. The use of `\dontrun{}` is discouraged by
#' CRAN and should only be used the example really cannot be executed
#' (e.g. because of missing additional software, missing API keys, ...) by
#' the user. Instead use methods to run the examples conditionally, such as
#' with the `@examplesIf()` roxygen tag.
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return character vector of function names that have examples using `\dontrun{}`.
#' @noRd
pkgchk_uses_dontrun <- function(checks) {
  rd_files <- list.files(
    fs::path(
      checks$pkg$path,
      "man"
    ),
    pattern = "\\.Rd$",
    full.names = TRUE
  )

  parsed_rds <- lapply(
    rd_files,
    tools::parse_Rd,
    warningCalls = FALSE,
    macros = FALSE,
    permissive = TRUE
  )

  has_dontrun_examples <- vapply(
    parsed_rds,
    \(rd) {
      # get_Rd_meta doesn't get the \dontrun part of the examples section,
      # so need to look for it manually.
      # Using as.character() + grepl() may be a bit imprecise, however parsing
      # the Rd directly is messy, and I think this is sufficient
      rd_char <- as.character(rd)
      any(grepl("\\examples", rd_char, fixed = TRUE)) &&
        any(grepl("\\dontrun", rd_char, fixed = TRUE))
    },
    logical(1)
  )

  fn_names <- if (any(has_dontrun_examples)) {
    vapply(
      parsed_rds[has_dontrun_examples],
      \(rd) {
        fn_name <- get_Rd_meta(rd, "name")
        ret <- if (length(fn_name) == 0) "" else fn_name
        ret
      },
      character(1)
    )
  } else {
    character(0)
  }

  fn_names
}

output_pkgchk_uses_dontrun <- function(checks) {
  out <- list(
    check_pass = length(checks$checks$uses_dontrun) == 0,
    summary = "",
    print = ""
  )

  if (!out$check_pass) {
    out$summary <- "Examples should not use `\\dontrun{{}}` unless really necessary."
    out$print <- paste0(
      "The following functions have examples that use `\\dontrun{{}}`:\n'",
      paste(checks$checks$uses_dontrun, collapse = "', '"),
      "'.\nConsider using `@examplesIf()` to conditionally run examples instead."
    )
  }

  return(out)
}
