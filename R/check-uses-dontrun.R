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

  rd_files_use_dontrun(rd_files)
}

output_pkgchk_uses_dontrun <- function(checks) {

  out <- list(
    check_pass = all(checks$checks$uses_dontrun == "none"),
    summary = "",
    print = ""
  )

  # This is set up so that if all examples use dontrun, it is a failure (red X),
  # if no examples use dontrun, it is a pass (green check), and if some examples
  # use dontrun, it is a watch (:eyes:)
  if (!out$check_pass) {
    out$print <- "'.\nConsider using `@examplesIf()` to conditionally run examples instead."
    if (all(checks$checks$uses_dontrun == "all")) {
      out$summary <- "All examples use `\\dontrun{}`."
      out$print <- paste0(
      "All of your functions' examples use `\\dontrun{}`:\n'",
      out$print
      )
    } else {
      out$summary <- "Examples should not use `\\dontrun{}` unless really necessary."
      out$print <- paste0(
        "The following functions have examples that use `\\dontrun{}`:\n'",
        paste(
          names(checks$checks$uses_dontrun[checks$checks$uses_dontrun != "none"]),
          collapse = "', '"
        ),
        out$print
      )
      out$check_type <- "none_watch"
    }
  }

  return(out)
}

rd_files_use_dontrun <- function(rd_files) {
  parsed_rds <- parse_rd_files(rd_files)

  # Only check Rds that actually have examples
  has_egs <- fns_have_exs(rd_files)

  vapply(
    parsed_rds[names(has_egs[has_egs])],
    rd_has_dontrun_examples,
    FUN.VALUE = character(1L)
  )
}

## Utilities for parsing examples in Rd files
get_Rd_section <- utils::getFromNamespace(".Rd_get_section", "tools")

parse_rd_files <- function(rd_files) {
  rds <- lapply(
    rd_files,
    function(f) {
      tryCatch(
        tools::parse_Rd(
          f,
          warningCalls = FALSE,
          macros = FALSE,
          permissive = TRUE
        ),
        error = function(e) {
          warning(sprintf("Error parsing Rd file '%s': %s", file, e$message))
          NULL
        }
      )
    }
  )
  nms <- vapply(rds, get_Rd_meta, FUN.VALUE = character(1), "name")
  stats::setNames(rds, nms)
}

rd_has_dontrun_examples <- function(rd) {

  ex <- get_Rd_section(rd, "examples")
  tags <- vapply(ex, function(exi) attr(exi, "Rd_tag"), character(1L))

  # Code that is not wrapped in \dontrun has the attribute Rd_tag set to "RCODE"
  # and is listed line-by-line in the examples section (i.e., each line has the
  # Rd_tag "RCODE" attribute). Code that that is wrapped in \dontrun has the
  # attribute Rd_tag set to "\\dontrun" and is nested in a list element, and
  # within that list element the code lines have the Rd_tag attribute set to
  # "VERB" (i.e. 'verbatim'; not run).

  # Remove RCODE lines that are just whitespace or comments
  bare_r_code <- grep(
    "(^\\s+$)|(^#)", 
    unlist(ex[tags == "RCODE"]), 
    invert = TRUE, 
    value = TRUE
  )

  dontrun_r_code <- grep(
    "(^\\s+$)|(^#)", 
    unlist(ex[tags == "\\dontrun"]),
    invert = TRUE,
    value = TRUE
  )
  
  any_bare_r_code <- length(bare_r_code) > 0
  any_dont_run <- length(dontrun_r_code) > 0

  # We could report how many lines are wrapped in \dontrun, but for now we just
  # return a summary of whether there are any examples that use \dontrun.
  if (any_bare_r_code && any_dont_run) {
    return("some")
  } else if (any_dont_run && !any_bare_r_code) {
    return("all")
  } else {
    # Either no examples or no dontrun blocks
    return("none")
  }
}
