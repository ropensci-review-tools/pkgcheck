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

  parsed_rds <- parse_rd_files(rd_files)

  # Only check Rds that actually have examples
  has_egs <- pkgchk_fns_have_exs(checks)

  has_dontrun <- vapply(
    parsed_rds[names(has_egs[has_egs])], 
    has_dontrun_examples,
    FUN.VALUE = logical(1L)
  )

  has_dontrun
}

output_pkgchk_uses_dontrun <- function(checks) {
  out <- list(
    check_pass = all(!checks$checks$uses_dontrun),
    summary = "",
    print = ""
  )

  if (!out$check_pass) {
    out$summary <- if (all(checks$checks$uses_dontrun)) {
      # When #248 is addressed, this condition should be :heavy_multiplication_x:
      "All examples use `\\dontrun{}`." 
    } else {
      # :eyes: when some examples use `\dontrun{}`
      "Examples should not use `\\dontrun{{}}` unless really necessary."
    }
    out$print <- paste0(
      "The following functions have examples that use `\\dontrun{{}}`:\n'",
      paste(
        names(checks$checks$uses_dontrun[checks$checks$uses_dontrun]),
        collapse = "', '"
      ),
      "'.\nConsider using `@examplesIf()` to conditionally run examples instead."
    )
  }

  return(out)
}


## Utilities for parsing examples in Rd files
get_Rd_section <- utils::getFromNamespace (".Rd_get_section", "tools")

parse_rd_files <- function(rd_files) {
  rds <- lapply(
    rd_files,
    function(f) {
      tryCatch(
        tools::parse_Rd(f, warningCalls = FALSE, macros = FALSE, permissive = TRUE),
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

has_dontrun_examples <- function(rd) {
  ex <- get_Rd_section(rd, "examples")  
  tags <- vapply(ex, function(exi) attr(exi, "Rd_tag"), character(1L))  
  
  # Check if there are any \dontrun blocks
  any(tags == "\\dontrun")
}
