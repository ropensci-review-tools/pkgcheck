#' Check if authors have ORCID IDs
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Names of any items which should not be present; otherwise an empty
#' character.
#' @noRd
pkgchk_has_orcid <- function (checks) {
  desc <- data.frame (
    read.dcf (fs::path (
      checks$pkg$path,
      "DESCRIPTION"
    )),
    stringsAsFactors = FALSE
  )
  authors <- eval (str2lang (desc$Authors.R))
  authors <- authors [!is_institution (authors)]

  has_orcid <- vapply (
    authors,
    function (x) {
      !is.null (x$comment ["ORCID"])
    },
    FUN.VALUE = TRUE
  )

  #TODO check if the ORCID is valid. If not, suggest fixing it.
  # check if the ORCID matches the author name. If not, suggest updaing their ORCID profile

  names (has_orcid) <- vapply (
    authors,
    function (x) {
      paste (x$given, x$family)
    },
    FUN.VALUE = "a"
  )

  return (has_orcid)
}

output_pkgchk_has_orcid <- function (checks) {
  out <- list (
    check_pass = all (checks$checks$has_orcid),
    summary = "All authors have ORCID IDs",
    print = ""
  )
  
  if (!out$check_pass) {
    out$summary <- "Not all authors have ORCID IDs"
    out$print <- list (
      msg_pre = paste0 (
        "The following authors are missing ORCID IDs:"
      ),
      obj = names (checks$checks$has_orcid)[!checks$checks$has_orcid],
      msg_post = character (0)
    )
  }
  
  return (out)
}

#' Check if instutitions (if there are any) have RORs
#' 
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return Names of any items which should not be present; otherwise an empty
#' character.
#' @noRd
pkgchk_has_ror <- function (checks) {

  desc <- data.frame (
    read.dcf (fs::path (
        checks$pkg$path,
        "DESCRIPTION"
    )),
    stringsAsFactors = FALSE
)
  authors <- eval (str2lang (desc$Authors.R))
  institutions <- authors[is_institution (authors)]
  
  has_ror <- vapply (
    institutions,
    function (x) {
      !is.null (x$comment["ROR"])
    },
    FUN.VALUE = TRUE
  )

  #TODO check if ROR is valid and matches institution name

  names (has_ror) <- vapply (
    institutions,
    function(x) x$given,
    FUN.VALUE = "a"
  )
  
  return(has_ror)
}

output_pkgchk_has_ror <- function (checks) {
  out <- list (
    check_pass = all (checks$checks$has_ror), #safe because all(list()) returns TRUE
    summary = "", #silent if passing, since most packages won't have institutions as authors
    print = ""
  )
  
  if (!out$check_pass) {
    out$summary <- "Institutions listed as authors without RORs"
    out$print <- list (
      msg_pre = paste0 (
        "The following institutions are missing RORs:"
      ),
      obj = names (checks$checks$has_ror)[!checks$checks$has_ror],
      msg_post = character (0)
    )
  }
  
  return (out)
}

# Helper function to determine if an author is an institution
is_institution <- function (person) {
  is.null (person$family) & person$role %in% c ( "cph", "fnd")
}