pkgchk_has_orcid <- function (checks) {
  desc <- data.frame (
    read.dcf (fs::path (
        checks$pkg$path,
        "DESCRIPTION"
    )),
    stringsAsFactors = FALSE
)
  authors <- eval (str2lang (desc$Authors.R))
  #TODO only apply this to authors that aren't institutions
  has_orcid <- sapply(authors, function(x) !is.null(x$comment["ORCID"]))

  names(has_orcid) <- sapply(authors, function(x) paste(x$given, x$family))

  return(has_orcid)

}

is_institution <- function (person) {
  is.null(person$family) & person$role %in% c ( "cph", "fnd")
}


output_pkgchk_has_orcid <- function (checks) {
  out <- list (
    check_pass = all (checks$checks$has_orcid),
    summary = "All authors have ORCIDs",
    print = ""
  )
  
  if (!out$check_pass) {
    out$summary <- "Not all authors have ORCIDs"
    out$print <- list (
      msg_pre = paste0 (
        "The following authors are missing ORCIDs:"
      ),
      obj = names (checks$checks$has_orcid)[!checks$checks$has_orcid],
      msg_post = character (0)
    )
  }
  
  return (out)
}
