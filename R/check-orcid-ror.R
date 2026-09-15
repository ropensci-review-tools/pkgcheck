#' Check if authors have ORCID IDs
#'
#' @param checks A 'pkgcheck' object with full \pkg{pkgstats} summary and
#' \pkg{goodpractice} results.
#' @return A named vector of binary flags indicating whether or not authors
#' have ORCID entries.
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
    # Only examine roles of "cre", "aut", "ctb":
    cres <- which (auts_are_role (authors, "cre"))
    auts <- which (auts_are_role (authors, "aut"))
    ctbs <- which (auts_are_role (authors, "ctb"))
    authors <- authors [unique (c (cres, auts, ctbs))]

    has_orcid <- vapply (
        authors,
        function (x) "ORCID" %in% names (x$comment),
        logical (1L)
    )

    # TODO check if the ORCID is valid. If not, suggest fixing it.
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
            obj = names (checks$checks$has_orcid) [!checks$checks$has_orcid],
            msg_post = character (0)
        )
    }

    return (out)
}

#' Check whether any instutitions listed as `role = "fnd"` have RORs
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
    funders <- authors [which (auts_are_role (authors, "fnd"))]

    has_ror <- vapply (
        funders,
        function (x) {
            "ROR" %in% names (x$comment)
        },
        logical (1L)
    )

    names (has_ror) <- vapply (
        funders,
        function (x) x$given,
        character (1L)
    )

    return (has_ror)
}

output_pkgchk_has_ror <- function (checks) {
    out <- list (
        check_pass = all (checks$checks$has_ror), # safe because all(list()) returns TRUE
        summary = "", # silent if passing, since most packages won't have institutions as authors
        print = "",
        check_type = "none_watch"
    )

    if (!out$check_pass) {
        out$summary <- "Institutions listed as authors without RORs"
        out$print <- list (
            msg_pre = paste0 (
                "The following institutions are missing RORs:"
            ),
            obj = names (checks$checks$has_ror) [!checks$checks$has_ror],
            msg_post = character (0)
        )
    }

    return (out)
}

# Helper function to match a vector of authors to a defined role
auts_are_role <- function (authors, role = "fnd") {
    vapply (
        authors,
        function (a) any (a$role %in% role),
        logical (1L)
    )
}
