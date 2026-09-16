order_checks_sequence <- c (
    "pkgname",
    "license",
    "has_citation",
    "has_contrib",
    "fns_have_return_vals",
    "uses_roxygen2",
    "has_url",
    "has_bugs",
    "has_vignette",
    "fns_have_exs",
    "global_assign",
    "no_r_subdir",
    "repo_not_fork",
    "repo_has_website",
    "pkgdown",
    "ci",
    "covr",
    "lintr",
    "has_scrap",
    "left_assign",
    "renv_activated",
    "branch_is_master",
    "srr_okay",
    "srr_missing",
    "srr_todo",
    "srr_most_in_one_file",
    "srr_general_only",
    # These are "watch" checks, not outright fails; they must be
    # additionally explicitly listed below in `watch_checks()`:
    "obsolete_pkg_deps",
    "unique_fn_names",
    "uses_dontrun",
    "num_imports",
    "has_orcid",
    "has_ror"
)

#' Function to specify the order in which checks appear in the summary method.
#'
#' @param fns List of output functions with prefixes `output_pkgchk_`, for which
#' order is to be established.
#' @return Modified version of input list with functions ordered in specified
#' sequence.
#' @noRd
order_checks <- function (fns) {

    fns <- fns [which (fns %in% order_checks_sequence)]
    # This line needed b/c 'covr' is removed w/o gp:
    ord <- order_checks_sequence [which (order_checks_sequence %in% fns)]
    fns <- fns [match (ord, fns)]

    return (fns)
}
