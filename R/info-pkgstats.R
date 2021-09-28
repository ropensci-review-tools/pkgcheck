# These functions provide information derived from \pkg{pkgstats} without
# actually being checks

pkgcheck_url_from_desc <- function (path) {

    desc <- file.path (path, "DESCRIPTION")
    if (!file.exists (desc))
        return (NULL)

    d <- data.frame (read.dcf (desc))
    if (!"URL" %in% names (d))
        return (NULL)

    u <- strsplit (d$URL, "\\s+") [[1]]
    u <- grep ("^https", u, value = TRUE)
    if (length (u) > 1)
        u <- grep ("git", u, value = TRUE)
    if (length (u) > 1)
        u <- u [which (!grepl ("\\.io", u))]

    u <- gsub (",|\\s+", "", u)

    return (u [1])
}

#' @param s Result of `pkgstats(path)`
#' @noRd
pkgcheck_pkg_name <- function (s) {
    s$desc$package
}

pkgcheck_pkg_version <- function (s) {
    s$desc$version
}

pkgcheck_pkg_license <- function (s) {
    s$desc$license
}

pkgchk_pkgstats_summary <- function (s) {

    pkgstats <- fmt_pkgstats_checks (s)

    num_exported_fns <- pkgstats$value [pkgstats$measure == "n_fns_r_exported"]
    num_non_exported_fns <- pkgstats$value [pkgstats$measure ==
                                            "n_fns_r_not_exported"]
    num_src_fns <- sum (pkgstats$value [pkgstats$measure %in%
                        c ("n_fns_src", "n_fns_inst")])
    loc_exported_fns <- pkgstats$value [pkgstats$measure ==
                                        "loc_per_fn_r_exp"]
    loc_non_exported_fns <- pkgstats$value [pkgstats$measure ==
                                            "loc_per_fn_r_not_exp"]
    loc_src_fns <- stats::median (pkgstats$value [pkgstats$measure %in%
                                  c ("loc_per_fn_src", "loc_per_fn_inst")])
    num_params_per_fn <- pkgstats$value [pkgstats$measure ==
                                         "num_params_per_fn"]

    list (
         num_authors = s$desc$aut,
         num_vignettes = unname (s$vignettes [1]),
         num_data = unname (s$data_stats [1]),
         imported_pkgs = length (strsplit (s$desc$imports, ",") [[1]]),
         num_exported_fns = as.integer (num_exported_fns),
         num_non_exported_fns = as.integer (num_non_exported_fns),
         num_src_fns = as.integer (num_src_fns),
         loc_exported_fns = as.integer (loc_exported_fns),
         loc_non_exported_fns = as.integer (loc_non_exported_fns),
         loc_src_fns = as.integer (loc_src_fns),
         num_params_per_fn = as.integer (num_params_per_fn),
         languages = attr (pkgstats, "language")
         )
}
