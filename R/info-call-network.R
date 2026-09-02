#' Plot function call network from \pkg{pkgstats} objects
#'
#' @param s Result of \pkg{pkgstats} call
#' @return Local path to 'd3js' HTML diagram of call network.
#' @noRd
fn_call_network <- function (s) {

    if (nrow (s$stats$network) == 0L && nrow (s$stats$objects) == 0L) {
        return (NULL)
    }

    d3js_dir <- fs::path (
        Sys.getenv ("PKGCHECK_CACHE_DIR"),
        "static"
    )
    if (!dir.exists (d3js_dir)) {
        dir.create (d3js_dir, recursive = TRUE)
    }

    d3js_file <- paste0 (
        s$out$name,
        "_pkgstats",
        substring (s$out$git$HEAD, 1, 8),
        ".html"
    )
    d3js_path <- fs::path (d3js_dir, d3js_file)

    # clean up any older ones
    flist <- list.files (
        d3js_dir,
        pattern = paste0 (s$out$package, "_pkgstats"),
        full.names = TRUE
    )

    if (!d3js_path %in% flist) {
        unlink (flist, recursive = TRUE)
        pkgstats::plot_network (s$stats, vis_save = d3js_path)
    }

    return (d3js_path)
}
