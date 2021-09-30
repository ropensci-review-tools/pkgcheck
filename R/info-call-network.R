
#' Plot function call network from \pkg{pkgstats} objects
#'
#' @param s Result of \pkg{pkgstats} call
#' @return Local path to 'visjs' HTML diagram of call network.
#' @noRd
fn_call_network <- function (s) {
    if (nrow (s$stats$network) == 0L && nrow (s$stats$objects) == 0L) {
          return (NULL)
      }

    visjs_dir <- file.path (
        getOption ("pkgcheck.cache_dir"),
        "static"
    )
    if (!dir.exists (visjs_dir)) {
          dir.create (visjs_dir, recursive = TRUE)
      }

    visjs_file <- paste0 (
        s$out$package,
        "_pkgstats",
        substring (s$out$git$HEAD, 1, 8),
        ".html"
    )
    visjs_path <- file.path (visjs_dir, visjs_file)

    # clean up any older ones
    flist <- list.files (
        visjs_dir,
        pattern = paste0 (s$out$package, "_pkgstats"),
        full.names = TRUE
    )

    if (!visjs_path %in% flist) {
        unlink (flist, recursive = TRUE)
        pkgstats::plot_network (s$stats, vis_save = visjs_path)
        # visNetwork renames the generic `lib` folder to the specific name, so
        # needs to be cleaned up:
        flist <- list.files (
            visjs_dir,
            pattern = paste0 (s$out$package, "_pkgstats"),
            full.names = TRUE
        )
        libdir <- flist [which (dir.exists (flist))]
        if (!"lib" %in% list.files (visjs_dir)) {
            if (length (libdir) > 0) {
                libdir <- libdir [1]
                fpath <- file.path (libdir, "..")
                newlibdir <- file.path (normalizePath (fpath), "lib")
                file.rename (libdir, newlibdir)
            }
        } else {
            unlink (libdir)
        }
    }

    return (visjs_path)
}
