
#' Generate report on package compliance with rOpenSci Statistical Software
#' requirements as background process
#'
#' @param path Path to local repository
#' @return A \pkg{processx} object connecting to the background process
#' generating the main \link{pkgcheck} results (see Note).
#'
#' @note The return object will by default display whether it is still running,
#' or whether it has finished. Once it has finished, the results can be obtained
#' by calling `$get_result()`, or the main \link{pkgcheck} function can be
#' called to quickly retrieve the main results from local cache.
#' @export
pkgcheck_bg <- function (path) {

    requireNamespace ("callr")

    logfiles <- logfile_names (path)
    stopfile <- gsub ("\\_stdout$", "_stop",
                      logfiles$stdout)
    if (file.exists (stopfile))
        file.remove (stopfile)

    Sys.setenv ("pkgcheck_pxbg_stop" = stopfile)

    callr::r_bg (func = pkgcheck::pkgcheck,
                 args = list (path = path),
                 stdout = logfiles$stdout,
                 stderr = logfiles$stderr,
                 package = TRUE)
}
