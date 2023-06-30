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
#'
#' @note This function does not accept the `extra_env` parameter of the main
#' \link{pkgcheck} function, and can not be used to run extra, locally-defined
#' checks.
#' @family pkgcheck_fns
#' @export
#' @examples
#' \dontrun{
#' # Foreground checks as "blocking" process which will return
#' # only after all checks have finished:
#' checks <- pkgcheck ("/path/to/my/package")
#'
#' # Or run process in background, do other things in the meantime,
#' # and obtain checks once they have finished:
#' ps <- pkgcheck_bg ("/path/to/my/package")
#' ps # print status to screen, same as 'ps$print()'
#' # To examine process state while running:
#' f <- ps$get_output_file ()
#' readLines (f) # or directly open file with local file viewer
#' # ... ultimately wait until 'running' changes to 'finished', then:
#' checks <- ps$get_result ()
#' }
pkgcheck_bg <- function (path) {

    requireNamespace ("callr")

    logfiles <- logfile_names (path)
    stopfile <- gsub (
        "\\_stdout$", "_stop",
        logfiles$stdout
    )
    if (file.exists (stopfile)) {
        file.remove (stopfile)
    }

    e <- c (callr::rcmd_safe_env (),
        "PKGCHECK_PXBG_STOP" = stopfile,
        "PKGCHECK_BG" = TRUE,
        "PKGCHECK_CACHE_DIR" = Sys.getenv ("PKGCHECK_CACHE_DIR")
    )

    Sys.setenv ("PKGCHECK_PXBG_STOP" = stopfile)

    callr::r_bg (
        func = pkgcheck::pkgcheck,
        args = list (path = path),
        stdout = logfiles$stdout,
        stderr = logfiles$stderr,
        env = e,
        package = TRUE
    )
}
