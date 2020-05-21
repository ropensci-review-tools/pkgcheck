#' serve plumber API to report on packages
#'
#' The API exposes the two POST points of
#' \itemize{
#' \item report Download the given URL to `tempdir()`
#' \item pgstats Analyse the package held at location returned from `dlurl` and
#' return a formatted report
#' }
#' @param port Port for API to be served on
#' @param cache_dir Directory where previously downloaded repositories are
#' cached
#' @param bg If `FALSE`, run process as a blocking foreground process
#' @param debug For background processes: If `TRUE`, dump output logs to
#' `/tmp/out` and `/tmp/err`
#' @return A `processx` process which must be actively stopped with `ps$kill()`.
#' @export
serve_api <- function (port = 8000L, cache_dir = tempdir (), bg = TRUE, debug = FALSE) {
    r <- plumber::plumb (file.path (here::here (), "R", "plumber.R"))

    e <- callr::rcmd_safe_env ()
    e <- c (e, cache_dir = cache_dir)
    if (!file.exists (cache_dir))
        dir.create (cache_dir, recursive = TRUE)

    ps <- NULL
    if (bg) {
        f <- function (r, port = 8000L) r$run (port = port)

        sout <- serr <- "|"
        if (debug) {
            sout <- "/tmp/out"
            serr <- "/tmp/err"
        }

        ps <- callr::r_bg (f, list (r = r, port = as.integer (port)),
                           env = e, stdout = sout, stderr = serr)
    } else
        r$run (port = as.integer (port))

    return (ps)
}


#' pr_report
#'
#' Query plumber API (set up with \link{serve_api}) for a report on a repository
#' @param u URL of package repository
#' @param port Port at which plumber API has been served via \link{serve_api}.
#' @return Report on repository
#' @export
pr_report <- function (u, port = 8000L) {
    sprintf ("http://localhost:%s/report?u=%s", as.integer (port), u) %>%
        httr::POST () %>%
        httr::content () %>%
        unlist ()
}
