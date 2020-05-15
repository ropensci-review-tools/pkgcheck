#' serve plumber API to report on packages
#'
#' The API exposes the two POST points of
#' \itemize{
#' \item dlurl Download the given URL to `tempdir()`
#' \item pgstats Analyse the package held at location returned from `dlurl` and
#' return a formatted report
#' }
#' @param port Port for API to be served on
#' @return A `processx` process which must be actively stopped with `ps$kill()`.
#' @export
serve_api <- function (port = 8000) {
    r <- plumber::plumb (file.path (here::here (), "R", "plumber.R"))
    f <- function (r, port = 8000) r$run (port = port)
    ps <- callr::r_bg (f, list (r = r, port = 8000))
    return (ps)
}


#' pr_dl
#'
#' Download a package from a specified URL, storing in tempdir()
#' @param u URL of package repository
#' @param port Port at which plumber API has been served via \link{serve_api}.
#' @return Location of download, to be passed to \link{pg_report}
#' @export
pr_dl <- function (u, port = 8000) {
    x <- sprintf ("http://localhost:%s/dlurl?u=%s", port, u) %>%
        httr::POST () %>%
        httr::content ()
    return (x [[1]]) # location of download
}

#' pr_report
#'
#' Query plumber API (set up with \link{serve_api}) for a report on a repository
#' downloaded locally with the \link{pr_dl} function.
#' @param dl Location of downloaded repository as returned from \link{pr_dl}
#' @param port Port used to set up API with \link{serve_api}
#' @export
pr_report <- function (dl, port = 8000) {
    x <- sprintf ("http://localhost:%s/pgstats?f=%s", port, dl) %>%
        httr::POST () %>%
        httr::content ()
    return (unlist (x))
}
