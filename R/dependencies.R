
#' Install all system and package dependencies of an R package
#'
#' @param local_repo Path to local package
#' @inheritParams serve_api
#' @export
pkgrep_install_deps <- function (local_repo, os, os_release) {

    sysreq <- remotes::system_requirements (os = os,os_release = os_release,path = local_repo)
    tmp <- lapply (sysreq, system)

    remotes::install_deps (pkgdir = local_repo,
                           dependencies = TRUE,
                           upgrade = "always")
}
