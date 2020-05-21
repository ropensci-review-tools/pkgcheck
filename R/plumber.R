# plumber.R

#* Download a package from URL
#* @param u The URL for a repo
#* @post /report
function(u){
    pkg_name <- tail (strsplit (u, "/") [[1]], 1)

    cache_dir <- Sys.getenv ("cache_dir")

    download_repo <- function (u, pkg_name) {
        if (substring (u, nchar (u), nchar (u)) == "/")
            u <- substring (u, 1, nchar (u) - 1)
        u <- paste0 (u, "/archive/master.zip")

        f <- file.path (cache_dir, paste0 (pkg_name, "-master.zip"))
        download.file (u, destfile = f)
        return (f)
    }

    local_repo <- file.path (cache_dir, paste0 (pkg_name, "-master"))
    local_zip <- paste0 (local_repo, ".zip")
    if (!file.exists (local_zip))
        f <- download_repo (u, pkg_name)

    flist <- unzip (local_zip, exdir = cache_dir)
    g <- packgraph::pg_graph (local_repo, plot = FALSE)
    res <- packgraph::pg_report (g)

    return (res)
}
