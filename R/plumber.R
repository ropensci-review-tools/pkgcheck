# plumber.R

#* Download a package from URL
#* @param u The URL for a repo
#* @post /report
function(u){
    pkg_name <- tail (strsplit (u, "/") [[1]], 1)

    get_exdir <- function () {
        exdir <- Sys.getenv ("pkgreport_dir")
        ifelse (exdir != "", exdir, tempdir ())
    }

    download_repo <- function (u, pkg_name) {
        if (substring (u, nchar (u), nchar (u)) == "/")
            u <- substring (u, 1, nchar (u) - 1)
        u <- paste0 (u, "/archive/master.zip")

        f <- file.path (get_exdir (), paste0 (pkg_name, "-master.zip"))
        download.file (u, destfile = f)
        return (f)
    }

    local_repo <- file.path (get_exdir (),
                             paste0 (pkg_name, "-master"))
    local_zip <- paste0 (local_repo, ".zip")
    if (!file.exists (local_zip))
        f <- download_repo (u, pkg_name)

    flist <- unzip (local_zip, exdir = get_exdir ())
    g <- packgraph::pg_graph (local_repo, plot = FALSE)
    res <- packgraph::pg_report (g)

    return (res)
}
