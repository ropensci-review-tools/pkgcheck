# plumber.R

#* Download a package from URL
#* @param u The URL for a repo
#* @post /dlurl
function(u){
    u <- "https://github.com/ropensci/osmdata/"
    pkg_name <- tail (strsplit (u, "/") [[1]], 1)
    if (substring (u, nchar (u), nchar (u)) == "/")
        u <- substring (u, 1, nchar (u) - 1)
    u <- paste0 (u, "/archive/master.zip")

    f <- file.path (tempdir (), "master.zip")
    if (!file.exists (f))
        download.file (u, destfile = f)

    flist <- unzip (f, exdir = tempdir ())

    return (file.path (tempdir (), paste0 (pkg_name, "-master")))
}

#* Get packgraph stats for a downloaded pkg
#* @param filepath Path to downloaded package on server
#* @post /pgstats
function(f){
    if (!file.exists (f))
        return (FALSE)

    g <- packgraph::pg_graph (f, plot = FALSE)
    res <- packgraph::pg_report (g)

    return (res)
}
