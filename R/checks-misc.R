
get_Rd_meta <- utils::getFromNamespace (".Rd_get_metadata", "tools") # nolint


pkg_on_cran <- function (path) {

    desc <- data.frame (read.dcf (file.path (path, "DESCRIPTION")))
    pkg <- desc$Package

    ap <- data.frame (utils::available.packages ())
    res <- pkg %in% ap$Package

    if (res) {
        # Check whether CRAN package of that name has same title

        u <- paste0 ("https://cran.r-project.org/web/packages/",
                     pkg, "/index.html")
        x <- rvest::read_html (u)
        h2 <- paste0 (rvest::html_elements (x, "h2"))
        h2 <- gsub ("<h2>|<\\/h2>", "", h2)
        res <- grepl (desc$Title, h2, ignore.case = TRUE)
    }

    return (res)
}
