# Extracts lists of CRAN-acceptable licenses (plus one additional CDDL
# license from the OSI), and writes to R/license-list.R to use in license
# checks.
# This should never have to be re-run. See revision history of the file at
# https://github.com/ropensci-review-tools/pkgcheck/issues/73#issuecomment-972846817

u <- "https://svn.r-project.org/R/trunk/share/licenses/license.db"
f <- file.path (tempdir (), "license.db")
download.file (u, f)

x <- readLines (f)

index <- rep (0, length (x))
index [which (nchar (x) == 0L)] <- 1
index <- cumsum (index)
x <- lapply (
    split (x, f = factor (index)),
    function (i) i [which (nchar (i) > 0L)]
)
licenses <- vapply (x, function (i) {
    nm <- gsub (
        "^Name:\\s?", "",
        grep ("^Name\\:", i, value = TRUE)
    )
    ab <- gsub (
        "^Abbrev:\\s?", "",
        grep ("^Abbrev\\:", i, value = TRUE)
    )
    if (length (ab) == 0L) {
        ab <- NA_character_
    }
    c (name = nm, abbrev = ab)
},
character (2),
USE.NAMES = FALSE
)
licenses <- data.frame (t (licenses))
names (licenses) <- c ("name", "abbrev")
index <- which (is.na (licenses$abbrev))
licenses$abbrev [index] <- licenses$name [index]
licenses <- unique (licenses$abbrev)
licenses <- paste0 ("        \"", licenses, "\",")

f <- c (
    "# do not edit by hand; created by:",
    "# source ('./data-raw/license-list.r')",
    "#",
    "# rOpenSci recommendations:",
    "# https://devguide.ropensci.org/pkg_building.html#licence",
    "# links to cran list used to fill this license-list at",
    "# https://svn.r-project.org/R/trunk/share/licenses/license.db",
    "# plus OSI list which includes the extra CDDL-1.0 license.",
    "",
    "license_list <- function () {",
    "    c (",
    licenses,
    "        \"CDDL-1.0\"",
    "    )",
    "}"
)

here <- rprojroot::find_root (rprojroot::is_r_package)
writeLines (f, file.path (here, "R", "license-list.R"))
