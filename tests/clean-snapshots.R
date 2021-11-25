# Functions to clean files used for snapshot tests

# some checks like rcmdcheck differ on different systems for things like
# compilation flags, so the snapshot test excludes any rmcdcheck output. It
# also reverts the final package versions to a generic number.
edit_markdown <- function (md) {

    change_pkg_vers <- function (md, pkg = "pkgstats", to = "42") {
        i <- grep ("Package Versions", md)
        pkg_i <- grep (pkg, md)
        pkg_i <- pkg_i [pkg_i > i] [1]
        md [pkg_i] <- gsub ("([0-9]\\.)+[0-9]+", to, md [pkg_i])
        # white space also changes with version numbers:
        md [pkg_i] <- gsub (
            paste0 (to, "\\s+"),
            paste0 (to, "    "), md [pkg_i]
        )
        return (md)
    }
    md <- change_pkg_vers (md, "pkgstats")
    md <- change_pkg_vers (md, "pkgcheck")
    md <- change_pkg_vers (md, "srr")

    return (md)
}

# issue#111
# html output includes `.js` src files and other stuff which can have machine
# and OS-specific components, plus the titles automatically append the path
# including value of `tempdir`. This clean all of that out, leaving just the
# bits representing the report details.
#
# Note that it presumed that `edit_markdown` has already been called to revert
# package versions to generic values prior to rendering html version of that
# report.
#
# @param f Name of html file in current tempdir
edit_html <- function (f) {

    h <- readLines (f)

    # title includes path, so reset to generic value:
    h <- readLines (f)
    i <- grep ("^<title>", h) [1]
    h [i] <- "<title>pkgcheck.knit</title>"

    # rm bundled scripts, which can easily be identified via nchar
    h <- h [which (nchar (h) < 1000)]
    # plus one script and one css data ref
    ptn <- "^<(script\\ssrc|meta|link\\shref=\"data\\:text\\/css)"
    i <- grep (ptn, h)
    if (length (i) > 0) {
        h <- h [-i]
    }

    writeLines (h, con = f)
}
