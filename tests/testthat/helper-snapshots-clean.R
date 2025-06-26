# Functions to clean files used for snapshot tests

# some checks like rcmdcheck differ on different systems for things like
# compilation flags, so the snapshot test excludes any rmcdcheck output. It
# also reverts the final package versions to a generic number.
#
# print_method edits the output of `print.pkgcheck`.
edit_markdown <- function (md, print_method = FALSE) {

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

    i <- grep ("Click to see.*static", md)
    md [i] <- edit_temp_path_date_time (md [i])

    if (print_method) {

        # change path to visjs html file when generated locally:
        i <- grep ("network diagram", md)
        md [i] <- gsub (
            "network\\sdiagram\\sis\\sat\\s.*$",
            "network diagram is not here.",
            md [i]
        )

        return (md)
    }

    # The headers of those tables also change between machines and/or pandoc
    # versions, some stretching `-`s to fit text, some using fixed with. This
    # ensure regularity
    i <- grep ("Package Versions", md)
    vers_i <- grep ("version\\s+\\|$", md)
    vers_i <- vers_i [which (vers_i > i)] [1]
    md [vers_i] <- gsub ("version\\s+\\|$", "version |", md [vers_i])
    hbar_i <- vers_i + 1 # always!
    md [hbar_i] <- gsub ("\\:\\-+\\|$", ":------|", md [hbar_i])

    # change path to visjs html file when generated locally:
    i <- grep ("interactive network visualisation", md)
    md [i] <- gsub ("\\]\\(.*$", "](network.html)", md [i])
    # or the equivalent version generated on GitHub actions:
    i <- grep ("An interactive visualisation", md)
    md [i] <- gsub ("\\]\\(.*\\)", "](network.html)", md [i])

    # remove <details> sections of function usage because numbers of function
    # calls to dependency packages can be arbitrarily ordered when numbers of
    # calls are equal.
    i <- grep ("^<details>", md)
    j <- grep ("</details>", md)
    # These are the sub-sections:
    index <- which (j [-length (i)] > i [-1])
    if (length (index) > 0L) {
        index <- seq (min (i [index + 1]), max (j [index]))
        md <- md [-index]
    }

    # and for some reason, the covr environment pulls in an extra external
    # package which must be removed to align snapshots:
    i <- grep ("\\|mgcv", md)
    if (length (i) > 0L) {
        md <- md [-i]
    }

    return (md)
}

# issue#111
# html output is not generally reproducible, because all sorts of scripts get
# inserted on different systems. This reduces the entire html file to the data
# within the primary `<div>` containers only.
#
# Note that it presumed that `edit_markdown` has already been called to revert
# package versions to generic values prior to rendering html version of that
# report.
#
# @param f Name of html file in current tempdir
edit_html <- function (f) {

    h <- readLines (f)

    # title includes path, so reset to generic value:
    i <- grep ("^<title>", h) [1]
    h [i] <- "<title>pkgcheck.knit</title>"

    # reduce down to only elements within the main `div` containers:
    i <- grep ("^<div.*>$", h)
    j <- grep ("^<\\/div>$", h)
    len <- min (c (length (i), length (j)))
    ij <- cbind (i [seq (len)], j [seq (len)])
    index <- apply (ij, 1, function (i) i [1]:i [2])
    index <- sort (unique (unlist (index)))

    h <- h [index]

    i <- grep ("Click to see.*static", h)
    h [i] <- edit_temp_path_date_time (h [i])

    # some machines/systems split `<summary>` items across multiple lines,
    # others concatenate, so concanate all regardless:
    i <- grep ("^<summary.*>$", h)
    j <- grep ("^<\\/summary>$", h)
    len <- min (c (length (i), length (j)))
    ij <- cbind (i [seq (len)], j [seq (len)])
    # rm any which are on single line:
    ij <- ij [which (ij [, 2] > ij [, 1]), ]
    index <- apply (ij, 1, function (i) i [1]:i [2])
    if (!is.list (index)) {
        index <- lapply (seq (ncol (index)), function (i) index [, i])
    }
    index <- rev (index)
    for (i in index) {
        h [i [1]] <- paste0 (h [i], collapse = "")
        h <- h [-(i [-1])]
    }

    # some machines/systems generate a `colgroup` group specifying column
    # widths, while others do not (likely pandoc v1 - v2 difference), so remove
    # that:
    i <- grep ("^<colgroup>$", h)
    if (length (i) > 0L) {
        j <- grep ("^<\\/colgroup>$", h)
        ij <- apply (
            cbind (i, j),
            1,
            function (k) seq (k [1], k [2]),
            simplify = FALSE
        )
        ij <- unlist (ij)
        h <- h [seq_along (h) [-ij]]
    }

    writeLines (h, con = f)
}

edit_temp_path_date_time <- function (txt_line) {

    # Standardise date-time inserted by srr for network diagram link:
    ptn_ymd <- "[0-9]{4}\\-[0-9]{2}\\-[0-9]{2}"
    ptn_hms <- "[0-9]{2}\\:[0-9]{2}\\:[0-9]+\\.[0-9]+"
    ptn <- paste0 ("srr", ptn_ymd, "\\-", ptn_hms)
    txt_line <- gsub (ptn, "srr2000-01-01", txt_line)

    # And network diagram path includes tempdir, so standardised that:
    ptn <- paste0 (
        "tmp", .Platform$file.sep, "Rtempdir", .Platform$file.sep, "pkgcheck"
    )
    gsub ("tmp.*pkgcheck", ptn, txt_line)
}
