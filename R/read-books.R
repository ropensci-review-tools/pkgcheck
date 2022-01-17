#' Browse packaging guidelines
#'
#' @param which Whether to read the released or "dev" development version.
#'
#' @export
read_pkg_guide <- function (which = c ("release", "dev")) {
    which <- match.arg (which [1],
        c ("release", "dev"),
        several.ok = FALSE
    )

    utils::browseURL (sprintf ("%sbuilding.html", devguide_url (which)))
}


devguide_url <- function (which) {
    switch (which,
        release = "https://devguide.ropensci.org/",
        dev = "https://devdevguide.netlify.app/"
    )
}
