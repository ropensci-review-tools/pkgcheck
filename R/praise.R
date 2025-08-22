# Directly inspired by https://github.com/r-lib/testthat/blob/main/R/praise.R

praise_from_bot <- function (okay = TRUE) {

    ifelse (
        okay,
        "This package may be submitted.",
        "This package is not ready to be submitted."
    )
}

pkgcheck_praise <- function (okay = TRUE) {
    praise <- c (
        "Your package is in great shape!",
        "Rockin package, great work!",
        "Nothing wrong here!",
        "Sheesh, impressive package!",
        "Wowsers, that's some fine code!",
        "Yup, those checks are fine!",
        "A truly lovely set of check results!",
        praise::praise ("Your package is ${adjective}!"),
        praise::praise ("${EXCLAMATION} - ${adjective} code.")
    )
    encourage <- c (
        "Hmmm, not quite there yet ...",
        "Sorry about that, but your package is not quite ready ...",
        "Darn it, still problems lurking there ...",
        "Keep trying, you'll get there ...",
        "Frustration is a natural part of programming :)",
        "Argh, checks are a bit less than fully okay ...",
        "Sorry about those failing checks ...",
        "Nope, not quite there yet ..."
    )

    ifelse (
        okay,
        sample (praise, 1L),
        sample (encourage, 1L)
    )
}
