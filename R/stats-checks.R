
#' Compare 'pkgstats' summary with statistics from all CRAN packages.
#' @param s Result of \link{pkgstats_summary}
#' @param threshold Proportion threshold below which to report on statistically
#' unusual properties.
#' @export
stats_checks <- function (s, threshold = 0.05) {

    dat <- pkgreport::pkgstats_data

    # temporary fixes until data are re-generated:
    dat$loc_R [dat$files_R == 0] <- NA_integer_
    dat$loc_src [dat$files_src == 0] <- NA_integer_
    dat$loc_inst [dat$files_inst == 0] <- NA_integer_
    dat$loc_vignettes [dat$files_vignettes == 0] <- NA_integer_
    dat$loc_tests [dat$files_test == 0] <- NA_integer_

    nms <- names (dat)
    index <- which (vapply (nms, function (i)
                            is.numeric (dat [[i]]),
                            logical (1)))
    nms <- nms [index]
    ptn <- "^desc_n|^num\\_|^files\\_inst|^files\\_src"
    nms <- nms [which (!grepl (ptn, nms))]
    dists <- lapply (nms, function (i)
                     sort (dat [[i]] [which (!is.na (dat [[i]]))]))
    names (dists) <- nms

    nms_should_be_low <- grep ("comment\\_lines", nms, value = TRUE)
    nms_should_be_high <- grep ("files_|n\\_fns\\_", nms, value = TRUE)

    pc <- vapply (nms, function (i) {
               if (is.na (s [[i]]))
                   return (c (NA, NA))
               imin <- length (which (dists [[i]] <= s [[i]])) /
                   length (dists [[i]])
               imax <- length (which (dists [[i]] >= s [[i]])) /
                   length (dists [[i]])
               return (c (imin, imax))
                    }, double (2))
    pc <- data.frame (t (pc))
    names (pc) <- c ("below", "above")

    pc$flagged <- FALSE
    pc$flagged [which (rownames (pc) %in% nms &
                       (pc$below < threshold | pc$above < threshold))] <- TRUE
    pc$flagged [which (rownames (pc) %in% nms_should_be_low &
                       pc$above < threshold)] <- TRUE
    pc$flagged [which (rownames (pc) %in% nms_should_be_high &
                       pc$below < threshold)] <- TRUE

    pc <- pc [which (pc$flagged), ]

    return (pc)
}
