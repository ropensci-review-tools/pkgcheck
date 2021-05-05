
#' Compare 'pkgstats' summary with statistics from all CRAN packages.
#' @param s Result of \link{pkgstats_summary}
#' @param threshold Proportion threshold below which to report on statistically
#' unusual properties.
#' @return A 'data.frame' of selected statistical properties and percentiles in
#' relation to all other packages on CRAN.
#' @noRd
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

    pc <- vapply (nms, function (i) {
               if (is.na (s [[i]]))
                   return (NA)
               return (length (which (dists [[i]] < s [[i]])) /
                   length (dists [[i]]))
                    }, double (1))
    index <- match (names (pc), names (s))
    pc <- data.frame (measure = names (pc),
                      value = as.numeric (s [1, index]),
                      percentile = pc,
                      row.names = NULL)

    keep <- c (grep ("^files\\_", pc$measure),
               grep ("^loc\\_", pc$measure),
               grep ("^data\\_", pc$measure),
               grep ("^f\\_fns\\_", pc$measure),
               grep ("^npars\\_", pc$measure),
               grep ("^loc\\_per\\_fn", pc$measure),
               grep ("^n\\_edges", pc$measure))
    pc <- pc [keep, ]
    pc <- pc [which (!is.na (pc$percentile)), ]
    rownames (pc) <- NULL
    if (!"files_src" %in% pc$measures)
        pc <- pc [which (!grepl ("\\_src$", pc$measure)), ]
    pc <- pc [which (!grepl ("^n\\_edges\\_", pc$measure)), ]

    # reduce to median estimates only
    pc <- pc [which (!grepl ("\\_mn$", pc$measure)), ]
    pc$measure <- gsub ("\\_md$", "", pc$measure)
    # rm data checks when no data
    if (pc$percentile [pc$measure == "data_size_total"] == 0.0) {

        pc <- pc [which (!grepl ("^data\\_", pc$measure)), ]
    }

    pc$noteworthy <- FALSE
    index <- which (pc$percentile < threshold |
                    pc$percentile > (1 - threshold))
    pc$noteworthy [index] <- TRUE

    # renames:
    pc$measure [pc$measure == "npars_exported"] <-
        "num_params_per_fn"
    pc$measure [pc$measure == "n_edges"] <-
        "fn_call_network_size"

    return (pc)
}
