
#' Compare 'pkgstats' summary with statistics from all CRAN packages.
#' @param s Result of \link{pkgstats_summary}
#' @param threshold Proportion threshold below which to report on statistically
#' unusual properties.
#' @return A 'data.frame' of selected statistical properties and percentiles in
#' relation to all other packages on CRAN.
#' @noRd
stats_checks <- function (s, threshold = 0.05) {

    # npars is set to NA when there are none; replace with 0:
    if (is.na (s$npars_exported_mn))
        s$npars_exported_mn <- 0L
    if (is.na (s$npars_exported_md))
        s$npars_exported_md <- 0L


    dat <- pkgcheck::pkgstats_data

    # convert blank line measures into relative
    b_s <- grep ("^blank\\_lines", names (s))
    b_d <- grep ("^blank\\_lines", names (dat))
    c_s <- grep ("^loc\\_", names (s)) # also includes loc_per_fn stats
    c_d <- grep ("^loc\\_", names (dat))
    rel_white_pkg <- rel_white_all <- list ()
    for (i in seq_along (b_s)) {
        nm <- gsub ("blank\\_lines\\_", "", names (s) [b_s [i]]) # directory name
        rel_white_pkg [[nm]] <- as.numeric (unname (s [[b_s [i]]] / s [[c_s [i]]]))
        tmp <- as.numeric (unname (dat [[b_d [i]]] / s [[c_d [i]]]))
        rel_white_all [[nm]] <- tmp [which (!is.na (tmp))]
    }
    index <- which (!is.na (rel_white_pkg))
    rel_white_score <- vapply (seq_along (rel_white_pkg), function (i) {
                                 s <- sort (rel_white_all [[i]])
                                 length (which (s < rel_white_pkg [[i]])) /
                                     length (s) },
                                 numeric (1))
    names (rel_white_score) <- names (rel_white_pkg)
    rel_white_pkg <- unlist (rel_white_pkg)

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
    #ptn <- "^desc_n|^num\\_|^files\\_inst|^files\\_src"
    ptn <- "^desc_n"
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
               grep ("^num\\_vignettes$", pc$measure),
               grep ("^n\\_fns\\_", pc$measure),
               grep ("^npars\\_", pc$measure),
               grep ("^doclines\\_", pc$measure),
               grep ("^n\\_edges", pc$measure))
    pc <- pc [sort (unique (keep)), ]
    pc <- pc [which (!is.na (pc$percentile)), ]
    pc <- pc [which (!grepl ("^n\\_edges\\_", pc$measure)), ]

    # reduce to median estimates only
    pc <- pc [which (!grepl ("\\_mn$", pc$measure)), ]
    pc$measure <- gsub ("\\_md$", "", pc$measure)

    # add relative white space metrics
    index <- which (!is.na (rel_white_pkg))
    measure = paste0 ("rel_whitespace_", names (rel_white_pkg))
    rel_white <- data.frame (measure = measure,
                             value = 100 * rel_white_pkg,
                             percentile = rel_white_score) [index, ]
    i <- max (grep ("^loc_", pc$measure))
    pc <- rbind (pc [seq (i), ],
                 rel_white,
                 pc [-seq (i), ])

    rownames (pc) <- NULL

    # additional tidying & removal:
    if (pc$percentile [pc$measure == "data_size_total"] == 0.0) {
        pc <- pc [which (!grepl ("^data\\_", pc$measure)), ]
    }
    if (pc$value [pc$measure == "files_inst"] == 0.0) {
        pc <- pc [which (pc$measure != "files_inst"), ]
    }
    # rm src if no src present
    index <- which (grepl ("\\_src$", pc$measure) & pc$value == 0)
    if (length (index) > 0) {
        pc <- pc [-index, ]
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

    # language summary:
    cloc <- cloc::cloc (file.path (attr (s, "path"), "R"))
    add_if <- function (pc, measure, fpath) {

        ret <- NULL
        i <- which (pc$measure == measure)
        if (length (i) > 0) {
            if (pc$value [i] > 0) {
                ret <- cloc::cloc (fpath)
            }
        }
        return (ret)
    }
    cloc <- rbind (cloc,
                   add_if (pc, "loc_src", file.path (attr (s, "path"), "src")))
    cloc <- rbind (cloc,
                   add_if (pc, "loc_inst",
                           file.path (attr (s, "path"), "src", "include")))

    cloc <- cloc [which (cloc$language != "SUM"), ]
    cloc$pc <- 100 * cloc$loc / sum (cloc$loc)
    attr (pc, "language") <- paste0 (cloc$language,
                                     ": ",
                                     round (cloc$pc),
                                     "%")
    attr (pc, "files") <- paste0 (cloc$language, ": ", cloc$file_count)

    return (pc)
}
