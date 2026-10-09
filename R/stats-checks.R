#' Compare 'pkgstats' summary with statistics from all CRAN packages.
#' @param s Result of `pkgstats::pkgstats_summary`
#' @param threshold Proportion threshold below which to report on statistically
#' unusual properties.
#' @return A 'data.frame' of selected statistical properties and percentiles in
#' relation to all other packages on CRAN.
#' @noRd
stats_checks <- function (s, threshold = 0.05) {

    # npars is set to NA when there are none; replace with 0:
    if (is.na (s$npars_exported_mn)) {
        s$npars_exported_mn <- 0L
    }
    if (is.na (s$npars_exported_md)) {
        s$npars_exported_md <- 0L
    }

    if (is_test_env ()) {
        dat <- fake_pkgstats_test_data (s)
    } else {
        dat <- get_pkgstats_data ()
    }

    # convert blank line measures into relative
    b_s <- grep ("^blank\\_lines", names (s))
    b_d <- grep ("^blank\\_lines", names (dat))
    c_s <- grep ("^loc\\_", names (s)) # also includes loc_per_fn stats
    c_d <- grep ("^loc\\_", names (dat))
    rel_white_pkg <- rel_white_all <- list ()

    for (i in seq_along (b_s)) {

        # get directory name:
        nm <- gsub ("blank\\_lines\\_", "", names (s) [b_s [i]])
        rel_white_pkg [[nm]] <- as.numeric (unname (
            s [[b_s [i]]] / s [[c_s [i]]]
        ))
        tmp <- as.numeric (unname (dat [[b_d [i]]] / s [[c_d [i]]]))
        rel_white_all [[nm]] <- tmp [which (!is.na (tmp))]
    }

    index <- which (!is.na (rel_white_pkg))
    rel_white_score <- vapply (
        seq_along (rel_white_pkg), function (i) {
            s <- sort (rel_white_all [[i]])
            length (which (s < rel_white_pkg [[i]])) /
                length (s)
        },
        numeric (1)
    )
    names (rel_white_score) <- names (rel_white_pkg)
    rel_white_pkg <- unlist (rel_white_pkg)

    # temporary fixes until data are re-generated:
    dat$loc_R [dat$files_R == 0] <- NA_integer_
    dat$loc_src [dat$files_src == 0] <- NA_integer_
    dat$loc_inst [dat$files_inst == 0] <- NA_integer_
    dat$loc_vignettes [dat$files_vignettes == 0] <- NA_integer_
    dat$loc_tests [dat$files_tests == 0] <- NA_integer_

    nms <- names (dat)
    index <- which (vapply (
        nms, function (i) {
            is.numeric (dat [[i]])
        },
        logical (1)
    ))
    nms <- nms [index]
    # ptn <- "^desc_n|^num\\_|^files\\_inst|^files\\_src"
    ptn <- "^desc_n"
    nms <- nms [which (!grepl (ptn, nms))]
    dists <- lapply (nms, function (i) {
        sort (dat [[i]] [which (!is.na (dat [[i]]))])
    })
    names (dists) <- nms

    pc <- vapply (nms, function (i) {
        if (is.na (s [[i]])) {
            return (NA)
        }
        return (length (which (dists [[i]] < s [[i]])) /
            length (dists [[i]]))
    }, double (1))

    index <- match (names (pc), names (s))
    pc <- data.frame (
        measure = names (pc),
        value = as.numeric (s [1, index]),
        percentile = pc,
        row.names = NULL,
        stringsAsFactors = FALSE
    )

    keep <- c (
        grep ("^files\\_", pc$measure),
        grep ("^loc\\_", pc$measure),
        grep ("^data\\_", pc$measure),
        grep ("^num\\_vignettes$", pc$measure),
        grep ("^n\\_fns\\_", pc$measure),
        grep ("^npars\\_", pc$measure),
        grep ("^doclines\\_", pc$measure),
        grep ("^n\\_edges", pc$measure)
    )
    pc <- pc [sort (unique (keep)), ]
    pc <- pc [which (!is.na (pc$percentile)), ]
    pc <- pc [which (!grepl ("^n\\_edges\\_", pc$measure)), ]

    # reduce to median estimates only
    pc <- pc [which (!grepl ("\\_mn$", pc$measure)), ]
    pc$measure <- gsub ("\\_md$", "", pc$measure)

    # add relative white space metrics
    index <- which (!is.na (rel_white_pkg))
    measure <- paste0 ("rel_whitespace_", names (rel_white_pkg))
    rel_white <- data.frame (
        measure = measure,
        value = 100 * rel_white_pkg,
        percentile = rel_white_score,
        stringsAsFactors = FALSE
    ) [index, ]
    i <- max (grep ("^loc_", pc$measure))
    pc <- rbind (
        pc [seq (i), ],
        rel_white,
        pc [-seq (i), ]
    )

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
    loc <- pkgstats::loc_stats (fs::path (attr (s, "path")))
    loc <- loc [which (loc$dir %in% c ("R", "inst", "src")), ]
    loc <- vapply (unique (loc$language), function (i) {
        c (
            sum (loc$ncode [loc$language == i]) /
                sum (loc$ncode) * 100,
            sum (loc$nfiles [loc$language == i])
        )
    }, numeric (2))
    langs <- paste0 (colnames (loc), ": ", round (loc [1, ]), "%")
    files <- paste0 (colnames (loc), ": ", as.integer (loc [2, ]))

    attr (pc, "language") <- langs
    attr (pc, "files") <- files

    return (pc)
}

get_pkgstats_data <- function () {

    cache_path <- Sys.getenv ("PKGCHECK_CACHE_DIR")
    f_name <- "pkgstats-CRAN-current.Rds"
    f_path <- fs::path_norm (fs::path (cache_path, f_name))

    f_path <- dl_pkgstats_data (f_path)

    numeric_pkgstats_cols (readRDS (f_path))
}

#' Convert character columns of 'pkgstats' data which hold only numbers back
#' to numeric. The data uploaded on 2026-10-09 had every column as character,
#' and 'stats_checks()' then failed with "non-numeric argument to binary
#' operator".
#' @noRd
numeric_pkgstats_cols <- function (dat) {

    for (i in names (dat) [vapply (dat, is.character, logical (1L))]) {
        if (i %in% c ("package", "version", "date")) {
            next
        }
        v <- suppressWarnings (as.numeric (dat [[i]]))
        if (!any (is.na (v) & !is.na (dat [[i]]) & dat [[i]] != "NaN")) {
            dat [[i]] <- v
        }
    }

    return (dat)
}

dl_pkgstats_data <- function (f_path) {

    # The cache_path is set to tempdir in tests, in which case static data must
    # be used in order to generate reproducible test snapshots (see #204).
    # Default is otherwise to use daily updates of data which then constnatly
    # change snapshot results.
    cache_path <- Sys.getenv ("PKGCHECK_CACHE_DIR")
    cache_is_temp <- identical (
        normalizePath (dirname (cache_path)),
        normalizePath (tempdir ())
    )

    pkgstats_remote <- "https://github.com/ropensci-review-tools/pkgstats/"
    u_tag <- ifelse (
        cache_is_temp,
        "v0.1.2",
        utils::getFromNamespace ("RELEASE_TAG", "pkgstats")
    )
    u_base <- paste0 (pkgstats_remote, "releases/download/", u_tag, "/")
    f_name <- "pkgstats-CRAN-current.Rds"
    url <- paste0 (u_base, f_name)

    latest <- FALSE
    # Data are updated if older than:
    update_days <- 7L
    if (fs::file_exists (f_path)) {
        latest <- difftime (
            Sys.Date (),
            as.Date (fs::file_info (f_path)$modification_time),
            units = "days"
        ) <= update_days
    }

    if (!latest) {
        req <- httr2::request (url)
        req <- httr2::req_headers (req, "Accept" = "application/octet-stream")
        resp <- httr2::req_perform (req)

        if (httr2::resp_is_error (resp)) {
            return (NULL)
        }

        writeBin (httr2::resp_body_raw (resp), f_path)
    }

    return (f_path)
}

#' To avoid downloading full `pkgstats` data in tests, this takes the
#' `pkgstats_summary()` result, `s`, as input and returns an equivalent fake
#' version.
#'
#' @noRd
fake_pkgstats_test_data <- function (s, n = 100, seed = 1L) {

    withr::with_seed (seed, fake_pkgstats_test_data_impl (s, n = n))
}

fake_pkgstats_test_data_impl <- function (s, n = 100) {

    out <- cbind (s, rep (row.names (s), each = n))

    out$files_R <- stats::rnorm (n = n, mean = 8.77, sd = 16.6)
    out$files_src <- stats::rnorm (n = n, mean = 5.54, sd = 98.6)
    out$files_inst <- stats::rnorm (n = n, mean = 2.25, sd = 16.1)
    out$files_vignettes <- stats::rnorm (n = n, mean = 0.58, sd = 1.8)
    out$files_tests <- stats::rnorm (n = n, mean = 1.96, sd = 8.1)
    out$num_data_files <- stats::rnorm (n = n, mean = 6.32, sd = 18.5)

    out$loc_R <- stats::rnorm (n = n, mean = 671, sd = 2126)
    out$loc_src <- stats::rnorm (n = n, mean = 5073, 39032)
    out$loc_inst <- stats::rnorm (n = n, mean = 875, 7220)
    out$loc_vignettes <- stats::rnorm (n = n, mean = 341, 995)
    out$loc_tests <- stats::rnorm (n = n, mean = 428, 2071)
    out$loc_per_fn_r_mn <- stats::rnorm (n = n, mean = 34, 141)
    out$loc_per_fn_r_md <- stats::rnorm (n = n, mean = 27, 137)
    out$loc_per_fn_r_exp_mn <- stats::rnorm (n = n, mean = 34, 130)
    out$loc_per_fn_r_exp_md <- stats::rnorm (n = n, mean = 28, 126)
    out$loc_per_fn_r_not_exp_mn <- stats::rnorm (n = n, mean = 16, 91)
    out$loc_per_fn_r_not_exp_md <- stats::rnorm (n = n, mean = 12, 89)
    out$loc_per_fn_src_mn <- stats::rnorm (n = n, mean = 6.6, 26)
    out$loc_per_fn_src_md <- stats::rnorm (n = n, mean = 4.1, 22)

    out$data_size_total <- stats::rnorm (n = n, mean = 722702, 218470)
    out$data_size_median <- stats::rnorm (n = n, mean = 1782499, 740378)
    out$num_vignettes <- stats::rnorm (n = n, mean = 0.59, sd = 1.69)

    out$n_fns_r <- stats::rnorm (n = n, mean = 25, sd = 95)
    out$n_fns_r_exported <- stats::rnorm (n = n, mean = 15, sd = 47)
    out$n_fns_r_not_exported <- stats::rnorm (n = n, mean = 12, sd = 63)
    out$n_fns_src <- stats::rnorm (n = n, mean = 59, sd = 629)
    out$n_fns_per_file_r <- stats::rnorm (n = n, mean = 2., sd = 8.2)
    out$n_fns_per_file_src <- stats::rnorm (n = n, mean = 1.4, sd = 5.0)

    out$npars_exported_mn <- stats::rnorm (n = n, mean = 2.7, sd = 5.9)
    out$npars_exported_md <- stats::rnorm (n = n, mean = 2.4, sd = 5.8)

    out$doclines_per_fn_exp_mn <- stats::rnorm (n = n, mean = 34, sd = 60)
    out$doclines_per_fn_exp_md <- stats::rnorm (n = n, mean = 32, sd = 61)
    out$doclines_per_fn_not_exp_mn <- stats::rnorm (n = n, mean = 2.2, sd = 7.9)
    out$doclines_per_fn_not_exp_md <- stats::rnorm (n = n, mean = 1.47, sd = 7.3)
    out$doclines_per_fn_src_mn <- stats::rnorm (n = n, mean = 0.28, sd = 1.4)
    out$doclines_per_fn_src_md <- stats::rnorm (n = n, mean = 0.09, sd = 0.6)

    out$n_edges <- stats::rnorm (n = n, mean = 238, sd = 3224)
    out$n_edges_r <- stats::rnorm (n = n, mean = 37, sd = 178)
    out$n_edges_src <- stats::rnorm (n = n, mean = 201, sd = 3218)

    return (out)
}
