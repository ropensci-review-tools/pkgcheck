# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    # CACHE_DIR can be set as local envvar, but entire package uses `options`
    # and not `Sys.getenv`!
    cache_dir <- Sys.getenv ("PKGCHECK_CACHE_DIR")

    if (cache_dir == "") {
        cache_dir <- file.path (rappdirs::user_cache_dir (), "pkgcheck")
    }

    cache_dir <- fs::path_abs (cache_dir)

    if (!dir.exists (cache_dir)) {
        dir.create (cache_dir, recursive = TRUE)
    }

    if (!dir.exists (file.path (cache_dir, "static"))) {
        dir.create (file.path (cache_dir, "static"), recursive = TRUE)
    }

    op <- options ()

    op.pkgcheck <- list (pkgcheck.cache_dir = cache_dir) # nolint

    toset <- !(names (op.pkgcheck) %in% names (op))
    if (any (toset)) {
        options (op.pkgcheck [toset])
    }
    invisible ()
}

.onUnload <- function (libname, pkgname) { # nolint

    options ("pkgcheck.cache_dir" = NULL)
}
# nocov end
