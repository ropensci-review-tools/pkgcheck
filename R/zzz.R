# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    cache_dir <- Sys.getenv ("pkgcheck_cache_dir")

    if (cache_dir == "") {
        cache_dir <- file.path (rappdirs::user_cache_dir (),
                                "pkgcheck")
        cache_dir <- normalizePath (cache_dir)
        Sys.setenv ("pkgcheck_cache_dir" = cache_dir)
    }

    if (!dir.exists (cache_dir)) {
        dir.create (cache_dir, recursive = TRUE)
    }

    op <- options ()

    op.pkgcheck <- list (pkgcheck.cache_dir = cache_dir) # nolint

    toset <- !(names (op.pkgcheck) %in% names (op))
    if (any (toset))
        options (op.pkgcheck [toset])
    invisible ()
}
# nocov end
