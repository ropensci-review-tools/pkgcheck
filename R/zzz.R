# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    cache_dir <- Sys.getenv ("pkgreport_cache_dir")

    if (cache_dir == "") {
        cache_dir <- file.path (rappdirs::user_cache_dir (),
                                "pkgreport")
    }
    cache_dir <- normalizePath (cache_dir)

    if (!dir.exists (cache_dir)) {
        dir.create (cache_dir, recursive = TRUE)
    }

    op <- options ()

    op.pkgreport <- list (pkgreport.cache_dir = cache_dir)

    toset <- !(names (op.pkgreport) %in% names (op))
    if (any (toset))
        options (op.pkgreport [toset])
    invisible ()
}
# nocov end
