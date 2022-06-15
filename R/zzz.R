# nocov start
.onLoad <- function (libname, pkgname) { # nolint

    cache_dir <- Sys.getenv ("PKGCHECK_CACHE_DIR")

    if (cache_dir == "") {
        cache_dir <- fs::path_expand (fs::path (
            rappdirs::user_cache_dir (),
            "R",
            "pkgcheck"
        ))
        Sys.setenv ("PKGCHECK_CACHE_DIR" = cache_dir)
        Sys.setenv ("PKGCHECK_CACHE_DIR_UNSET" = "true")
    }
}

.onUnload <- function (libname, pkgname) { # nolint

    if (Sys.getenv ("PKGCHECK_CACHE_DIR_UNSET") == "true") {

        Sys.unsetenv ("PKGCHECK_CACHE_DIR")
        Sys.unsetenv ("PKGCHECK_CACHE_DIR_UNSET")
    }
}
# nocov end
