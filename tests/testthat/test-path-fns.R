testthat::skip_on_os ("windows")
testthat::skip_on_os ("mac")

# Results on non-Linux systems are equal paths, but not identical

test_that ("pkgcheck", {

    pkgname <- paste0 (
        sample (c (letters, LETTERS), 8),
        collapse = ""
    )
    pkg_root <- fs::dir_create (fs::path_temp (), pkgname)
    path <- fs::dir_create (pkg_root, "subdir")
    f_desc <- fs::path (path, "DESCRIPTION")
    desc <- system.file ("DESCRIPTION", package = "pkgcheck")
    fs::file_copy (desc, f_desc, overwrite = TRUE)
    gert::git_init (pkg_root)

    path_conv <- convert_path (pkg_root)
    # path_conv should then be subdir:
    expect_identical (path_conv, path)

    fs::dir_delete (pkg_root)
})
