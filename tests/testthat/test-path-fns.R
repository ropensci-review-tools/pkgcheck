testthat::skip_on_os ("windows")
testthat::skip_on_os ("mac")

err_msg <- "Could not find unambiguous project root"

# Results on non-Linux systems are equal paths, but not identical

test_that ("convert_path works with pkgdir same as and subdir of git root", {
    git_root <- fs::as_fs_path (withr::local_tempdir (pkgname ()))
    sub_dir <- fs::as_fs_path (withr::local_tempdir ("subdir", tmpdir = git_root))
    gert::git_init (git_root)
    with_descriptions (git_root, {
        # If description is in git_root,
        # convert_path returns git_root
        # regardless of whether git_root or subdir is provided
        expect_identical (convert_path (git_root), git_root)
        expect_identical (convert_path (sub_dir), git_root)
    })
    with_descriptions (sub_dir, {
        # If description is in subdir,
        # convert_path returnssubdir
        # regardless of whether git_root or subdir is provided
        expect_identical (convert_path (git_root), sub_dir)
        expect_identical (convert_path (sub_dir), sub_dir)
    })
    with_descriptions (c (git_root, sub_dir), {
        # If description in both subdir and git root
        # return whichever is passed in
        expect_identical (convert_path (git_root), git_root)
        expect_identical (convert_path (sub_dir), sub_dir)
    })
})

test_that ("convert_path works with pkg outside of git repo", {
    pkg_root <- fs::as_fs_path (withr::local_tempdir (pkgname ()))
    sub_dir <- fs::as_fs_path (withr::local_tempdir ("subdir", tmpdir = pkg_root))

    with_descriptions (pkg_root, {
        # If description is in pkg_root, return it
        expect_identical (convert_path (pkg_root), pkg_root)
        expect_identical (convert_path (sub_dir), pkg_root)
    })
    with_descriptions (sub_dir, {
        # If description is in subdir,
        # convert_path returns subdir
        # regardless of whether git_root or subdir is provided
        expect_identical (convert_path (pkg_root), sub_dir)
        expect_identical (convert_path (sub_dir), sub_dir)
    })
    with_descriptions (c (pkg_root, sub_dir), {
        # If description in both subdir and git root
        # return whichever is passed in
        expect_identical (convert_path (pkg_root), pkg_root)
        expect_identical (convert_path (sub_dir), sub_dir)
    })
})

test_that ("convert_path errors if no description found", {
    pkg_root <- fs::as_fs_path (withr::local_tempdir (pkgname ()))
    sub_dir <- fs::as_fs_path (withr::local_tempdir ("subdir", tmpdir = pkg_root))
    expect_error (convert_path (pkg_root), err_msg)
    expect_error (convert_path (sub_dir), err_msg)
    gert::git_init (pkg_root)
    expect_error (convert_path (pkg_root), err_msg)
    expect_error (convert_path (sub_dir), err_msg)
})

test_that ("convert_path works with description in multiple subdirs", {
    pkg_root <- fs::as_fs_path (withr::local_tempdir (pkgname ()))
    sub_dirs <- fs::as_fs_path (c (
        withr::local_tempdir ("subdir", tmpdir = pkg_root),
        withr::local_tempdir ("subdir", tmpdir = pkg_root)
    ))
    with_descriptions (sub_dirs, {
        expect_error (convert_path (pkg_root), err_msg)
        expect_identical (convert_path (sub_dirs[1]), sub_dirs[1])
        expect_identical (convert_path (sub_dirs[2]), sub_dirs[2])
    })
    gert::git_init (pkg_root)
    with_descriptions (sub_dirs, {
        expect_error (convert_path (pkg_root), err_msg)
        expect_identical (convert_path (sub_dirs[1]), sub_dirs[1])
        expect_identical (convert_path (sub_dirs[2]), sub_dirs[2])
    })
})
