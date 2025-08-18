# in the style of withr
# - adds a valid DESCRIPTION file to dir_paths
# - executes code
# - removes DESCRIPTION files
with_descriptions <- function (dir_paths, code) {
    f_descs <- fs::path (dir_paths, "DESCRIPTION")
    withr::with_file (
        f_descs,
        {
            desc <- system.file ("DESCRIPTION", package = "pkgcheck")
            sapply (f_descs, \(f_desc) fs::file_copy (desc, f_desc, overwrite = TRUE))
            code
        }
    )
}

# Generate 8 letter name for fake package
pkgname <- function () paste0 (
    sample (c (letters, LETTERS), 8),
    collapse = ""
)