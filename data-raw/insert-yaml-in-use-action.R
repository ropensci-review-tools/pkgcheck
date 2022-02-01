# Insert the yaml inputs from pkgcheck-actions into the example code for the
# use_github_action_pkgcheck function.

# Grab inputs from pkgcheck-action/action.yaml
action_dir <- normalizePath (file.path (here::here (), "..", "pkgcheck-action"))
if (!dir.exists (action_dir)) {
      stop ("Directory [", action_dir, "] not found.")
  }

y <- readLines (file.path (action_dir, "action.yaml"))
index <- grep ("^[[:alpha:]].*\\:$", y)
inputs <- y [seq (index [1], index [2] - 1)]
inputs <- inputs [nzchar (inputs)]

f <- file.path (here::here (), "R", "github.R")
r_fn <- readLines (f)
i1 <- grep ("^#'\\s\\`\\`\\`yaml$", r_fn)
i2 <- grep ("^#'\\s\\`\\`\\`$", r_fn)
i2 <- i2 [which (i2 > i1)] [1]
r_fn <- c (
    r_fn [seq (i1)],
    paste0 ("#' ", inputs),
    r_fn [i2:length (r_fn)]
)
writeLines (r_fn, f)
