#!/usr/bin/env Rscript

# Tests to confirm that no external calls are make on CRAN-like setups.
#
# Every network-egress function used anywhere in this package is traced. All
# requests to GitHub APIs are logged *and* aborted, so this script both
# *confirms* and *guarantees* that no such request is ever actually made.
#
# Requests to other GitHub-hosted URLs (e.g. raw.githubusercontent.com) are
# only logged, but allowed to proceed as real network calls. Requests served
# from local httptest2 fixtures (tests/testthat/gh*/) never reach these traced
# functions in the first place, so they are correctly not flagged at all.
#
# NOTE: Some tests may fail here, and test snapshots may also be deleted by
# this script. Those should be replaced and/or ignored - the sole purpose of
# this script is to test external calls.

Sys.unsetenv (c (
    "GITHUB_PAT", "GITHUB_TOKEN", "GITHUB_PAT_GITHUB_COM",
    "RRT_TEST_ALL", "GITHUB_JOB", "GITHUB_ACTIONS"
))
stopifnot (
    "GITHUB_PAT is still set" = !nzchar (Sys.getenv ("GITHUB_PAT")),
    "GITHUB_TOKEN is still set" = !nzchar (Sys.getenv ("GITHUB_TOKEN")),
    "RRT_TEST_ALL is still set" = !nzchar (Sys.getenv ("RRT_TEST_ALL"))
)

# ---- Neutralise any locally-stored git credential helper -----------------
# `gh::gh_token()` falls back to `gitcreds::gitcreds_get()` when no PAT env
# var is set, which can still return a real token from a local credential
# helper. A clean CRAN machine has no such helper configured, so simulate
# that by making `gitcreds_get()` fail, which `gh_token()` already handles
# gracefully (falling back to an empty token).
trace (
    "gitcreds_get",
    tracer = quote (stop (
        "[cran-test] no git credential helper (simulated)",
        call. = FALSE
    )),
    print = FALSE,
    where = asNamespace ("gitcreds")
)

# ---- Detect any request to a GitHub host; block only real API calls ------
gh_call_log <- new.env ()
gh_call_log$calls <- list ()

record_and_maybe_block <- function (fn_name, url) {
    if (!is.character (url) || length (url) != 1L || is.na (url) ||
        !grepl ("github", url, ignore.case = TRUE)) {
        return (invisible (NULL))
    }

    is_api <- grepl ("^https?://api\\.github\\.com", url, ignore.case = TRUE)
    gh_call_log$calls [[length (gh_call_log$calls) + 1L]] <- list (
        fn = fn_name, url = url, is_api = is_api, time = Sys.time ()
    )

    if (is_api) {
        stop (
            "[cran-test] BLOCKED: '", fn_name,
            "' attempted a request to the GitHub API: ", url,
            call. = FALSE
        )
    }
}

traced_fns <- list (
    list (name = "curl_fetch_memory", pkg = "curl"),
    list (name = "curl_fetch_disk", pkg = "curl"),
    list (name = "curl_download", pkg = "curl"),
    list (name = "download.file", pkg = "utils")
)

for (fn in traced_fns) {
    trace (
        fn$name,
        tracer = bquote (
            record_and_maybe_block (. (paste0 (fn$pkg, "::", fn$name)), url)
        ),
        print = FALSE,
        where = asNamespace (fn$pkg)
    )
}

on.exit (
    {
        for (fn in traced_fns) {
            try (
                suppressMessages (
                    untrace (fn$name, where = asNamespace (fn$pkg))
                ),
                silent = TRUE
            )
        }
        try (
            suppressMessages (
                untrace ("gitcreds_get", where = asNamespace ("gitcreds"))
            ),
            silent = TRUE
        )
    },
    add = TRUE
)

# ---- Run the full test suite, exactly as `make test` does ----------------
library (testthat)
devtools::load_all ()

test_result <- tryCatch (
    testthat::test_local (reporter = "summary"),
    error = function (e) e
)

# ---- Report ----------------------------------------------------------------
cat ("\n\n==================== cran-test.R summary ====================\n")
cat ("GITHUB_PAT set:    ", nzchar (Sys.getenv ("GITHUB_PAT")), "\n")
cat ("RRT_TEST_ALL set:  ", nzchar (Sys.getenv ("RRT_TEST_ALL")), "\n")

is_api_call <- vapply (gh_call_log$calls, `[[`, logical (1), "is_api")
api_calls <- gh_call_log$calls [is_api_call]
other_calls <- gh_call_log$calls [!is_api_call]

if (length (api_calls) == 0L) {
    cat ("GitHub API calls: none detected (PASS)\n")
} else {
    cat ("GitHub API calls:", length (api_calls), "attempted (FAIL)\n")
    for (call in api_calls) {
        cat ("  -", call$fn, "->", call$url, "\n")
    }
}

if (length (other_calls) > 0L) {
    urls <- unique (vapply (other_calls, `[[`, character (1), "url"))
    cat (
        "\nOther GitHub-hosted (non-API, unauthenticated) ",
        "URLs also requested (",
        length (other_calls),
        "calls,",
        length (urls),
        "distinct URL(s) ):\n"
    )
    for (u in urls) cat ("  -", u, "\n")
}
cat ("===============================================================\n")

if (inherits (test_result, "error")) {
    stop (test_result)
}

if (length (api_calls) > 0L) {
    stop (
        "cran-test.R FAILED: ", length (api_calls),
        " attempted call(s) to the GitHub API were ",
        "detected during the test run."
    )
}
