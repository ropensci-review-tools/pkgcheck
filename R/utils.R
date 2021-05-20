
#' Tick symbol for markdown output
#' @noRd
symbol_tck <- function () {
    ":heavy_check_mark:"
}

#' Cross symbol for markdown output
#' @noRd
symbol_crs <- function () {
    ":heavy_multiplication_x:"
}

url_from_desc <- function (path) {

    desc <- file.path (path, "DESCRIPTION")
    if (!file.exists (desc))
        return (NULL)

    d <- data.frame (read.dcf (desc))
    if (!"URL" %in% names (d))
        return (NULL)

    u <- strsplit (d$URL, "\\s+") [[1]]
    u <- grep ("^https", u, value = TRUE)
    if (length (u) > 1)
        u <- grep ("git", u, value = TRUE)
    if (length (u) > 1)
        u <- u [which (!grepl ("\\.io", u))]

    u <- gsub (",|\\s+", "", u)

    return (u [1])
}

#' Bob Rudis's URL checker function
#'
#' @param x a single URL
#' @param non_2xx_return_value what to do if the site exists but the HTTP status
#' code is not in the `2xx` range. Default is to return `FALSE`.
#' @param quiet if not `FALSE`, then every time the `non_2xx_return_value`
#' condition arises a warning message will be displayed. Default is `FALSE`.
#' @param ... other params (`timeout()` would be a good one) passed directly to
#' `httr::HEAD()` and/or `httr::GET()`
#'
#' @note
#' https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
#' @noRd
url_exists <- function(x, non_2xx_return_value = FALSE, quiet = FALSE, ...) {

  # you don't need thse two functions if you're already using `purrr`
  # but `purrr` is a heavyweight compiled pacakge that introduces
  # many other "tidyverse" dependencies and this doesnt.

  capture_error <- function(code, otherwise = NULL, quiet = TRUE) {
    tryCatch(
      list(result = code, error = NULL),
      error = function(e) {
        if (!quiet)
          message("Error: ", e$message)

        list(result = otherwise, error = e)
      },
      interrupt = function(e) {
        stop("Terminated by user", call. = FALSE)
      }
    )
  }

  safely <- function(.f, otherwise = NULL, quiet = TRUE) {
    function(...) capture_error(.f(...), otherwise, quiet)
  }

  sHEAD <- safely(httr::HEAD)   # nolint
  sGET <- safely(httr::GET)     # nolint

  # Try HEAD first since it's lightweight
  res <- sHEAD(x, ...)

  if (is.null(res$result) ||
      ((httr::status_code(res$result) %/% 200) != 1)) {

    res <- sGET(x, ...)

    if (is.null (res$result))
        return (NA) # or whatever you want to return on "hard" errors

    if (((httr::status_code(res$result) %/% 200) != 1)) {
      if (!quiet)
          warning (paste0 ("Requests for [",
                           x,
                           "] responded but without an HTTP status ",
                           "code in the 200-299 range"))
      return (non_2xx_return_value)
    }

    return(TRUE)

  } else {
    return(TRUE)
  }

}
