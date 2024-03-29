% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkgcheck-bg.R
\name{pkgcheck_bg}
\alias{pkgcheck_bg}
\title{Generate report on package compliance with rOpenSci Statistical Software
requirements as background process}
\usage{
pkgcheck_bg(path)
}
\arguments{
\item{path}{Path to local repository}
}
\value{
A \pkg{processx} object connecting to the background process
generating the main \link{pkgcheck} results (see Note).
}
\description{
Generate report on package compliance with rOpenSci Statistical Software
requirements as background process
}
\note{
The return object will by default display whether it is still running,
or whether it has finished. Once it has finished, the results can be obtained
by calling \verb{$get_result()}, or the main \link{pkgcheck} function can be
called to quickly retrieve the main results from local cache.

This function does not accept the \code{extra_env} parameter of the main
\link{pkgcheck} function, and can not be used to run extra, locally-defined
checks.
}
\examples{
\dontrun{
# Foreground checks as "blocking" process which will return
# only after all checks have finished:
checks <- pkgcheck ("/path/to/my/package")

# Or run process in background, do other things in the meantime,
# and obtain checks once they have finished:
ps <- pkgcheck_bg ("/path/to/my/package")
ps # print status to screen, same as 'ps$print()'
# To examine process state while running:
f <- ps$get_output_file ()
readLines (f) # or directly open file with local file viewer
# ... ultimately wait until 'running' changes to 'finished', then:
checks <- ps$get_result ()
}
}
\seealso{
Other pkgcheck_fns: 
\code{\link{pkgcheck}()},
\code{\link{print.pkgcheck}()}
}
\concept{pkgcheck_fns}
