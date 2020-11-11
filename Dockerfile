FROM rocker/r-base
MAINTAINER Mark Padgham <mark.padgham@email.com>

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libgit2-dev \
  libssl-dev \
  libcurl4-gnutls-dev \
  libsodium-dev \
  libxml2-dev

RUN install2.r plumber \
        remotes \
        devtools \
        && R -e "remotes::install_github('r-lib/pkgapi')" \
        && R -e "remotes::install_github('mpadge/packgraph')" \
        && R -e "remotes::install_github('mpadge/pkgreport')"

EXPOSE 8000
ENTRYPOINT ["R", "-e", "pr <- plumber::plumb(commandArgs()[4]); pr$run(host='0.0.0.0', port=8000)"]
CMD ["/usr/local/lib/R/site-library/plumber/examples/04-mean-sum/plumber.R"]
