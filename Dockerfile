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
        && Rscript -e "remotes::install_github('r-lib/pkgapi')" \
        && Rscript -e "remotes::install_github('mpadge/packgraph')" \
        && Rscript -e "remotes::install_github('mpadge/pkgreport')"

EXPOSE 8000
ENTRYPOINT ["R", "-e", "ps <- pkgreport::serve_api(port=8000)"]
