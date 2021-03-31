#FROM rocker/tidyverse:4.0.3
FROM rocker/r-ubuntu:20.04
MAINTAINER Mark Padgham <mark.padgham@email.com>

RUN add-apt-repository --yes "ppa:edd/r-4.0" \
	&& apt-get install -y --no-install-recommends \
                sudo \
                r-cran-bspm \
        && echo "bspm::enable()" >> /etc/R/Rprofile.site \
        && echo "options(bspm.sudo=TRUE)" >> /etc/R/Rprofile.site \
        && echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/90local-no-recommends \
        && echo "docker ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/local-docker-user \
        && chmod 0440 /etc/sudoers.d/local-docker-user \
        && chgrp 1000 /usr/local/lib/R/site-library \
        && install.r remotes

RUN echo "GITHUB_PAT='<my_github_token>'" > ~/.Renviron

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libgit2-dev \
  libssl-dev \
  libcurl4-gnutls-dev \
  libglpk-dev \
  libsodium-dev \
  libxml2-dev \
  texlive-latex-base \
  texlive-fonts-recommended \
  texlive-latex-extra \
  texlive-fonts-extra \
  texinfo \
  pandoc

RUN install2.r \
  plumber \
  devtools \
&& installGithub.r \
      r-lib/pkgapi \
      ropenscilabs/packgraph \
      ropenscilabs/pkgreport

#RUN echo "suppressMessages(bspm::enable())" > ~/.Rprofile

EXPOSE 8000

RUN echo "#!/bin/bash\nRscript -e 'pkgreport::serve_api(port=8000)'" > /server_api.sh \
  && chmod a+x /server_api.sh

CMD /server_api.sh

#COPY inst/plumber.R /

#ENTRYPOINT ["R", "-e", "pr <- pkgreport::serve_api(port = 8000L, bg = FALSE)"]

#ARG ENTRYPOINT_FILE=/usr/local/lib/R/site-library/pkgreport/plumber.R
#RUN cp ${ENTRYPOINT_FILE} ~/plumber.R

#CMD ["~/plumber.R"]
