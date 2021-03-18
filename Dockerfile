FROM rocker/tidyverse:4.0.3
MAINTAINER Mark Padgham <mark.padgham@email.com>

#RUN echo "GITHUB_PAT='<my_github_token>'" > ~/.Renviron

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libgit2-dev \
  libssl-dev \
  libcurl4-gnutls-dev \
  libglpk-dev \
  libsodium-dev \
  libxml2-dev \
  texlive-fonts-extra \
  texinfo

RUN install2.r \
  plumber \
  remotes \
  devtools \
&& installGithub.r \
      r-lib/pkgapi \
      ropenscilabs/packgraph \
      ropenscilabs/pkgreport

EXPOSE 8000

RUN echo "#!/bin/bash\nRscript -e 'pkgreport::serve_api(port=8000, bg = FALSE)'" > /server_api.sh \
  && chmod a+x /server_api.sh

CMD /server_api.sh

#COPY inst/plumber.R /

#ENTRYPOINT ["R", "-e", "pr <- pkgreport::serve_api(port = 8000L, bg = FALSE)"]

#ARG ENTRYPOINT_FILE=/usr/local/lib/R/site-library/pkgreport/plumber.R
#RUN cp ${ENTRYPOINT_FILE} ~/plumber.R

#CMD ["~/plumber.R"]
