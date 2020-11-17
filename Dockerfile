FROM rocker/tidyverse:4.0.3
MAINTAINER Mark Padgham <mark.padgham@email.com>

RUN apt-get update -qq && apt-get install -y \
  git-core \
  libgit2-dev \
  libssl-dev \
  libcurl4-gnutls-dev \
  libsodium-dev \
  libxml2-dev

RUN install2.r \
  plumber \
  remotes \
  devtools \
&& installGithub.r \
      r-lib/pkgapi \
      ropenscilabs/packgraph \
      ropenscilabs/pkgreport

RUN echo "#!/bin/bash\nRscript -e 'pkgreport::serve_api(port=8000, bg = FALSE)'" > /server_api.sh \
  && chmod a+x /server_api.sh

CMD /server_api.sh
EXPOSE 8000
