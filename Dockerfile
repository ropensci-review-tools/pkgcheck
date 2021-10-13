# This is built on top of a bspm image, generated with the first RUN comand.
# The second command then installs most but not all of the libraries used to
# build GitHub's Ubuntu-20.04 runner.
#
# After that are manual installs of ctags & the GitHub cli (`gh`).
# Finally, a standard setup for RCMD check, plus a few additional system
# libraries.

FROM rocker/r-bspm:20.04
MAINTAINER Mark Padgham <mark.padgham@email.com>

RUN apt-get install -y --no-install-recommends \
                sudo \
                r-cran-bspm \
        && echo "bspm::enable()" >> /etc/R/Rprofile.site \
        && echo "options(bspm.sudo=TRUE)" >> /etc/R/Rprofile.site \
        && echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/90local-no-recommends \
        && echo "docker ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/local-docker-user \
        && chmod 0440 /etc/sudoers.d/local-docker-user \
        && chgrp 1000 /usr/local/lib/R/site-library \
        && install.r remotes


# GitHub Ubuntu-20.04 runner, but not imagemagick because v7 needs to be
# compiled with librsvg2, rather than binary-installed
# https://github.com/actions/virtual-environments/blob/main/images/linux/Ubuntu2004-README.md
RUN apt-get update -qq && apt-get install -y \
    acl \
    binutils \
    bison \
    brotli \
    build-essential \
    bzip2 \
    coreutils \
    curl \
    dbus \
    dnsutils \
    dpkg \
    fakeroot \
    file \
    flex \
    fonts-noto-color-emoji \
    ftp \
    gnupg2 \
    haveged \
    iproute2 \
    iputils-ping \
    jq \
    lib32z1 \
    libc++-dev \
    libc++abi-dev \
    libcurl4 \
    libgbm-dev \
    libgconf-2-4 \
    libgsl-dev \
    libgtk-3-0 \
    libsecret-1-dev \
    libsqlite3-dev \
    libunwind8 \
    libxkbfile-dev \
    libxss1 \
    locales \
    m4 \
    mediainfo \
    net-tools \
    netcat \
    openssh-client \
    p7zip-full \
    p7zip-rar \
    parallel \
    pass \
    patchelf \
    pkg-config \
    pollinate \
    python-is-python3 \
    rpm \
    rsync \
    shellcheck \
    sphinxsearch \
    sqlite3 \
    ssh \
    swig \
    telnet \
    texinfo \
    time \
    tk \
    tzdata \
    unzip \
    upx \
    wget \
    xorriso \
    xvfb \
    xz-utils \
    zip \
    zstd \
    zsync

# ctags install
RUN apt-get install -y \
    apt-utils \
    gcc make \
    autoconf automake \
    git-core \
    python3-docutils \
    libseccomp-dev \
    libjansson-dev \
    libyaml-dev \
    libxml2-dev
RUN git clone https://github.com/universal-ctags/ctags.git \
    && cd ctags \
    && ./autogen.sh \
    && ./configure --prefix=/usr \
    && make \
    && make install

# gh cli:
#RUN wget https://github.com/cli/cli/releases/download/v${VERSION}/gh_${VERSION}_linux_amd64.tar.gz
RUN VERSION=`curl "https://api.github.com/repos/cli/cli/releases/latest" | grep '"tag_name"' | sed -E 's/.*"([^"]+)".*/\1/' | cut -c2-` \
    && curl -sSL https://github.com/cli/cli/releases/download/v${VERSION}/gh_${VERSION}_linux_amd64.tar.gz -o gh_${VERSION}_linux_amd64.tar.gz \
    && tar xvf gh_${VERSION}_linux_amd64.tar.gz \
    && cp gh_${VERSION}_linux_amd64/bin/gh /usr/local/bin/
#RUN cp -r gh_${VERSION}_linux_amd64/share/man/man1/* /usr/share/man/man1/

# still need ubuntugis for gdal 3.1.0 (currently standard candidate is 3.0.4)
RUN add-apt-repository -y ppa:ubuntugis/ppa \
    && apt update \
    && apt -y upgrade

# netbase is critical:
# https://github.com/commercialhaskell/stack/issues/2372#issuecomment-234113085
# https://github.com/tensorflow/haskell/issues/182
RUN apt-get install -y \
  cargo \
  dos2unix \
  global \
  libgit2-dev \
  libssl-dev \
  libcurl4-gnutls-dev \
  #libcurl4-openssl-dev \
  libgdal-dev \
  libgeos-dev \
  libproj-dev \
  libglpk-dev \
  libsodium-dev \
  libudunits2-dev \
  netbase \
  texlive-latex-base \
  texlive-fonts-recommended \
  texlive-latex-extra \
  texlive-fonts-extra \
  pandoc \
  pandoc-citeproc

# A selection of R packages, including extra stats packages
RUN install2.r \
  devtools \
  foreign \
  glmnet \
  lme4 \
  mgcv \
  Rcpp \
  RcppArmadillo \
  RcppEigen \
  RcppParallel \
  randomForest \
  rmarkdown \
  sf \
  survival \
  tidymodels \
  tidyverse \
  xts \
  zoo
