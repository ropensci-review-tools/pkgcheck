# This is built on top of a bspm image, generated with the first RUN comand.
# The second command then installs most but not all of the libraries used to
# build GitHub's Ubuntu-24.04 runner.
#
# After that are manual installs of ctags & the GitHub cli (`gh`).
# Finally, a standard setup for RCMD check, plus a few additional system
# libraries.

FROM eddelbuettel/r2u:24.04
MAINTAINER Mark Padgham <mark.padgham@email.com>

RUN apt-get update && apt-get install -y --no-install-recommends \
                sudo \
                r-cran-bspm \
        && echo "bspm::enable()" >> /etc/R/Rprofile.site \
        && echo "options(bspm.sudo=TRUE)" >> /etc/R/Rprofile.site \
        && echo 'APT::Install-Recommends "false";' > /etc/apt/apt.conf.d/90local-no-recommends \
        && echo "docker ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/local-docker-user \
        && chmod 0440 /etc/sudoers.d/local-docker-user \
        && chgrp 1000 /usr/local/lib/R/site-library \
        && install.r remotes

# still need ubuntugis for gdal
RUN apt-get update -qq \
    && apt-get install -y software-properties-common gpg-agent \
    && apt-get update
RUN add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable \
    && apt update \
    && apt -y upgrade

# GitHub Ubuntu-24.04 runner, but not imagemagick because v7 needs to be
# compiled with librsvg2, rather than binary-installed
# https://github.com/actions/runner-images/blob/main/images/ubuntu/Ubuntu2004-Readme.md
# netbase: https://github.com/tensorflow/haskell/issues/182
RUN apt-get update -qq && apt-get install -y \
    acl \
    aria2 \
    autoconf \
    automake \
    binutils \
    bison \
    brotli \
    bzip2 \
    coreutils \
    curl \
    dbus \
    dnsutils \
    dpkg \
    dpkg-dev \
    fakeroot \
    file \
    findutils \
    flex \
    fonts-noto-color-emoji \
    ftp \
    g++ \
    gcc \
    gnupg2 \
    haveged \
    iproute2 \
    iputils-ping \
    jq \
    lib32z1 \
    libc++-dev \
    libc++abi-dev \
    libc6-dev \
    libcurl4 \
    libgbm-dev \
    libgsl-dev \
    libgtk-3-0 \
    libmagic-dev \
    libmagickcore-dev \
    libmagickwand-dev \
    libsecret-1-dev \
    libsqlite3-dev \
    libtool \
    libunwind8 \
    libxkbfile-dev \
    libxss1 \
    libyaml-dev \
    locales \
    m4 \
    make \
    mediainfo \
    mercurial \
    net-tools \
    netcat-openbsd \
    openssh-client \
    p7zip-full \
    p7zip-rar \
    parallel \
    pass \
    patchelf \
    pigz \
    pkg-config \
    pollinate \
    python-is-python3 \
    rpm \
    rsync \
    shellcheck \
    sphinxsearch \
    sqlite3 \
    ssh \
    sshpass \
    swig \
    tar \
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
    zsync && \
    apt-get clean

RUN apt-get update -qq && apt-get install -y \
    apt-utils \
    build-essential \
    cargo \
    cmake \
    coinor-libcbc-dev  \
    coinor-libsymphony-dev \
    dos2unix \
    flac \
    fonts-emojione \
    git \
    global \
    jags \
    language-pack-en-base \
    libapparmor-dev \
    libarchive-dev \
    libavfilter-dev \
    libbam-dev \
    libboost-filesystem-dev \
    libboost-program-options-dev \
    libcairo2-dev \
    libcurl4-openssl-dev \
    libdb-dev \
    libeigen3-dev \
    libelf-dev \
    libfftw3-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgdal-dev \
    libgeos-dev \
    libghc-citeproc-dev \
    libgit2-dev \
    libglpk-dev \
    libglu1-mesa-dev \
    libgpgme-dev \
    libharfbuzz-dev \
    libhdf5-dev \
    libhiredis-dev \
    libicu-dev \
    libjansson-dev \
    libjpeg-dev \
    libjq-dev \
    libmagick++-dev \
    libmpfr-dev \
    libmysqlclient-dev \
    libnetcdf-dev \
    libnng-dev \
    libopenbabel-dev \
    libopenblas0 \
    libopencv-dev \
    libpng-dev \
    libpoppler-cpp-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libprotoc-dev \
    librabbitmq-dev \
    librdf0 \
    librrd-dev \
    librsvg2-dev \
    libsasl2-dev \
    libseccomp-dev \
    libsodium-dev \
    libssh-dev \
    libssh2-1-dev \
    libssl-dev \
    libtesseract-dev \
    libtiff-dev \
    libudunits2-dev \
    libv8-dev \
    libwebp-dev \
    libxml2-dev \
    libxslt-dev \
    libxslt1-dev \
    libzmq3-dev \
    netbase \
    pandoc \
    protobuf-compiler \
    python3-dev \
    python3-full \
    python3-docutils \
    python3-numpy \
    python3-pandas \
    python3-pip \
    python3-venv \
    r-base-dev \
    r-cran-rjava \
    tesseract-ocr-eng \
    texlive-fonts-extra \
    texlive-fonts-recommended \
    texlive-latex-base \
    texlive-latex-extra \
    ttf-mscorefonts-installer \
    unixodbc-dev \
    zlib1g-dev \
    zstd && \
    apt-get clean

# For some reason, librdf0-dev doesn't install in the list above:
# (See also pkgcheck-action#48)
RUN apt-get install -y librdf0-dev

# ctags install
RUN git clone https://github.com/universal-ctags/ctags.git \
    && cd ctags \
    && ./autogen.sh \
    && ./configure --prefix=/usr \
    && make \
    && make install

# gh cli: https://github.com/cli/cli/blob/trunk/docs/install_linux.md
RUN curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg | dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg \
    && chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg \
    && echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | tee /etc/apt/sources.list.d/github-cli.list > /dev/null \
    && apt update \
    && apt install gh -y

# nix:
RUN curl --proto '=https' --tlsv1.2 -sSf \
    -L https://install.determinate.systems/nix | \
    sh -s -- install linux --no-confirm --init none

# Julia:
# https://github.com/ropensci-review-tools/roreviewapi/issues/28
RUN bash -ci "$(curl -fsSL https://raw.githubusercontent.com/abelsiqueira/jill/main/jill.sh)"

# Extra python packages:
RUN mkdir /home/.virtualenvs \
    && python3 -m venv /home/.virtualenvs/venv
# ---- Authors: Please submit PRs which insert extra python requirements here,
# ----  followed by package name and "#<ropensci/software-review issue number>":
RUN /home/.virtualenvs/venv/bin/pip install earthengine-api # rgeeExtra #608


# https://arrow.apache.org/docs/r/articles/install.html#s3-support
ENV ARROW_S3="ON"

# ropensci-review-tools/pkgcheck/issues/134:
#ENV R_REMOTES_UPGRADE="always"
ENV NOT_CRAN="true"
ENV CI="true"
ENV ROPENSCI="true"

# A selection of R packages, including extra stats packages
RUN install2.r \
  arrow \
  decor \
  devtools \
  distill \
  duckdb \
  foreign \
  glmnet \
  goodpractice \
  lme4 \
  mgcv \
  Rcpp \
  RcppArmadillo \
  RcppEigen \
  RcppParallel \
  randomForest \
  rdflib \
  reticulate \
  rmarkdown \
  seasonal \
  survival \
  tidymodels \
  tidyverse \
  xts \
  zoo

# This is the format needed to install from GitHub:
# RUN --mount=type=secret,id=GITHUB_PAT,env=GITHUB_PAT installGithub.r \
#     ropensci-review-tools/goodpractice

RUN Rscript -e 'reticulate::virtualenv_create()'

# arrow docs suggest this shouldn't be needed, but s3
# support doesn't work without re-install/compile:
RUN Rscript -e 'arrow::install_arrow()'

# Plus current ubuntu-unstable versions cause failed linkage of sf to GEOS, so
# need to reinstall 'sf' without bspm:
RUN Rscript -e 'bspm::disable();install.packages(c("sf","terra"));bspm::enable()'
