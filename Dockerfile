# This is built on top of a bspm image, generated with the first RUN comand.
# The second command then installs most but not all of the libraries used to
# build GitHub's Ubuntu-20.04 runner.
#
# After that are manual installs of ctags & the GitHub cli (`gh`).
# Finally, a standard setup for RCMD check, plus a few additional system
# libraries.

FROM eddelbuettel/r2u:20.04
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

# still need ubuntugis for gdal 3.1.0 (currently standard candidate is 3.0.4)
RUN apt-get update -qq && apt-get install -y software-properties-common
RUN add-apt-repository -y ppa:ubuntugis/ubuntugis-unstable \
    && apt update \
    && apt -y upgrade

# GitHub Ubuntu-20.04 runner, but not imagemagick because v7 needs to be
# compiled with librsvg2, rather than binary-installed
# https://github.com/actions/virtual-environments/blob/main/images/linux/Ubuntu2004-README.md
# netbase: https://github.com/tensorflow/haskell/issues/182
RUN apt-get update -qq && apt-get install -y \
    acl \
    apt-utils \
    autoconf automake \
    binutils \
    bison \
    brotli \
    build-essential \
    bzip2 \
    cargo \
    cmake \
    coinor-libcbc-dev  \
    coinor-libsymphony-dev \
    coreutils \
    curl \
    dbus \
    dnsutils \
    dos2unix \
    dpkg \
    fakeroot \
    file \
    flac \
    flex \
    fonts-emojione \
    fonts-noto-color-emoji \
    ftp \
    gcc \
    git \
    global \
    gnupg2 \
    haveged \
    iproute2 \
    iputils-ping \
    jags \
    jq \
    language-pack-en-base \
    lib32z1 \
    libapparmor-dev \
    libarchive-dev \
    libavfilter-dev \
    libbam-dev \
    libboost-filesystem-dev \
    libboost-program-options-dev \
    libc++-dev \
    libc++abi-dev \
    libcairo2-dev \
    libcurl4 \
    libcurl4-openssl-dev \
    libdb-dev \
    libeigen3-dev \
    libelf-dev \
    libfftw3-dev \
    libfreetype6-dev \
    libfribidi-dev \
    libgbm-dev \
    libgconf-2-4 \
    libgdal-dev \
    libgeos-dev \
    libgit2-dev \
    libglpk-dev \
    libglu1-mesa-dev \
    libgpgme-dev \
    libgsl-dev \
    libgtk-3-0 \
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
    libsecret-1-dev \
    libsodium-dev \
    libsqlite3-dev \
    libssh-dev \
    libssh2-1-dev \
    libssl-dev \
    libtesseract-dev \
    libtiff-dev \
    libudunits2-dev \
    libunwind8 \
    libv8-dev \
    libwebp-dev \
    libxkbfile-dev \
    libxml2-dev \
    libxslt-dev \
    libxslt1-dev \
    libxss1 \
    libyaml-dev \
    libzmq3-dev \
    locales \
    make \
    m4 \
    mediainfo \
    net-tools \
    netbase \
    netcat \
    openssh-client \
    p7zip-full \
    p7zip-rar \
    pandoc \
    pandoc-citeproc \
    parallel \
    pass \
    patchelf \
    pkg-config \
    pollinate \
    protobuf-compiler \
    python-is-python3 \
    python3-docutils \
    python3-numpy \
    python3-pip \
    r-base-dev \
    r-cran-rjava \
    rpm \
    rsync \
    shellcheck \
    sphinxsearch \
    sqlite3 \
    ssh \
    swig \
    telnet \
    tesseract-ocr-eng \
    texinfo \
    texlive-fonts-extra \
    texlive-fonts-recommended \
    texlive-latex-base \
    texlive-latex-extra \
    time \
    tk \
    ttf-mscorefonts-installer \
    tzdata \
    unixodbc-dev \
    unzip \
    upx \
    wget \
    xorriso \
    xvfb \
    xz-utils \
    zip \
    zlib1g-dev \
    zstd \
    zsync && \
    apt-get clean

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
RUN pip install jill
RUN jill install --confirm

# Extra python packages:
RUN pip install numpy pandas
# ---- Authors: Please submit PRs which insert extra python requirements here,
# ----  followed by package name and "#<ropensci/software-review issue number>":
RUN pip install earthengine-api # rgeeExtra #608

# https://arrow.apache.org/docs/r/articles/install.html#s3-support
ENV ARROW_S3 "ON"

# ropensci-review-tools/pkgcheck/issues/134:
#ENV R_REMOTES_UPGRADE "always"
ENV NOT_CRAN "true"

# A selection of R packages, including extra stats packages
RUN install2.r \
  arrow \
  decor \
  devtools \
  distill \
  duckdb \
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
  seasonal \
  sf \
  survival \
  tidymodels \
  tidyverse \
  xts \
  zoo

RUN installGithub.r \
    mangothecat/goodpractice

# arrow docs suggest this shouldn't be needed, but s3
# support doesn't work without re-install/compile:
RUN Rscript -e 'arrow::install_arrow()'

# Plus current ubuntu-unstable versions cause failed linkage of sf to GEOS, so
# need to reinstall 'sf' without bspm:
RUN Rscript -e 'bspm::disable();install.packages("sf");bspm::enable()'
