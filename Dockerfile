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

# Most but not all of the libraries from 
# https://github.com/actions/virtual-environments/blob/main/images/linux/Ubuntu2004-README.md
# but not imagemagick because v7 needs to be compiled with librsvg2, rather than
# binary-installed
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

# netbase is critical:
# https://github.com/commercialhaskell/stack/issues/2372#issuecomment-234113085
# https://github.com/tensorflow/haskell/issues/182
RUN apt-get install -y \
  cargo \
  global \
  libgit2-dev \
  libssl-dev \
  libcurl4-gnutls-dev \
  #libcurl4-openssl-dev \
  libglpk-dev \
  libsodium-dev \
  netbase \
  texlive-latex-base \
  texlive-fonts-recommended \
  texlive-latex-extra \
  texlive-fonts-extra \
  pandoc \
  pandoc-citeproc

RUN install2.r \
  plumber \
  devtools \
  rmarkdown \
  visNetwork \
&& installGithub.r \
      ropenscilabs/pkgstats \
      ropenscilabs/pkgreport

EXPOSE 8000

RUN echo "#!/bin/bash\nRscript -e 'pkgreport::serve_api(port=8000)'" > /server_api.sh \
  && chmod a+x /server_api.sh

CMD /server_api.sh
