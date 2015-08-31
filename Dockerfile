FROM debian:jessie
MAINTAINER Robert Newson <rnewson@apache.org>
ENV DEBIAN_FRONTEND noninteractive

# Install prereqs
RUN apt-get update -y && apt-get install -y --no-install-recommends \
  apt-transport-https \
  build-essential \
  ca-certificates \
  curl \
  default-jdk \
  git \
  haproxy \
  libcurl4-openssl-dev \
  libicu-dev \
  libmozjs185-dev \
  libnspr4 \
  libnspr4-0d \
  libnspr4-dev \
  libwxgtk3.0 \
  openssl \
  pkg-config \
  procps \
  python \
  python-sphinx \
  texinfo \
  texlive-base \
  texlive-fonts-extra \
  texlive-fonts-recommended \
  texlive-latex-extra \
  wget \
  && curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - \
  && echo 'deb https://deb.nodesource.com/node jessie main' > /etc/apt/sources.list.d/nodesource.list \
  && echo 'deb-src https://deb.nodesource.com/node jessie main' >> /etc/apt/sources.list.d/nodesource.list \
  && apt-get update -y && apt-get install -y nodejs \
  && npm install -g npm && npm install -g grunt-cli

RUN wget http://packages.erlang-solutions.com/erlang/esl-erlang/FLAVOUR_1_general/esl-erlang_18.0-1~debian~jessie_amd64.deb && \
  dpkg -i esl-erlang_18.0-1~debian~jessie_amd64.deb

# Build rebar
RUN git clone https://github.com/rebar/rebar /usr/src/rebar \
 && (cd /usr/src/rebar ; make && mv rebar /usr/local/bin/)

RUN groupadd -r couchdb && useradd -d /usr/src/couchdb -g couchdb couchdb

# Build couchdb
COPY . /usr/src/couchdb
RUN cd /usr/src/couchdb && \
  sed -i'' '/require_otp_vsn/d' rebar.config.script && \
  ./configure && make && \
  chmod +x dev/run && chown -R couchdb:couchdb /usr/src/couchdb

USER couchdb

# Expose nodes on external network interface
RUN sed -i'' 's/bind_address = 127.0.0.1/bind_address = 0.0.0.0/' /usr/src/couchdb/rel/overlay/etc/default.ini

EXPOSE 5984 15984 25984 35984 15986 25986 35986
WORKDIR /usr/src/couchdb

ENTRYPOINT ["/usr/src/couchdb/dev/run"]
