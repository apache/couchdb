FROM debian:jessie
MAINTAINER Robert Newson <rnewson@apache.org>
ENV DEBIAN_FRONTEND noninteractive

# Configure backports
RUN apt-get -qq update

# Install prereqs
RUN apt-get --no-install-recommends -y install \
    build-essential \
    ca-certificates \
    curl \
    erlang-dev \
    erlang-nox \
    git \
    libicu-dev \
    libmozjs185-dev \
    python \
    nodejs-legacy

RUN curl -L --insecure -O https://www.npmjs.org/install.sh \
    && npm_install=2.8.4 /bin/bash install.sh


# Build couchdb
RUN useradd -m couchdb
ADD . /home/couchdb
WORKDIR /home/couchdb

# We don't to be so strict for simple testing.
RUN sed -i'' '/require_otp_vsn/d' rebar.config.script

# Expose nodes on external network interface
RUN sed -i'' 's/bind_address = 127.0.0.1/bind_address = 0.0.0.0/' rel/overlay/etc/default.ini

# Build
RUN ./configure
RUN make couch

EXPOSE 15984 25984 35984 15986 25986 35986
ENTRYPOINT ["/home/couchdb/dev/run"]
