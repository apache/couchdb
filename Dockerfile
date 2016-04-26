FROM debian:wheezy
MAINTAINER Robert Newson <rnewson@apache.org>
ENV DEBIAN_FRONTEND noninteractive

# Configure backports
RUN echo "deb http://http.debian.net/debian wheezy-backports main" >> /etc/apt/sources.list
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
    python

# Install Nodejs
RUN curl -sL https://deb.nodesource.com/setup | bash -	
RUN apt-get install -y nodejs

# Build rebar
RUN useradd -m rebar
USER rebar
WORKDIR /home/rebar
RUN curl -L https://github.com/rebar/rebar/archive/2.5.0.tar.gz | tar zxf -
WORKDIR /home/rebar/rebar-2.5.0
RUN ./bootstrap
USER root
RUN cp rebar /usr/local/bin/
RUN chmod 755 /usr/local/bin/rebar

# Build couchdb
RUN useradd -m couchdb
RUN mkdir -p /home/couchdb
ADD . /home/couchdb
USER root
RUN chown -R couchdb:couchdb /home/couchdb
USER couchdb
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
