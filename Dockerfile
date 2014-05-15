FROM debian:7.4
MAINTAINER Robert Newson <rnewson@apache.org>
ENV DEBIAN_FRONTEND noninteractive

# Install prereqs
RUN echo "deb http://http.debian.net/debian wheezy-backports main" >> /etc/apt/sources.list
RUN apt-get -qq update
RUN apt-get -y install build-essential git libmozjs185-dev libicu-dev erlang-nox rebar

# Set up user for the build
RUN useradd -m couchdb
USER couchdb

# Get the source
WORKDIR /home/couchdb
RUN git clone https://git-wip-us.apache.org/repos/asf/couchdb.git

# Switch branches
WORKDIR /home/couchdb/couchdb
RUN git checkout 1843-feature-bigcouch

# We don't to be so strict for simple testing.
RUN sed -i'' '/require_otp_vsn/d' rebar.config.script

# Expose nodes on external network interface
RUN sed -i'' 's/bind_address = 127.0.0.1/bind_address = 0.0.0.0/' rel/overlay/etc/default.ini

# Build
RUN ./configure
RUN make

EXPOSE 15984 25984 35984
ENTRYPOINT ["/home/couchdb/couchdb/dev/run"]
