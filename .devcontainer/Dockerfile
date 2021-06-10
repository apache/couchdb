ARG FDB_VERSION
ARG ELIXIR_VERSION

# Grab fdbcli and client library from same image as server
FROM foundationdb/foundationdb:${FDB_VERSION} as fdb

# Debian image with Erlang + Elixir installed (we need elixir for test suite)
FROM elixir:${ELIXIR_VERSION}

# The FROM directive above sweeps out the ARGs so we need to re-declare here
# in order to use it again to download the FDB client package
ARG FDB_VERSION

# Install SpiderMonkey 60 and tell CouchDB to use it in configure
ARG SM_VSN
ENV SM_VSN=${SM_VSN:-60}

RUN set -ex; \
    wget https://www.foundationdb.org/downloads/${FDB_VERSION}/ubuntu/installers/foundationdb-clients_${FDB_VERSION}-1_amd64.deb; \
    mkdir /var/lib/foundationdb; \
    dpkg -i foundationdb-clients_${FDB_VERSION}-1_amd64.deb

# Use NodeSource binaries for Node.js (Fauxton dependency)
RUN set -ex; \
    curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -; \
    echo "deb https://deb.nodesource.com/node_10.x buster main" | tee /etc/apt/sources.list.d/nodesource.list; \
    echo "deb-src https://deb.nodesource.com/node_10.x buster main" | tee -a /etc/apt/sources.list.d/nodesource.list

RUN set -ex; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        dnsutils \
        libmozjs-${SM_VSN}-dev \
        libicu-dev \
        python3-venv \
        python3-pip \
        python3-sphinx \
        nodejs

# Documentation theme
RUN pip3 install sphinx_rtd_theme

COPY --from=fdb /var/fdb/scripts/create_cluster_file.bash /usr/local/bin/

CMD sleep infinity
