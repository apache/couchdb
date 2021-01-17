# Based on erlang:22
FROM elixir:1.10

RUN set -ex; \
    curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add -; \
    echo "deb https://deb.nodesource.com/node_10.x buster main" | tee /etc/apt/sources.list.d/nodesource.list; \
    echo "deb-src https://deb.nodesource.com/node_10.x buster main" | tee -a /etc/apt/sources.list.d/nodesource.list

RUN set -ex; \
    apt-get update; \
    apt-get install -y --no-install-recommends \
        libmozjs-60-dev \
        libicu-dev \
        python3-venv \
        python3-pip \
        python3-sphinx \
        nodejs

RUN pip3 install sphinx_rtd_theme