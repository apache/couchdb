#!/usr/bin/env bash

# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

set -ex

. /etc/os-release 2>/dev/null || true

# Choose Elixir version automatically based on Erlang version
if [ -z ${ELIXIR_VSN} ]; then
    if [[ ${ERLANG_VSN} =~ ^20 ]]; then
        ELIXIR_VSN='v1.9.4'
    elif [[ ${ERLANG_VSN} =~ ^21 ]]; then
        ELIXIR_VSN='v1.11.4'
    elif [[ ${ERLANG_VSN} =~ ^2[234] ]]; then
        ELIXIR_VSN='v1.12.3'
    else
        ELIXIR_VSN='v1.13.3'
    fi
fi

# SpiderMonkey likes to bake the version into the package name
if [ "${VERSION_CODENAME}" == "stretch" ]; then
    # We provide our own build of SM 1.8.5 for ancient distros
    curl https://couchdb.apache.org/repo/keys.asc | gpg --dearmor | tee /usr/share/keyrings/couchdb-archive-keyring.gpg >/dev/null 2>&1
    echo "deb [signed-by=/usr/share/keyrings/couchdb-archive-keyring.gpg] https://apache.jfrog.io/artifactory/couchdb-deb/ stretch main" \
    | tee /etc/apt/sources.list.d/couchdb.list >/dev/null
    spidermonkey_pkg='couch-libmozjs185-dev'
elif [ "${VERSION_CODENAME}" == "buster" ]; then
    spidermonkey_pkg='libmozjs-60-dev'
elif [ "${VERSION_CODENAME}" == "bullseye" ]; then
    spidermonkey_pkg='libmozjs-78-dev'
fi

# Activate NodeSource repo for Node.js (Fauxton dependency)
wget -q https://deb.nodesource.com/setup_${NODE_VSN}.x
/bin/bash setup_${NODE_VSN}.x
rm setup_${NODE_VSN}.x

apt-get install --no-install-recommends -y \
    sudo \
    dnsutils \
    python3-pip \
    python3-sphinx \
    python3-venv \
    libicu-dev \
    $spidermonkey_pkg \
    nodejs

npm install npm@latest -g --unsafe-perm

# Documentation theme
pip3 install sphinx_rtd_theme

# Elixir
wget -q https://github.com/elixir-lang/elixir/releases/download/${ELIXIR_VSN}/Precompiled.zip
unzip -qq Precompiled.zip -d /usr/local
rm Precompiled.zip

# FoundationDB client
wget -q https://github.com/apple/foundationdb/releases/download/${FDB_VSN}/foundationdb-clients_${FDB_VSN}-1_amd64.deb
dpkg -i ./foundationdb*deb
rm -rf ./foundationdb*deb

apt-get clean
