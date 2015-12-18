# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

FROM debian:jessie

MAINTAINER Clemens Stolle klaemo@apache.org

ENV COUCHDB_VERSION master

RUN groupadd -r couchdb && useradd -d /usr/src/couchdb -g couchdb couchdb

# download dependencies
RUN apt-get update -y -qq && apt-get install -y --no-install-recommends \
    apt-transport-https \
    build-essential \
    ca-certificates \
    curl \
    erlang-dev \
    erlang-nox \
    git \
    haproxy \
    libicu-dev \
    libmozjs185-dev \
    openssl \
    python \
 && curl -s https://deb.nodesource.com/gpgkey/nodesource.gpg.key | apt-key add - \
 && echo 'deb https://deb.nodesource.com/node_4.x jessie main' > /etc/apt/sources.list.d/nodesource.list \
 && echo 'deb-src https://deb.nodesource.com/node_4.x jessie main' >> /etc/apt/sources.list.d/nodesource.list \
 && apt-get update -y -qq && apt-get install -y nodejs \
 && npm install -g grunt-cli

COPY . /usr/src/couchdb

WORKDIR /usr/src/couchdb
RUN ./configure --disable-docs && make couch

# permissions
RUN chmod +x /usr/src/couchdb/dev/run && chown -R couchdb:couchdb /usr/src/couchdb

USER couchdb
EXPOSE 5984 15984 25984 35984 15986 25986 35986

ENTRYPOINT ["/usr/src/couchdb/dev/run"]
