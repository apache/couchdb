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

ARG ERLANG_VSN
FROM erlang:${ERLANG_VSN}

ARG ERLANG_VSN
ARG FDB_VSN
ARG NODE_VSN

# Create Jenkins user and group for usage in CI
RUN groupadd --gid 910 jenkins; \
    useradd --uid 910 --gid jenkins --create-home jenkins

COPY install_dependencies.sh /tmp/

RUN ERLANG_VSN=${ERLANG_VSN} FDB_VSN=${FDB_VSN} NODE_VSN=${NODE_VSN} /tmp/install_dependencies.sh

# Allow Jenkins to sudo
RUN echo "jenkins ALL=(ALL) NOPASSWD: ALL" > /etc/sudoers.d/jenkins

COPY create_cluster_file.bash /usr/local/bin/

CMD sleep infinity
