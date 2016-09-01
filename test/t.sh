#!/bin/sh -ex
# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

HEADERS="-HContent-Type:application/json"
# show cluster state:
curl a:b@127.0.0.1:15986/_nodes/_all_docs

# Enable Cluster on node A
curl a:b@127.0.0.1:15984/_cluster_setup -d '{"action":"enable_cluster","username":"foo","password":"baz","bind_address":"0.0.0.0"}' $HEADERS

# Enable Cluster on node B
curl a:b@127.0.0.1:25984/_cluster_setup -d '{"action":"enable_cluster","username":"foo","password":"baz","bind_address":"0.0.0.0"}' $HEADERS

# Add node B on node A
curl a:b@127.0.0.1:15984/_cluster_setup -d '{"action":"add_node","username":"foo","password":"baz","host":"127.0.0.1","port":25984,"name":"node2"}' $HEADERS

# Show cluster state:
curl a:b@127.0.0.1:15986/_nodes/_all_docs

# Show db doesn’t exist on node A
curl a:b@127.0.0.1:15984/foo

# Show db doesn’t exist on node B
curl a:b@127.0.0.1:25984/foo

# Create database (on node A)
curl -X PUT a:b@127.0.0.1:15984/foo

# Show db does exist on node A
curl a:b@127.0.0.1:15984/foo

# Show db does exist on node B
curl a:b@127.0.0.1:25984/foo

# Finish cluster
curl a:b@127.0.0.1:15984/_cluster_setup -d '{"action":"finish_cluster"}' $HEADERS

# Show system dbs exist on node A
curl a:b@127.0.0.1:15984/_users
curl a:b@127.0.0.1:15984/_replicator
curl a:b@127.0.0.1:15984/_metadata
curl a:b@127.0.0.1:15984/_global_changes

# Show system dbs exist on node B
curl a:b@127.0.0.1:25984/_users
curl a:b@127.0.0.1:25984/_replicator
curl a:b@127.0.0.1:25984/_metadata
curl a:b@127.0.0.1:25984/_global_changes

echo "YAY ALL GOOD"
