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
curl a:b@127.0.0.1:15984/_cluster_setup

# Enable Cluster on single node
curl a:b@127.0.0.1:15984/_cluster_setup -d '{"action":"enable_single_node","username":"foo","password":"baz","bind_address":"127.0.0.1"}' $HEADERS

# Show cluster state:
curl a:b@127.0.0.1:15986/_nodes/_all_docs
curl a:b@127.0.0.1:15984/_all_dbs
curl a:b@127.0.0.1:15984/_cluster_setup

# Delete a database
curl -X DELETE a:b@127.0.0.1:15984/_global_changes

# Should show single_node_disabled
curl a:b@127.0.0.1:15984/_cluster_setup

# Change the check
curl -g 'a:b@127.0.0.1:15984/_cluster_setup?ensure_dbs_exist=["_replicator","_users"]'

# delete all the things
curl -X DELETE a:b@127.0.0.1:15984/_replicator
curl -X DELETE a:b@127.0.0.1:15984/_users

# setup only creating _users
curl -g a:b@127.0.0.1:15984/_cluster_setup -d '{"action":"enable_single_node","username":"foo","password":"baz","bind_address":"127.0.0.1","ensure_dbs_exist":["_users"]}' $HEADERS

# check it
curl -g 'a:b@127.0.0.1:15984/_cluster_setup?ensure_dbs_exist=["_users"]'

echo "YAY ALL GOOD"
