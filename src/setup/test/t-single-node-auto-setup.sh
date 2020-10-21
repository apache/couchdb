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

# Show cluster state:
curl a:b@127.0.0.1:15986/_nodes/_all_docs
curl a:b@127.0.0.1:15984/_all_dbs
curl a:b@127.0.0.1:15984/_cluster_setup

# Change the check
curl -g 'a:b@127.0.0.1:15984/_cluster_setup?ensure_dbs_exist=["_replicator","_users"]'

echo "YAY ALL GOOD"
