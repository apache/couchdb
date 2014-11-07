#!/bin/sh -ex

HEADERS="-HContent-Type:application/json"
# show cluster state:
curl a:b@127.0.0.1:15986/nodes/_all_docs

# Enable Cluster on node A
curl a:b@127.0.0.1:15984/_cluster_setup -d '{"action":"enable_cluster","username":"foo","password":"baz","bind_address":"0.0.0.0"}' $HEADERS

# Enable Cluster on node B
curl a:b@127.0.0.1:25984/_cluster_setup -d '{"action":"enable_cluster","username":"foo","password":"baz","bind_address":"0.0.0.0"}' $HEADERS

# Add node B on node A
curl a:b@127.0.0.1:15984/_cluster_setup -d '{"action":"add_node","username":"foo","password":"baz","host":"127.0.0.1","port":25984}' $HEADERS

# Show cluster state:
curl a:b@127.0.0.1:15986/nodes/_all_docs

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

echo "YAY ALL GOOD"