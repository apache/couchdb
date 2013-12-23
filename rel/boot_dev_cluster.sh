#!/bin/bash

# Make log directory
mkdir -p ./rel/logs/

HAPROXY=`which haproxy`

# Start each node
./rel/dev1/bin/couchdb > ./rel/logs/couchdb1.log 2>&1 &
DB1_PID=$!

./rel/dev2/bin/couchdb > ./rel/logs/couchdb2.log 2>&1 &
DB2_PID=$!

./rel/dev3/bin/couchdb > ./rel/logs/couchdb3.log 2>&1 &
DB3_PID=$!

$HAPROXY -f rel/haproxy.cfg > ./rel/logs/haproxy.log 2>&1 &
HP_PID=$!

sleep 2

# Connect the cluster
curl localhost:15986/nodes/dev2@127.0.0.1 -X PUT -d '{}'
curl localhost:15986/nodes/dev3@127.0.0.1 -X PUT -d '{}'

trap "kill $DB1_PID $DB2_PID $DB3_PID $HP_PID" SIGINT SIGTERM SIGHUP

wait
