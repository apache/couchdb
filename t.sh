#!/bin/sh


CURL='curl -u a:a'
COUCH="http://127.0.0.1:15984"
$CURL $COUCH/asd -X DELETE
$CURL $COUCH/asd?q=1 -X PUT
$CURL $COUCH/asd/foo -X PUT -d '{"huhu":"0xb00bfart"}'
$CURL $COUCH/asd/bar/zomg -X PUT -d '\0xbarf00d' -Hcontent-type:binary/octet-stream


$CURL -X POST -Hcontent-type:application/json $COUCH/asd/_compact
ls -lah dev/lib/node1/data/shards/00000000-ffffffff/
sleep 5
ls -lah dev/lib/node1/data/shards/00000000-ffffffff/

$CURL $COUCH/asd/foo
$CURL $COUCH/asd/bar/zomg
ls -lah dev/lib/node1/data/shards/00000000-ffffffff/

# TODO second compaction
# $CURL -X POST -Hcontent-type:application/json $COUCH/asd/_compact
# ls -lah dev/lib/node1/data/shards/00000000-ffffffff/
# sleep 5

# hexdump -C dev/lib/node1/data/shards/00000000-ffffffff/asd.*.couch