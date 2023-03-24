#!/bin/sh


CURL='curl -u a:a'
$CURL http://127.0.0.1:15984/asd -X DELETE
$CURL http://127.0.0.1:15984/asd?q=1 -X PUT
$CURL http://127.0.0.1:15984/asd/foo -X PUT -d '{"huhu":"0xb00bfart"}'

$CURL -X POST -Hcontent-type:application/json http://127.0.0.1:15984/asd/_compact
ls -lah dev/lib/node1/data/shards/00000000-ffffffff/
sleep 5
ls -lah dev/lib/node1/data/shards/00000000-ffffffff/

$CURL http://127.0.0.1:15984/asd/foo
ls -lah dev/lib/node1/data/shards/00000000-ffffffff/
