#!/bin/sh -x
DB=http://a:a@127.0.0.1:15984 #
XDB=http://x:x@127.0.0.1:15984
YDB=http://y:y@127.0.0.1:15984

curl -sX PUT $DB/_users/org.couchdb.user:x -d @user.json > /dev/null #
curl -sX PUT $DB/_users/org.couchdb.user:y -d @user2.json > /dev/null #

# curl -sX DELETE $DB/_global_changes
curl -sX DELETE $DB/db
curl -sX PUT $DB/db?q=1'&access=true'

curl -sX DELETE $DB/db2
curl -sX PUT $DB/db2

# ##############
curl -sX PUT $DB/db/a -d '{"a":1,"_access":["x"]}'
curl -s $DB/db/a

curl -sX PUT $DB/db2/a -d '{"a":1,"_access":["x"]}'
curl -s $DB/db2/a


curl -s $XDB/db/a
curl -s $YDB/db/a
#
curl -sX PUT $DB/db/b -d '{"b":2,"_access":["x"]}'
curl -sX PUT $DB/db/c -d '{"c":3,"_access":["y"]}'
curl -X PUT $XDB/db/c?rev="1-0865d643568aa9be6bcdc15d88b25912" -d '{"c":6,"_access":["y"]}'
#
curl -sX PUT $DB/db/d -d '{"d":4,"_access":["y"]}'
#
curl -sX DELETE $DB/db/a?rev="1-967a00dff5e02add41819138abb3284d"

# echo
# echo "setup done"
# echo
#
# #
curl -s $DB/db/_all_docs?include_docs=true
curl -s $XDB/db/_all_docs?include_docs=true
curl -s $YDB/db/_all_docs?include_docs=true
# #
curl -s $DB/db/_changes?include_docs=true
curl -s $XDB/db/_changes?include_docs=true
curl -s $YDB/db/_changes?include_docs=true
# # #
curl -s $DB/db/a
curl -s $DB/db/b
curl -s $DB/db/c
curl -s $DB/db/d

curl -s $XDB/db/a
curl -s $XDB/db/b
curl -s $XDB/db/c
curl -s $XDB/db/d

curl -s $YDB/db/a
curl -s $YDB/db/b
curl -s $YDB/db/c
curl -s $YDB/db/d
#
curl -sX PUT $XDB/db/b?rev="1-967a00dff5e02add41819138abb3284d" -d '{"b":5,"_access":["x"]}'
curl -sX PUT $XDB/db/c?rev="1-0865d643568aa9be6bcdc15d88b25912" -d '{"c":6,"_access":["y"]}'
curl -sX PUT $XDB/db/d?rev="1-87adddda059e643409c43bea87c37bfe" -d '{"d":7,"_access":["y"]}'

curl -sX PUT $YDB/db/b?rev="1-809cfddb59a4f02dc1009785fad978b4" -d '{"b":5,"_access":["x"]}'
curl -sX PUT $YDB/db/c?rev="1-0865d643568aa9be6bcdc15d88b25912" -d '{"c":6,"_access":["y"]}'
curl -sX PUT $YDB/db/d?rev="1-87adddda059e643409c43bea87c37bfe" -d '{"d":7,"_access":["y"]}'

curl -sX DELETE $XDB/db/b?rev="2-384262a5168d1187dff405530046666"
curl -sX DELETE $XDB/db/c?rev="2-4cdae6f0a7539ea92dc79f3d3b0379b"
curl -sX DELETE $XDB/db/d?rev="2-868c18641965cfae831be1114b49a66b"

curl -sX DELETE $YDB/db/b?rev="2-384262a5168d1187dff405530046666"
curl -sX DELETE $YDB/db/c?rev="2-4cdae6f0a7539ea92dc79f3d3b0379b"
curl -sX DELETE $YDB/db/d?rev="2-868c18641965cfae831be1114b49a66b"
