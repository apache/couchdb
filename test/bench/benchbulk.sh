#!/bin/sh -e
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
#

# usage: time benchbulk.sh
# it takes about 30 seconds to run on my old MacBook with bulksize 1000

BULKSIZE=100
DOCSIZE=10
INSERTS=10
ROUNDS=10
DBURL="http://127.0.0.1:5984/benchbulk"
POSTURL="$DBURL/_bulk_docs"

function make_bulk_docs() {
  ROW=0
  SIZE=$(($1-1))
  START=$2
  BODYSIZE=$3  
  
  BODY=$(printf "%0${BODYSIZE}d")

  echo '{"docs":['
  while [ $ROW -lt $SIZE ]; do
    printf '{"_id":"%020d", "body":"'$BODY'"},' $(($ROW + $START))
    let ROW=ROW+1
  done
  printf '{"_id":"%020d", "body":"'$BODY'"}' $(($ROW + $START))
  echo ']}'
}

echo "Making $INSERTS bulk inserts of $BULKSIZE docs each"

echo "Attempt to delete db at $DBURL"
curl -X DELETE $DBURL -w\\n

echo "Attempt to create db at $DBURL"
curl -X PUT $DBURL -w\\n

echo "Running $ROUNDS rounds of $INSERTS concurrent inserts to $POSTURL"
RUN=0
while [ $RUN -lt $ROUNDS ]; do

  POSTS=0
  while [ $POSTS -lt $INSERTS ]; do
    STARTKEY=$[ POSTS * BULKSIZE + RUN * BULKSIZE * INSERTS ]
    echo "startkey $STARTKEY bulksize $BULKSIZE"
    DOCS=$(make_bulk_docs $BULKSIZE $STARTKEY $DOCSIZE)
    # echo $DOCS
    echo $DOCS | curl -T - -H Content-Type:application/json -X POST $POSTURL -w%{http_code}\ %{time_total}\ sec\\n >/dev/null 2>&1 &
    let POSTS=POSTS+1
  done

  echo "waiting"
  wait
  let RUN=RUN+1
done

curl $DBURL -w\\n
