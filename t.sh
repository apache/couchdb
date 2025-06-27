#!/usr/bin/env bash

########################################################################
# TESTS

test_doc_with_att () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'

  check-file 0 'DOC BODY'             2
  check-file 0 'OBVIOUS ATTACHMENT'   1
}

test_doc_with_att_compact () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'
  compact 0

  check-files 'DOC BODY'            0 1
  check-files 'OBVIOUS ATTACHMENT'  0 1
}

test_doc_compact_add_att () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  compact 0
  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'

  check-files 'DOC BODY'            1 1
  check-files 'OBVIOUS ATTACHMENT'  1 0
}

test_doc_compact_add_att_compact () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  compact 0
  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'
  compact 0

  check-files 'DOC BODY'            0 2
  check-files 'OBVIOUS ATTACHMENT'  0 1
}

test_doc_compact_add_att_compact_0_compact_1 () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  compact 0
  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'
  compact 0
  compact 1

  check-files 'DOC BODY'            0 0 1
  check-files 'OBVIOUS ATTACHMENT'  0 0 1
}

test_doc_with_att_compact_update () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'
  compact 0
  update-doc 'the-doc' '.basic = "UPDATED BODY"'

  check-files 'DOC BODY'            0 1
  check-files 'OBVIOUS ATTACHMENT'  0 1
  check-files 'UPDATED BODY'        1 0
}

test_doc_with_att_update_compact () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'
  update-doc 'the-doc' '.basic = "UPDATED BODY"'
  compact 0

  check-files 'DOC BODY'            0 0
  check-files 'OBVIOUS ATTACHMENT'  0 1
  check-files 'UPDATED BODY'        0 1
}

test_doc_with_att_compact_update_compact () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'
  compact 0
  update-doc 'the-doc' '.basic = "UPDATED BODY"'
  compact 0

  check-files 'DOC BODY'            0 1
  check-files 'OBVIOUS ATTACHMENT'  0 1
  check-files 'UPDATED BODY'        0 1
}

test_doc_with_att_compact_update_compact_0_compact_1 () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'
  compact 0
  update-doc 'the-doc' '.basic = "UPDATED BODY"'
  compact 0
  compact 1

  check-files 'DOC BODY'            0 0 0
  check-files 'OBVIOUS ATTACHMENT'  0 0 1
  check-files 'UPDATED BODY'        0 0 1
}

test_doc_with_att_compact_0_compact_1_add_att () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'
  compact 0
  compact 1
  add-att 'the-doc' 'extra-att' 'EXTRA READ ALL ABOUT IT'

  check-files 'DOC BODY'            1 0 1 0
  check-files 'OBVIOUS ATTACHMENT'  0 0 1 0
  check-files 'EXTRA READ'          1 0 0 0

  compact 0

  check-files 'DOC BODY'            0 1 1 0
  check-files 'OBVIOUS ATTACHMENT'  0 0 1 0
  check-files 'EXTRA READ'          0 1 0 0

  compact 1

  check-files 'DOC BODY'            0 0 2 0
  check-files 'OBVIOUS ATTACHMENT'  0 0 1 0
  check-files 'EXTRA READ'          0 0 1 0

  compact 2

  check-files 'DOC BODY'            0 0 0 1
  check-files 'OBVIOUS ATTACHMENT'  0 0 0 1
  check-files 'EXTRA READ'          0 0 0 1
}

test_compact_max_gen_0 () {
  create-db 0

  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  add-att 'the-doc' 'the-att' $'VERY OBVIOUS ATTACHMENT DATA\n'

  check-files 'DOC BODY'            2
  check-files 'OBVIOUS ATTACHMENT'  1

  compact 0

  check-files 'DOC BODY'            1
  check-files 'OBVIOUS ATTACHMENT'  1

  cdb '/asd/the-doc'
  cdb '/asd/the-doc/the-att'
}

test_compact_max_gen_1 () {
  create-db 1

  create-doc 'the-doc' '{ "basic": "DOC BODY" }'
  add-att 'the-doc' 'the-att' $'VERY OBVIOUS ATTACHMENT DATA\n'

  check-files 'DOC BODY'            2 0
  check-files 'OBVIOUS ATTACHMENT'  1 0

  compact 0

  check-files 'DOC BODY'            0 1
  check-files 'OBVIOUS ATTACHMENT'  0 1

  cdb '/asd/the-doc'
  cdb '/asd/the-doc/the-att'
}

test_retention_in_gen_0 () {
  create-doc 'doc-1' '{ "the": ["first", "doc"] }'
  add-att 'doc-1' 'att-1' 'whatever'

  check-files 'first'     2
  check-files 'whatever'  1

  compact 0

  check-files 'first'     0 1
  check-files 'whatever'  0 1

  create-doc 'doc-2' '{ "second": ["doc", "here"] }'
  add-att 'doc-2' 'att-2' 'an ATTACHMENT'

  check-files 'first'     0 1 0
  check-files 'whatever'  0 1 0
  check-files 'second'    2 0 0
  check-files 'ATTACH'    1 0 0

  compact 1
  compact 1
  compact 1

  check-files 'first'     0 0 1
  check-files 'whatever'  0 0 1
  check-files 'second'    1 0 0
  check-files 'ATTACH'    1 0 0

  return

  cdb '/asd/doc-1' | jq
  cdb '/asd/doc-1/att-1' -i

  cdb '/asd/doc-2' | jq
  cdb '/asd/doc-2/att-2' -i
}

test_compact_final_generation () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'

  compact 0
  compact 1
  compact 2

  check-files 'DOC BODY'            0 0 0 1

  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'

  check-files 'DOC BODY'            1 0 0 1
  check-files 'OBVIOUS ATTACHMENT'  1 0 0 0

  compact 0

  check-files 'DOC BODY'            0 1 0 1
  check-files 'OBVIOUS ATTACHMENT'  0 1 0 0

  compact 1

  check-files 'DOC BODY'            0 0 1 1
  check-files 'OBVIOUS ATTACHMENT'  0 0 1 0

  compact 2

  check-files 'DOC BODY'            0 0 0 2
  check-files 'OBVIOUS ATTACHMENT'  0 0 0 1

  cdb '/asd/the-doc'

  compact 3

  check-files 'DOC BODY'            0 0 0 1
  check-files 'OBVIOUS ATTACHMENT'  0 0 0 1

  cdb '/asd/the-doc'
}

test_copy_doc_on_moving_attachment () {
  create-doc 'the-doc' '{ "basic": "DOC BODY" }'

  compact 0
  compact 1

  check-files 'DOC BODY'            0 0 1

  add-att 'the-doc' 'the-att' 'VERY OBVIOUS ATTACHMENT DATA'

  check-files 'DOC BODY'            1 0 1
  check-files 'OBVIOUS ATTACHMENT'  1 0 0

  compact 0

  check-files 'DOC BODY'            0 1 1
  check-files 'OBVIOUS ATTACHMENT'  0 1 0

  update-doc 'the-doc' '.basic = "UPDATED BODY"'

  check-files 'DOC BODY'            0 1 1
  check-files 'OBVIOUS ATTACHMENT'  0 1 0
  check-files 'UPDATED BODY'        1 0 0

  compact 1

  check-files 'DOC BODY'            0 0 1
  check-files 'OBVIOUS ATTACHMENT'  0 0 1
  check-files 'UPDATED BODY'        1 0 0

  compact 0

  check-files 'DOC BODY'            0 0 1
  check-files 'OBVIOUS ATTACHMENT'  0 0 1
  check-files 'UPDATED BODY'        0 1 0

  compact 2

  check-files 'DOC BODY'            0 0 0 0
  check-files 'OBVIOUS ATTACHMENT'  0 0 0 1
  check-files 'UPDATED BODY'        0 2 0 0
}

check_continued_readability () {
  create-doc 'doc-1' '{ "the": ["first", "doc"] }'
  compact 0

  let n=1
  while :; do
    sleep 1
    echo "n: $n"
    cdb '/asd/doc-1'
    let n=n+1
  done
}

check_sizing () {
  create-doc 'doc-1' '{ "the": ["first", "doc"] }'

  cdb '/asd' | jq '.sizes'

  for n in {0..1} ; do
    compact "$n"
    cdb '/asd' | jq '.sizes'
  done

  create-doc 'doc-2' '{ "the": ["second", "doc"] }'
}

check_smoosh_trigger () {
  create-doc '_design/the-views' '{ "views": { "by-n": { "map": "function (doc) { emit(doc.n, doc) }" } } }'

  for n in {1..1000} ; do
    create-doc "doc-$n" '{ "a": "doc", "n": '$n' }'
  done

  cdb '/asd' | jq '.sizes'
}

check_views () {
  cdb '/asd/_design/foo' -X PUT -d @- <<JSON
    {
      "views": {
        "by-title": {
          "map": "function (doc) { emit(doc.title) }"
        }
      }
    }
JSON

  create-doc '/asd/doc-1' '{ "title": "Hello world" }'
  cdb '/asd/_design/foo'

  tree $DATA
}

check_partition_overflow () {
  cdb '/_node/_local/_config/couchdb/max_partition_size' -X PUT -d '"10240"'

  cdb '/asd' -X DELETE
  cdb '/asd?partitioned=true&q=1&gen=2' -X PUT

  local docs="$(ruby -rjson -e 'puts JSON.dump((1..15).map { |i| { "_id" => "foo:" + i.to_s.rjust(4, "0"), "value" => "0".rjust(1024, "0") } })')"
  cdb '/asd/_bulk_docs' -iX POST -d '{ "w": 3, "docs": '"${docs}"' }'

  compact 0

  cdb '/asd/_bulk_docs?w=3' -iX POST -d '{ "w": 3, "docs": [{ "_id": "foo:bar" }, { "_id": "baz:bang" }]}'
}

########################################################################
# Support functions

CDB_SERVER='http://127.0.0.1:15984'
CDB_AUTH='a:a'
DATA='dev/lib/node1/data/shards/00000000-ffffffff'

db='asd'
suffix=''
rev=''

cdb () {
  local json=(-H 'Content-Type: application/json')
  cdb-req "$@" "${json[@]}"
}

cdb-req () {
  curl -su "$CDB_AUTH" "${CDB_SERVER}$1" "${@:2}"
}

create-db () {
  local gen="${1:-3}"
  cdb "/$db" -X DELETE
  rm -f $DATA/$db*
  cdb "/$db?q=1&gen=$gen" -X PUT
  suffix="$(cdb "/$db" | jq -r '.instance_start_time')"
}

create-doc () {
  local id="$1"
  local value="$2"
  rev="$(cdb "/$db/$id" -X PUT -d "$value" | jq -r '.rev')"
}

add-att () {
  local docid="$1"
  local attid="$2"
  local data="$3"

  local url="/$db/$docid/$attid?rev=$rev" 
  local ct='Content-Type: application/octet-stream'

  rev="$(cdb-req "$url" -X PUT -H "$ct" -d "$data" | jq -r '.rev')"
}

update-doc () {
  local id="$1"
  local expr="$2"

  cdb "/$db/$id" | jq "$expr" > tmp.json
  rev="$(cdb "/$db/$id" -X PUT -d @tmp.json | jq -r '.rev')"
  rm tmp.json
}

show-doc () {
  cdb "/$db/the-doc" | jq
}

compact () {
  local gen="$1"
  cdb "/$db/_compact?gen=$gen" -X POST
  sleep 1

  local compact_files="$(find dev/lib -name '*compact*' | wc -l)"

  if [[ $compact_files -gt 0 ]] ; then
    echo "[ERROR] compaction files are still present"
  fi
}

check-files () {
  local str="$1"
  local counts=("${@:2}")
  local count

  let gen=0

  for count in "${counts[@]}" ; do
    check-file "$gen" "$str" "$count"
    let gen=gen+1
  done
}

check-file () {
  local gen="$1"
  local str="$2"
  local n="$3"

  local file="$(gen-file "$gen")"
  local count="$(scan-file "$file" "$str" | jq length)"

  if [[ $count -ne $n ]] ; then
    echo "[ERROR] scanning $file for string <$str>: expected $n, found $count"
  fi
}

gen-file () {
  local gen="$1"

  if [[ $gen -eq 0 ]] ; then
    echo "$DATA/$db.$suffix.couch"
  else
    echo "$DATA/$db.$suffix.$gen.couch"
  fi
}

scan-file () {
  local file="$1"
  local str="$2"

  cat "$file" | ruby -e "p STDIN.read.force_encoding('binary').scan(/$str/)"
}

########################################################################

main () {
  local t="$1"

  if [[ -z "$t" ]] ; then
    all-tests | while read -r fn ; do
      run-test "$fn"
    done
  else
    run-test "$t"
  fi
}

all-tests () {
  declare -F | awk '{ print $3 }' | egrep '^test_'
}

run-test () {
  local fn="$1"

  echo
  echo "----[ $fn ]"
  create-db
  $fn
}

main "$@"
