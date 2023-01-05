#!/bin/sh
REBAR=$1
DIALYZE_OPTS=$2

mkdir -p ./tmp

# run rebar xref, grep out rebar meta output (==> and WARN)
# pipe the rest into a file
$REBAR --keep-going --recursive xref $DIALYZE_OPTS | \
                                      grep -v '==>' | \
                                      grep -v 'WARN' | \
                                      grep -v hastings > ./tmp/xref-output.txt

# compare result against known allowed output
DIFF=`diff -u ./tmp/xref-output.txt ./test/fixtures/allowed-xref.txt`

# if the actual output differs from the allowed output
# print the difference and exit with 1
if [ -n "$DIFF" ]; then
  echo "$DIFF"
  exit 1
else
  exit 0
fi
