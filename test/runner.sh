#!/bin/sh -e

erlc runner.erl

erl -noshell -pa ../src/couchdb -pa ../src/mochiweb -eval "runner:run()"

cat ../share/www/script/couch.js ../share/www/script/couch_test_runner.js ../share/www/script/couch_tests.js ../share/www/script/test/* test.js  | ../src/couchdb/couchjs -
