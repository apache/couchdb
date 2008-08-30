#!/bin/sh -e

erl -noshell -pa ../src/couchdb -pa ../src/mochiweb -eval "runner:run()"
