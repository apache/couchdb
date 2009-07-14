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

erlc runner.erl

erl -noshell -pa ../src/couchdb -pa ../src/mochiweb -eval "runner:run()"

cat ../share/www/script/couch.js \
    ../share/www/script/couch_test_runner.js \
    ../share/www/script/couch_tests.js \
    ../share/www/script/test/* test.js \
    | ../src/couchdb/couchjs -
