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

SRC_DIR=%abs_top_srcdir%
SCRIPT_DIR=$SRC_DIR/share/www/script
JS_TEST_DIR=$SRC_DIR/test/javascript
JS_BENCH_DIR=$SRC_DIR/test/bench

COUCHJS=%abs_top_builddir%/src/couchdb/priv/couchjs

cat $SCRIPT_DIR/json2.js \
    $SCRIPT_DIR/couch.js \
    $JS_TEST_DIR/couch_http.js \
    $JS_BENCH_DIR/bench_marks.js \
    $JS_TEST_DIR/cli_runner.js \
    | $COUCHJS -

