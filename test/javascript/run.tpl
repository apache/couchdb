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
BUILD_DIR=%abs_top_builddir%
SCRIPT_DIR=$SRC_DIR/share/www/script
JS_TEST_DIR=$SRC_DIR/test/javascript

COUCHJS=%abs_top_builddir%/src/couchdb/priv/couchjs
COUCH_URI_FILE=%localstaterundir%/couch.uri

# make check-js calls us with MAKE=$(MAKE) so BSDish `gmake` invocations
# will get passed on correctly. If $0 gets run manually, default to
# `make`
if [ -z "$MAKE" ]; then
    MAKE=make
fi

if [ "$#" -eq 0 ];
then
    TEST_SRC="$SCRIPT_DIR/test/*.js"
else
    TEST_SRC="$1"
    if [ ! -f $TEST_SRC ]; then
        TEST_SRC="$SCRIPT_DIR/test/$1"
        if [ ! -f $TEST_SRC ]; then
            TEST_SRC="$SCRIPT_DIR/test/$1.js"
            if [ ! -f $TEST_SRC ]; then
                echo "file $1 does not exist"
                exit 1
            fi
        fi
    fi
fi

# stop CouchDB on exit from various signals
abort() {
    trap - 0
    ./utils/run -d
    exit 2
}

# start CouchDB
if [ -z $COUCHDB_NO_START ]; then
    $MAKE dev
    trap 'abort' EXIT INT
	./utils/run -b -r 1 -n \
		-a $BUILD_DIR/etc/couchdb/default_dev.ini \
		-a $SRC_DIR/test/random_port.ini \
		-a $BUILD_DIR/etc/couchdb/local_dev.ini
	sleep 1 # give it a sec
fi

# start the tests
$COUCHJS -H -u $COUCH_URI_FILE \
	$SCRIPT_DIR/json2.js \
	$SCRIPT_DIR/sha1.js \
	$SCRIPT_DIR/oauth.js \
	$SCRIPT_DIR/couch.js \
	$SCRIPT_DIR/couch_test_runner.js \
	$SCRIPT_DIR/couch_tests.js \
	$TEST_SRC \
	$JS_TEST_DIR/couch_http.js \
	$JS_TEST_DIR/cli_runner.js

RESULT=$?

if [ -z $COUCHDB_NO_START ]; then
    # stop CouchDB
    ./utils/run -d
    trap - 0
fi

exit $RESULT
