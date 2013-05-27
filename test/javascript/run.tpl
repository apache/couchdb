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

trap 'abort' EXIT INT

start() {
	./utils/run -b -r 0 -n \
		-a $BUILD_DIR/etc/couchdb/default_dev.ini \
		-a $SRC_DIR/test/random_port.ini \
		-a $BUILD_DIR/etc/couchdb/local_dev.ini 1>/dev/null
}

stop() {
    ./utils/run -d 1>/dev/null
}

restart() {
    stop
    start
}

abort() {
    trap - 0
    stop
    exit 2
}

process_response() {
    while read data
    do
        if [ $data = 'restart' ];
        then
            restart
        else
            echo "$data"
        fi
    done
}

run() {
    # start the tests
    /bin/echo -n "$1 ... "
    $COUCHJS -H -u $COUCH_URI_FILE \
        $SCRIPT_DIR/json2.js \
        $SCRIPT_DIR/sha1.js \
        $SCRIPT_DIR/oauth.js \
        $SCRIPT_DIR/couch.js \
        $SCRIPT_DIR/couch_test_runner.js \
        $JS_TEST_DIR/couch_http.js \
        $JS_TEST_DIR/test_setup.js \
        $1 \
        $JS_TEST_DIR/cli_runner.js | process_response

    if [ -z $RESULT ]; then
        RESULT=$?
    elif [ "$?" -eq 1 ]; then
        RESULT=$?
    fi

}

# start CouchDB
if [ -z $COUCHDB_NO_START ]; then
    $MAKE dev
    start
fi

echo "Running javascript tests ..."

if [ "$#" -eq 0 ];
then
    COUNTER=1
    FILES="$SCRIPT_DIR/test/*.js"
    FILE_COUNT=$(ls -l $FILES | wc -l)
    FILE_COUNT=$(expr $FILE_COUNT + 0)
    for TEST_SRC in $FILES
    do
        /bin/echo -n "$COUNTER/$FILE_COUNT "
        COUNTER=$(expr $COUNTER + 1)
        run $TEST_SRC
    done
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
            run $TEST_SRC
        fi
    fi
fi

stop
trap - 0
exit $RESULT
