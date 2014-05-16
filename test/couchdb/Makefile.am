## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License. You may obtain a copy of
## the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
## License for the specific language governing permissions and limitations under
## the License.

noinst_SCRIPTS = run

all:
	mkdir -p ebin/
	mkdir -p temp/
	chmod +x run

eunit_files = \
    couch_util_tests.erl \
    couchdb_modules_load_tests.erl \
    include/couch_eunit.hrl

EXTRA_DIST = \
    run.in \
    eunit.ini \
    $(eunit_files)

clean-local:
	rm -rf ebin/
	rm -rf temp/
