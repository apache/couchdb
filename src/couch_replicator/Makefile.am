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

couch_replicatorlibdir = $(localerlanglibdir)/couch_replicator-0.1
couch_replicatorincludedir = $(couch_replicatorlibdir)/include
couch_replicatorebindir = $(couch_replicatorlibdir)/ebin

couch_replicatorinclude_DATA = $(include_files)
couch_replicatorebin_DATA = $(compiled_files)

include_files = \
	src/couch_replicator_api_wrap.hrl \
	src/couch_replicator.hrl \
	src/couch_replicator_js_functions.hrl

source_files = \
	src/couch_replicator_api_wrap.erl \
	src/couch_replicator_httpc_pool.erl \
	src/couch_replicator_httpc.erl \
	src/couch_replicator_httpd.erl \
	src/couch_replicator_job_sup.erl \
	src/couch_replicator_notifier.erl \
	src/couch_replicator_manager.erl \
	src/couch_replicator_utils.erl \
	src/couch_replicator_worker.erl \
	src/couch_replicator.app.src \
	src/couch_replicator.erl

test_files = \
	test/couch_replicator_compact_tests.erl \
	test/couch_replicator_httpc_pool_tests.erl \
	test/couch_replicator_large_atts_tests.erl \
	test/couch_replicator_many_leaves_tests.erl \
	test/couch_replicator_missing_stubs_tests.erl \
	test/couch_replicator_modules_load_tests.erl \
	test/couch_replicator_use_checkpoints_tests.erl

compiled_files = \
	ebin/couch_replicator_api_wrap.beam \
	ebin/couch_replicator_httpc_pool.beam \
	ebin/couch_replicator_httpc.beam \
	ebin/couch_replicator_httpd.beam \
	ebin/couch_replicator_job_sup.beam \
	ebin/couch_replicator_notifier.beam \
	ebin/couch_replicator_manager.beam \
	ebin/couch_replicator_utils.beam \
	ebin/couch_replicator_worker.beam \
	ebin/couch_replicator.app \
	ebin/couch_replicator.beam

EXTRA_DIST = $(include_files) $(source_files) $(test_files)
CLEANFILES = $(compiled_files)

check:
if TESTS
	$(abs_top_builddir)/test/couchdb/run -v $(abs_top_srcdir)/src/couch_replicator/test
endif

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" \
	< $< > $@

ebin/%.beam: src/%.erl $(include_files)
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -I$(top_srcdir)/src/couchdb \
        -o ebin/ $(ERLC_FLAGS) ${TEST} $<;


