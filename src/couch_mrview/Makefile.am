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

couch_mrviewlibdir = $(localerlanglibdir)/couch_mrview-0.1
couch_mrviewincludedir = $(couch_mrviewlibdir)/include
couch_mrviewebindir = $(couch_mrviewlibdir)/ebin

couch_mrviewinclude_DATA = $(include_files)
couch_mrviewebin_DATA = $(compiled_files)

include_files = \
    include/couch_mrview.hrl

source_files = \
    src/couch_mrview.app.src \
    src/couch_mrview.erl \
    src/couch_mrview_cleanup.erl \
    src/couch_mrview_compactor.erl \
    src/couch_mrview_http.erl \
    src/couch_mrview_index.erl \
    src/couch_mrview_show.erl \
    src/couch_mrview_test_util.erl \
    src/couch_mrview_updater.erl \
    src/couch_mrview_util.erl

test_files = \
    test/couch_mrview_all_docs_tests.erl \
    test/couch_mrview_collation_tests.erl \
    test/couch_mrview_compact_tests.erl \
    test/couch_mrview_index_info_tests.erl \
    test/couch_mrview_map_views_tests.erl \
    test/couch_mrview_modules_load_tests.erl \
    test/couch_mrview_red_views_tests.erl

compiled_files = \
    ebin/couch_mrview.app \
    ebin/couch_mrview.beam \
    ebin/couch_mrview_cleanup.beam \
    ebin/couch_mrview_compactor.beam \
    ebin/couch_mrview_http.beam \
    ebin/couch_mrview_index.beam \
    ebin/couch_mrview_show.beam \
    ebin/couch_mrview_test_util.beam \
    ebin/couch_mrview_updater.beam \
    ebin/couch_mrview_util.beam

EXTRA_DIST = $(include_files) $(source_files) $(test_files)
CLEANFILES = $(compiled_files)

check:
if TESTS
	$(abs_top_builddir)/test/couchdb/run -v $(abs_top_srcdir)/src/couch_mrview/test
endif

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" \
	< $< > $@

ebin/%.beam: src/%.erl $(include_files)
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -I$(top_srcdir)/src/couchdb \
        -o ebin/ $(ERLC_FLAGS) ${TEST} $<;

