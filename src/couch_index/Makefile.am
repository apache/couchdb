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

couch_indexlibdir = $(localerlanglibdir)/couch_index-0.1
couch_indexebindir = $(couch_indexlibdir)/ebin

couch_indexebin_DATA = $(compiled_files)

EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

source_files = \
    src/couch_index.erl \
    src/couch_index_api.erl \
    src/couch_index.app.src \
    src/couch_index_compactor.erl \
    src/couch_index_server.erl \
    src/couch_index_updater.erl \
    src/couch_index_util.erl

compiled_files = \
    ebin/couch_index.beam \
    ebin/couch_index_compactor.beam \
    ebin/couch_index_server.beam \
    ebin/couch_index_updater.beam \
    ebin/couch_index_util.beam

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -I$(top_srcdir)/src/couchdb -o ebin/ $(ERLC_FLAGS) ${TEST} $<;

