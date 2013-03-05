## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License.  You may obtain a copy
## of the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
## License for the specific language governing permissions and limitations under
## the License.

fabricebindir = $(localerlanglibdir)/fabric/ebin

fabricebin_DATA = $(compiled_files)

source_files = \
    src/fabric.app.src \
    src/fabric.erl \
    src/fabric_db_create.erl \
    src/fabric_db_delete.erl \
    src/fabric_db_doc_count.erl \
    src/fabric_db_info.erl \
    src/fabric_db_meta.erl \
    src/fabric_db_update_listener.erl \
    src/fabric_dict.erl \
    src/fabric_doc_attachments.erl \
    src/fabric_doc_missing_revs.erl \
    src/fabric_doc_open.erl \
    src/fabric_doc_open_revs.erl \
    src/fabric_doc_update.erl \
    src/fabric_group_info.erl \
    src/fabric_rpc.erl \
    src/fabric_util.erl \
    src/fabric_view.erl \
    src/fabric_view_all_docs.erl \
    src/fabric_view_changes.erl \
    src/fabric_view_map.erl \
    src/fabric_view_reduce.erl

compiled_files = \
    ebin/fabric.app \
    ebin/fabric.beam \
    ebin/fabric_db_create.beam \
    ebin/fabric_db_delete.beam \
    ebin/fabric_db_doc_count.beam \
    ebin/fabric_db_info.beam \
    ebin/fabric_db_meta.beam \
    ebin/fabric_db_update_listener.beam \
    ebin/fabric_dict.beam \
    ebin/fabric_doc_attachments.beam \
    ebin/fabric_doc_missing_revs.beam \
    ebin/fabric_doc_open.beam \
    ebin/fabric_doc_open_revs.beam \
    ebin/fabric_doc_update.beam \
    ebin/fabric_group_info.beam \
    ebin/fabric_util.beam

# Conflicts in couch_db.hrl prevent us from building these
# until after we merge the couch application
#   ebin/fabric_rpc.beam
#	ebin/fabric_view.beam
#	ebin/fabric_view_all_docs.beam
#	ebin/fabric_view_changes.beam
#	ebin/fabric_view_map.beam
#	ebin/fabric_view_reduce.beam

EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" < $< > $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -o ebin/ $(ERLC_FLAGS) $<
