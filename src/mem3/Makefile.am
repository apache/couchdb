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

mem3ebindir = $(localerlanglibdir)/mem3/ebin

mem3ebin_DATA = $(compiled_files)

source_files = \
	src/mem3.app.src \
    src/mem3.erl \
	src/mem3_app.erl \
	src/mem3_httpd.erl \
	src/mem3_nodes.erl \
	src/mem3_rep.erl \
	src/mem3_shards.erl \
	src/mem3_sup.erl \
	src/mem3_sync.erl \
	src/mem3_sync_event.erl \
	src/mem3_sync_nodes.erl \
	src/mem3_sync_security.erl \
	src/mem3_util.erl

compiled_files = \
	ebin/mem3.app \
    ebin/mem3.beam \
	ebin/mem3_app.beam \
	ebin/mem3_httpd.beam \
	ebin/mem3_shards.beam \
	ebin/mem3_sup.beam \
	ebin/mem3_sync.beam \
	ebin/mem3_sync_event.beam \
	ebin/mem3_sync_nodes.beam \
	ebin/mem3_sync_security.beam \
	ebin/mem3_util.beam

# Conflicts in couch_db.hrl prevent us from building these
# until after we merge the couch application
#	ebin/mem3_nodes.beam
#	ebin/mem3_rep.beam


EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" < $< > $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -o ebin/ $(ERLC_FLAGS) $<
