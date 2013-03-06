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

chttpdebindir = $(localerlanglibdir)/chttpd/ebin

chttpdebin_DATA = $(compiled_files)

source_files = \
	src/chttpd.app.src \
	src/chttpd.erl \
	src/chttpd_app.erl \
	src/chttpd_config_listener.erl \
	src/chttpd_db.erl \
	src/chttpd_external.erl \
	src/chttpd_misc.erl \
	src/chttpd_rewrite.erl \
	src/chttpd_show.erl \
	src/chttpd_sup.erl \
	src/chttpd_view.erl

compiled_files = \
	ebin/chttpd.app \
	ebin/chttpd.beam \
	ebin/chttpd_app.beam \
	ebin/chttpd_config_listener.beam \
	ebin/chttpd_db.beam \
	ebin/chttpd_misc.beam \
	ebin/chttpd_rewrite.beam \
	ebin/chttpd_show.beam \
	ebin/chttpd_sup.beam

# Conflicts in couch_db.hrl prevent us from building these
# until after we merge the couch application
#	ebin/chttpd_external.beam
#	ebin/chttpd_view.beam


EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" < $< > $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -o ebin/ $(ERLC_FLAGS) $<
