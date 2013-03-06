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

rexiebindir = $(localerlanglibdir)/rexi/ebin

rexiebin_DATA = $(compiled_files)

source_files = \
	src/rexi.app.src \
    src/rexi.erl \
	src/rexi_app.erl \
	src/rexi_gov_manager.erl \
	src/rexi_governor.erl \
	src/rexi_monitor.erl \
	src/rexi_server.erl \
	src/rexi_sup.erl \
	src/rexi_utils.erl

compiled_files = \
	ebin/rexi.app \
    ebin/rexi.beam \
	ebin/rexi_app.beam \
	ebin/rexi_gov_manager.beam \
	ebin/rexi_governor.beam \
	ebin/rexi_monitor.beam \
	ebin/rexi_server.beam \
	ebin/rexi_sup.beam \
	ebin/rexi_utils.beam

EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" < $< > $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -o ebin/ $(ERLC_FLAGS) $<
