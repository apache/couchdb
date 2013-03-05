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

configebindir = $(localerlanglibdir)/config/ebin

configebin_DATA = $(compiled_files)

source_files = \
	src/config.app.src \
    src/config.erl \
	src/config_app.erl \
	src/config_listener.erl \
	src/config_sup.erl \
	src/config_util.erl \
	src/config_writer.erl

compiled_files = \
	ebin/config.app \
    ebin/config.beam \
	ebin/config_app.beam \
	ebin/config_listener.beam \
	ebin/config_sup.beam \
	ebin/config_util.beam \
	ebin/config_writer.beam

EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" < $< > $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -o ebin/ $(ERLC_FLAGS) $<
