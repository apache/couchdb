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

couch_pluginsebindir = $(couch_pluginslibdir)/ebin

couch_pluginsebin_DATA = $(compiled_files)


source_files = \
    src/couch_plugins.app.src \
    src/couch_plugins.erl \
    src/couch_plugins_httpd.erl

compiled_files = \
    ebin/couch_plugins.app \
    ebin/couch_plugins.beam \
    ebin/couch_plugins_httpd.beam

EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

ebin/%.app: src/%.app.src
	@mkdir -p ebin/
	sed -e "s|%version%|@version@|g" \
	< $< > $@

ebin/%.beam: src/%.erl $(include_files)
	@mkdir -p ebin/
	$(ERLC) -Wall -I$(top_srcdir)/src -I$(top_srcdir)/src/couchdb \
		-o ebin/ $(ERLC_FLAGS) ${TEST} $<;
