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

couch_dbupdateslibdir = $(localerlanglibdir)/couch_dbupdates-0.1
couch_dbupdatesebindir = $(couch_dbupdateslibdir)/ebin

couch_dbupdatesebin_DATA = $(compiled_files)

EXTRA_DIST = $(source_files)
CLEANFILES = $(compiled_files)

source_files = \
    src/couch_dbupdates.erl \
    src/couch_dbupdates.app.src \
    src/couch_dbupdates_httpd.erl

compiled_files = \
    ebin/couch_dbupdates.beam \
    ebin/couch_dbupdates_httpd.beam

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -I$(top_srcdir)/src/couchdb -o ebin/ $(ERLC_FLAGS) ${TEST} $<;

