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

ibrowseebindir = $(localerlanglibdir)/ibrowse-2.2.0/ebin

ibrowse_file_collection = \
	src/ibrowse.app.in \
	src/ibrowse.erl \
	src/ibrowse_app.erl \
	src/ibrowse_http_client.erl \
	src/ibrowse_lb.erl \
	src/ibrowse_lib.erl \
	src/ibrowse_sup.erl \
	src/ibrowse_test.erl

ibrowseebin_make_generated_file_list = \
	ebin/ibrowse.app \
	ebin/ibrowse.beam \
	ebin/ibrowse_app.beam \
	ebin/ibrowse_http_client.beam \
	ebin/ibrowse_lb.beam \
	ebin/ibrowse_lib.beam \
	ebin/ibrowse_sup.beam \
	ebin/ibrowse_test.beam

ibrowseebin_DATA = \
    $(ibrowseebin_make_generated_file_list)

EXTRA_DIST =  \
    $(ibrowse_file_collection) \
    include/ibrowse.hrl

CLEANFILES = \
    $(ibrowseebin_make_generated_file_list)

ebin/%.app: src/%.app.in
	@mkdir -p ebin/
	cp $< $@

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -o ebin/ -I$(top_srcdir)/src $(ERLC_FLAGS) $<
