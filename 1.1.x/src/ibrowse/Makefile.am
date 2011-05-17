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
	ibrowse.app.in \
    ibrowse.erl \
    ibrowse_app.erl \
    ibrowse_http_client.erl \
    ibrowse_lb.erl \
    ibrowse_lib.erl \
    ibrowse_sup.erl \
    ibrowse_test.erl

ibrowseebin_make_generated_file_list = \
	ibrowse.app \
    ibrowse.beam \
    ibrowse_app.beam \
    ibrowse_http_client.beam \
    ibrowse_lb.beam \
    ibrowse_lib.beam \
    ibrowse_sup.beam \
    ibrowse_test.beam

ibrowseebin_DATA = \
    $(ibrowseebin_make_generated_file_list)

EXTRA_DIST =  \
    $(ibrowse_file_collection) \
    ibrowse.hrl

CLEANFILES = \
    $(ibrowseebin_make_generated_file_list)

%.app: %.app.in
	cp $< $@

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
