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

etapebindir = $(localerlanglibdir)/etap/ebin

etap_file_collection = \
    etap.erl \
   	etap_application.erl \
    etap_can.erl \
    etap_exception.erl \
    etap_process.erl \
	etap_report.erl \
	etap_request.erl \
	etap_string.erl \
   	etap_web.erl

etapebin_make_generated_file_list = \
    etap.beam \
   	etap_application.beam \
    etap_can.beam \
    etap_exception.beam \
    etap_process.beam \
	etap_report.beam \
	etap_request.beam \
	etap_string.beam \
   	etap_web.beam

etapebin_DATA = $(etapebin_make_generated_file_list)

EXTRA_DIST =  $(etap_file_collection)

CLEANFILES = $(etapebin_make_generated_file_list)

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
