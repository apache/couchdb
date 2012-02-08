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

if USE_NATIVE_MOCHIJSON
MOCHIJSON_ERLC_FLAGS=+native
endif

mochiwebebindir = $(localerlanglibdir)/mochiweb-1.4.1/ebin

mochiweb_file_collection = \
	mochifmt.erl \
	mochifmt_records.erl \
	mochifmt_std.erl \
	mochiglobal.erl \
	mochihex.erl \
	mochijson.erl \
	mochijson2.erl \
	mochilists.erl \
	mochilogfile2.erl \
	mochinum.erl \
	mochitemp.erl \
	mochiutf8.erl \
	mochiweb.app.in \
	mochiweb.app.src \
	mochiweb.erl \
	mochiweb_acceptor.erl \
	mochiweb_app.erl \
	mochiweb_charref.erl \
	mochiweb_cookies.erl \
	mochiweb_cover.erl \
	mochiweb_echo.erl \
	mochiweb_headers.erl \
	mochiweb_html.erl \
	mochiweb_http.erl \
	mochiweb_io.erl \
	mochiweb_mime.erl \
	mochiweb_multipart.erl \
	mochiweb_request.erl \
	mochiweb_request_tests.erl \
	mochiweb_response.erl \
	mochiweb_skel.erl \
	mochiweb_socket.erl \
	mochiweb_socket_server.erl \
	mochiweb_sup.erl \
	mochiweb_util.erl \
	reloader.erl

mochiwebebin_make_generated_file_list = \
	mochifmt.beam \
	mochifmt_records.beam \
	mochifmt_std.beam \
	mochiglobal.beam \
	mochihex.beam \
	mochijson.beam \
	mochijson2.beam \
	mochilists.beam \
	mochilogfile2.beam \
	mochinum.beam \
	mochitemp.beam \
	mochiutf8.beam \
	mochiweb.app \
	mochiweb.beam \
	mochiweb_acceptor.beam \
	mochiweb_app.beam \
	mochiweb_charref.beam \
	mochiweb_cookies.beam \
	mochiweb_cover.beam \
	mochiweb_echo.beam \
	mochiweb_headers.beam \
	mochiweb_html.beam \
	mochiweb_http.beam \
	mochiweb_io.beam \
	mochiweb_mime.beam \
	mochiweb_multipart.beam \
	mochiweb_request.beam \
	mochiweb_response.beam \
	mochiweb_skel.beam \
	mochiweb_socket.beam \
	mochiweb_socket_server.beam \
	mochiweb_sup.beam \
	mochiweb_util.beam \
	reloader.beam

mochiwebebin_DATA = \
    $(mochiwebebin_make_generated_file_list)

EXTRA_DIST =  \
    $(mochiweb_file_collection) \
    internal.hrl

CLEANFILES = \
    $(mochiwebebin_make_generated_file_list)

%.app: %.app.in
	cp $< $@

mochijson2.beam: mochijson2.erl
	$(ERLC) $(ERLC_FLAGS) $(MOCHIJSON_ERLC_FLAGS) $<

%.beam: %.erl
	$(ERLC) $(ERLC_FLAGS) $<
