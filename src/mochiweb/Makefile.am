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
	src/mochifmt.erl \
	src/mochifmt_records.erl \
	src/mochifmt_std.erl \
	src/mochiglobal.erl \
	src/mochihex.erl \
	src/mochijson.erl \
	src/mochijson2.erl \
	src/mochilists.erl \
	src/mochilogfile2.erl \
	src/mochinum.erl \
	src/mochitemp.erl \
	src/mochiutf8.erl \
	src/mochiweb.app.in \
	src/mochiweb.app.src \
	src/mochiweb.erl \
	src/mochiweb_acceptor.erl \
	src/mochiweb_app.erl \
	src/mochiweb_charref.erl \
	src/mochiweb_cookies.erl \
	src/mochiweb_cover.erl \
	src/mochiweb_echo.erl \
	src/mochiweb_headers.erl \
	src/mochiweb_html.erl \
	src/mochiweb_http.erl \
	src/mochiweb_io.erl \
	src/mochiweb_mime.erl \
	src/mochiweb_multipart.erl \
	src/mochiweb_request.erl \
	src/mochiweb_request_tests.erl \
	src/mochiweb_response.erl \
	src/mochiweb_skel.erl \
	src/mochiweb_socket.erl \
	src/mochiweb_socket_server.erl \
	src/mochiweb_sup.erl \
	src/mochiweb_util.erl \
	src/reloader.erl

mochiwebebin_make_generated_file_list = \
	ebin/mochifmt.beam \
	ebin/mochifmt_records.beam \
	ebin/mochifmt_std.beam \
	ebin/mochiglobal.beam \
	ebin/mochihex.beam \
	ebin/mochijson.beam \
	ebin/mochijson2.beam \
	ebin/mochilists.beam \
	ebin/mochilogfile2.beam \
	ebin/mochinum.beam \
	ebin/mochitemp.beam \
	ebin/mochiutf8.beam \
	ebin/mochiweb.app \
	ebin/mochiweb.beam \
	ebin/mochiweb_acceptor.beam \
	ebin/mochiweb_app.beam \
	ebin/mochiweb_charref.beam \
	ebin/mochiweb_cookies.beam \
	ebin/mochiweb_cover.beam \
	ebin/mochiweb_echo.beam \
	ebin/mochiweb_headers.beam \
	ebin/mochiweb_html.beam \
	ebin/mochiweb_http.beam \
	ebin/mochiweb_io.beam \
	ebin/mochiweb_mime.beam \
	ebin/mochiweb_multipart.beam \
	ebin/mochiweb_request.beam \
	ebin/mochiweb_response.beam \
	ebin/mochiweb_skel.beam \
	ebin/mochiweb_socket.beam \
	ebin/mochiweb_socket_server.beam \
	ebin/mochiweb_sup.beam \
	ebin/mochiweb_util.beam \
	ebin/reloader.beam

mochiwebebin_DATA = \
    $(mochiwebebin_make_generated_file_list)

EXTRA_DIST =  \
    $(mochiweb_file_collection) \
    src/internal.hrl

CLEANFILES = \
    $(mochiwebebin_make_generated_file_list)

ebin/%.app: src/%.app.in
	@mkdir -p ebin/
	cp $< $@

ebin/mochijson2.beam: src/mochijson2.erl
	@mkdir -p ebin/
	$(ERLC) -o ebin/ $(ERLC_FLAGS) $(MOCHIJSON_ERLC_FLAGS) $<

ebin/%.beam: src/%.erl
	@mkdir -p ebin/
	$(ERLC) -o ebin/ $(ERLC_FLAGS) $<
