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

mochiwebebindir = $(localerlanglibdir)/mochiweb-r82/ebin

mochiweb_file_collection = \
    mochifmt.erl \
    mochifmt_records.erl \
    mochifmt_std.erl \
    mochihex.erl \
    mochijson.erl \
    mochijson2.erl \
    mochinum.erl \
    mochiweb.erl \
    mochiweb_app.erl \
    mochiweb_charref.erl \
    mochiweb_cookies.erl \
    mochiweb_echo.erl \
    mochiweb_headers.erl \
    mochiweb_html.erl \
    mochiweb_http.erl \
    mochiweb_multipart.erl \
    mochiweb_request.erl \
    mochiweb_response.erl \
    mochiweb_skel.erl \
    mochiweb_socket_server.erl \
    mochiweb_sup.erl \
    mochiweb_util.erl \
    reloader.erl

mochiwebebin_static_file = mochiweb.app

mochiwebebin_make_generated_file_list = \
    mochifmt.beam \
    mochifmt_records.beam \
    mochifmt_std.beam \
    mochihex.beam \
    mochijson.beam \
    mochijson2.beam \
    mochinum.beam \
    mochiweb.beam \
    mochiweb_app.beam \
    mochiweb_charref.beam \
    mochiweb_cookies.beam \
    mochiweb_echo.beam \
    mochiweb_headers.beam \
    mochiweb_html.beam \
    mochiweb_http.beam \
    mochiweb_multipart.beam \
    mochiweb_request.beam \
    mochiweb_response.beam \
    mochiweb_skel.beam \
    mochiweb_socket_server.beam \
    mochiweb_sup.beam \
    mochiweb_util.beam \
    reloader.beam

mochiwebebin_DATA = \
    $(mochiwebebin_static_file) \
    $(mochiwebebin_make_generated_file_list)

EXTRA_DIST =  \
    $(mochiweb_file_collection) \
    $(mochiwebebin_static_file)

CLEANFILES = \
    $(mochiwebebin_make_generated_file_list)

%.beam: %.erl
	$(ERLC) $<
