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

datarootdir = @prefix@/share

couchinetsebindir = $(erlanglibdir)/couch_inets-4.7.5/ebin
couchinetsincludedir = $(erlanglibdir)/couch_inets-4.7.5/include

couchinets_file_collection = \
    ftp.erl \
    ftp_progress.erl \
    ftp_response.erl \
    ftp_sup.erl \
    http_base_64.erl \
    httpc_handler.erl \
    http_chunk.erl \
    httpc_manager.erl \
    http_cookie.erl \
    httpc_request.erl \
    httpc_response.erl \
    httpc_sup.erl \
    httpd_acceptor.erl \
    httpd_acceptor_sup.erl \
    httpd_cgi.erl \
    httpd_conf.erl \
    httpd.erl \
    httpd_esi.erl \
    httpd_example.erl \
    httpd_instance_sup.erl \
    httpd_manager.erl \
    httpd_misc_sup.erl \
    httpd_request.erl \
    httpd_request_handler.erl \
    httpd_response.erl \
    httpd_script_env.erl \
    httpd_socket.erl \
    httpd_sup.erl \
    httpd_util.erl \
    http.erl \
    http_request.erl \
    http_response.erl \
    http_transport.erl \
    http_uri.erl \
    http_util.erl \
    inets_app.erl \
    inets.erl \
    inets_sup.erl \
    mod_actions.erl \
    mod_alias.erl \
    mod_auth_dets.erl \
    mod_auth.erl \
    mod_auth_mnesia.erl \
    mod_auth_plain.erl \
    mod_auth_server.erl \
    mod_browser.erl \
    mod_cgi.erl \
    mod_dir.erl \
    mod_disk_log.erl \
    mod_esi.erl \
    mod_get.erl \
    mod_head.erl \
    mod_htaccess.erl \
    mod_include.erl \
    mod_log.erl \
    mod_range.erl \
    mod_responsecontrol.erl \
    mod_security.erl \
    mod_security_server.erl \
    mod_trace.erl \
    tftp_binary.erl \
    tftp_engine.erl \
    tftp.erl \
    tftp_file.erl \
    tftp_lib.erl \
    tftp_sup.erl

couchinetsebin_static_file = couch_inets.app

couchinetsebin_generated_file_list = \
    ftp.beam \
    ftp_progress.beam \
    ftp_response.beam \
    ftp_sup.beam \
    http_base_64.beam \
    http.beam \
    httpc_handler.beam \
    http_chunk.beam \
    httpc_manager.beam \
    http_cookie.beam \
    httpc_request.beam \
    httpc_response.beam \
    httpc_sup.beam \
    httpd_acceptor.beam \
    httpd_acceptor_sup.beam \
    httpd.beam \
    httpd_cgi.beam \
    httpd_conf.beam \
    httpd_esi.beam \
    httpd_example.beam \
    httpd_instance_sup.beam \
    httpd_manager.beam \
    httpd_misc_sup.beam \
    httpd_request.beam \
    httpd_request_handler.beam \
    httpd_response.beam \
    httpd_script_env.beam \
    httpd_socket.beam \
    httpd_sup.beam \
    httpd_util.beam \
    http_request.beam \
    http_response.beam \
    http_transport.beam \
    http_uri.beam \
    http_util.beam \
    inets_app.beam \
    inets.beam \
    inets_sup.beam \
    mod_actions.beam \
    mod_alias.beam \
    mod_auth.beam \
    mod_auth_dets.beam \
    mod_auth_mnesia.beam \
    mod_auth_plain.beam \
    mod_auth_server.beam \
    mod_browser.beam \
    mod_cgi.beam \
    mod_dir.beam \
    mod_disk_log.beam \
    mod_esi.beam \
    mod_get.beam \
    mod_head.beam \
    mod_htaccess.beam \
    mod_include.beam \
    mod_log.beam \
    mod_range.beam \
    mod_responsecontrol.beam \
    mod_security.beam \
    mod_security_server.beam \
    mod_trace.beam \
    tftp.beam \
    tftp_binary.beam \
    tftp_engine.beam \
    tftp_file.beam \
    tftp_lib.beam \
    tftp_sup.beam

couchinetsinclude_DATA = \
    ftp_internal.hrl \
    httpc_internal.hrl \
    httpd.hrl \
    http_internal.hrl \
    inets_internal.hrl \
    mod_auth.hrl \
    tftp.hrl

couchinetsebin_DATA = \
    $(couchinetsebin_static_file) \
    $(couchinetsebin_generated_file_list)

EXTRA_DIST =  \
    $(couchinets_file_collection) \
    $(couchinetsinclude_DATA) \
    $(couchinetsebin_static_file)

CLEANFILES = $(couchinetsebin_generated_file_list)

%.beam: %.erl
	erlc $<
