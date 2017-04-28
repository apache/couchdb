% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(chttpd_httpd_handlers).

-export([url_handler/1, db_handler/1, design_handler/1]).

url_handler(<<>>)                  -> fun chttpd_misc:handle_welcome_req/1;
url_handler(<<"favicon.ico">>)     -> fun chttpd_misc:handle_favicon_req/1;
url_handler(<<"_utils">>)          -> fun chttpd_misc:handle_utils_dir_req/1;
url_handler(<<"_all_dbs">>)        -> fun chttpd_misc:handle_all_dbs_req/1;
url_handler(<<"_active_tasks">>)   -> fun chttpd_misc:handle_task_status_req/1;
url_handler(<<"_scheduler">>)      -> fun couch_replicator_httpd:handle_scheduler_req/1;
url_handler(<<"_node">>)           -> fun chttpd_misc:handle_node_req/1;
url_handler(<<"_reload_query_servers">>) -> fun chttpd_misc:handle_reload_query_servers_req/1;
url_handler(<<"_replicate">>)      -> fun chttpd_misc:handle_replicate_req/1;
url_handler(<<"_uuids">>)          -> fun chttpd_misc:handle_uuids_req/1;
url_handler(<<"_session">>)        -> fun chttpd_auth:handle_session_req/1;
url_handler(<<"_up">>)             -> fun chttpd_misc:handle_up_req/1;
url_handler(_) -> no_match.

db_handler(<<"_view_cleanup">>) -> fun chttpd_db:handle_view_cleanup_req/2;
db_handler(<<"_compact">>)      -> fun chttpd_db:handle_compact_req/2;
db_handler(<<"_design">>)       -> fun chttpd_db:handle_design_req/2;
db_handler(<<"_temp_view">>)    -> fun chttpd_view:handle_temp_view_req/2;
db_handler(<<"_changes">>)      -> fun chttpd_db:handle_changes_req/2;
db_handler(_) -> no_match.

design_handler(<<"_view">>)    -> fun chttpd_view:handle_view_req/3;
design_handler(<<"_show">>)    -> fun chttpd_show:handle_doc_show_req/3;
design_handler(<<"_list">>)    -> fun chttpd_show:handle_view_list_req/3;
design_handler(<<"_update">>)  -> fun chttpd_show:handle_doc_update_req/3;
design_handler(<<"_info">>)    -> fun chttpd_db:handle_design_info_req/3;
design_handler(<<"_rewrite">>) -> fun chttpd_rewrite:handle_rewrite_req/3;
design_handler(_) -> no_match.
