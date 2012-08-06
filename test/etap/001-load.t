#!/usr/bin/env escript
%% -*- erlang -*-

% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

% Test that we can load each module.

main(_) ->
    test_util:init_code_path(),
    Modules = [
        couch_auth_cache,
        couch_btree,
        couch_changes,
        couch_compress,
        couch_config,
        couch_config_writer,
        couch_db,
        couch_db_update_notifier,
        couch_db_update_notifier_sup,
        couch_db_updater,
        couch_doc,
        % Fails unless couch_config gen_server is started.
        % couch_ejson_compare,
        couch_event_sup,
        couch_external_manager,
        couch_external_server,
        couch_file,
        couch_httpd,
        couch_httpd_db,
        couch_httpd_external,
        couch_httpd_misc_handlers,
        couch_httpd_rewrite,
        couch_httpd_stats_handlers,
        couch_key_tree,
        couch_log,
        couch_os_process,
        couch_query_servers,
        couch_ref_counter,
        couch_server,
        couch_server_sup,
        couch_stats_aggregator,
        couch_stats_collector,
        couch_stream,
        couch_task_status,
        couch_util,
        couch_work_queue,
        json_stream_parse
    ],

    etap:plan(length(Modules)),
    lists:foreach(
        fun(Module) ->
            etap:loaded_ok(
                Module,
                lists:concat(["Loaded: ", Module])
            )
        end, Modules),
    etap:end_tests().
