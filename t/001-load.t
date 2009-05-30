#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa src/couchdb -sasl errlog_type error -boot start_sasl -noshell

% Test that we can load each module.

main(_) ->
    etap:plan(39),
    Modules = [
        couch_batch_save,
        couch_batch_save_sup,
        couch_btree,
        couch_config,
        couch_config_writer,
        couch_db,
        couch_db_update_notifier,
        couch_db_update_notifier_sup,
        couch_db_updater,
        couch_doc,
        couch_event_sup,
        couch_external_manager,
        couch_external_server,
        couch_file,
        couch_httpd,
        couch_httpd_db,
        couch_httpd_external,
        couch_httpd_misc_handlers,
        couch_httpd_show,
        couch_httpd_stats_handlers,
        couch_httpd_view,
        couch_key_tree,
        couch_log,
        couch_os_process,
        couch_query_servers,
        couch_ref_counter,
        couch_rep,
        couch_rep_sup,
        couch_server,
        couch_server_sup,
        couch_stats_aggregator,
        couch_stats_collector,
        couch_stream,
        couch_task_status,
        couch_util,
        couch_view,
        couch_view_compactor,
        couch_view_group,
        couch_view_updater
    ],

    lists:foreach(
        fun(Module) ->
            etap_can:loaded_ok(
                Module,
                lists:concat(["Loaded: ", Module])
            )
        end, Modules),
    etap:end_tests().
