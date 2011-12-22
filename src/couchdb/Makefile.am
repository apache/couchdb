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

SUBDIRS = priv

# devdocdir = $(localdocdir)/developer/couchdb
couchlibdir = $(localerlanglibdir)/couch-$(version)
couchincludedir = $(couchlibdir)/include
couchebindir = $(couchlibdir)/ebin

couchinclude_DATA = couch_api_wrap.hrl couch_db.hrl couch_js_functions.hrl couch_replicator.hrl
couchebin_DATA = $(compiled_files)

# dist_devdoc_DATA = $(doc_base) $(doc_modules)

CLEANFILES = $(compiled_files) $(doc_base)

# CLEANFILES = $(doc_modules) edoc-info

source_files = \
    couch.erl \
    couch_api_wrap.erl \
    couch_api_wrap_httpc.erl \
    couch_app.erl \
    couch_auth_cache.erl \
    couch_btree.erl \
    couch_changes.erl \
    couch_compaction_daemon.erl \
    couch_compress.erl \
    couch_config.erl \
    couch_config_writer.erl \
    couch_db.erl \
    couch_db_update_notifier.erl \
    couch_db_update_notifier_sup.erl \
    couch_doc.erl \
    couch_drv.erl \
    couch_ejson_compare.erl \
    couch_event_sup.erl \
    couch_external_manager.erl \
    couch_external_server.erl \
    couch_file.erl \
    couch_httpc_pool.erl \
    couch_httpd.erl \
    couch_httpd_db.erl \
    couch_httpd_auth.erl \
    couch_httpd_oauth.erl \
    couch_httpd_external.erl \
    couch_httpd_show.erl \
    couch_httpd_view.erl \
    couch_httpd_misc_handlers.erl \
    couch_httpd_proxy.erl \
    couch_httpd_replicator.erl \
    couch_httpd_rewrite.erl \
    couch_httpd_stats_handlers.erl \
    couch_httpd_vhost.erl \
    couch_key_tree.erl \
    couch_log.erl \
    couch_native_process.erl \
    couch_os_daemons.erl \
    couch_os_process.erl \
    couch_primary_sup.erl \
    couch_query_servers.erl \
    couch_ref_counter.erl \
    couch_rep_sup.erl \
    couch_replication_manager.erl \
    couch_replication_notifier.erl \
    couch_replicator.erl \
    couch_replicator_worker.erl \
    couch_replicator_utils.erl \
    couch_secondary_sup.erl \
    couch_server.erl \
    couch_server_sup.erl \
    couch_stats_aggregator.erl \
    couch_stats_collector.erl \
    couch_stream.erl \
    couch_task_status.erl \
    couch_users_db.erl \
    couch_util.erl \
    couch_uuids.erl \
    couch_view.erl \
    couch_view_compactor.erl \
    couch_view_updater.erl \
    couch_view_group.erl \
    couch_db_updater.erl \
    couch_work_queue.erl \
    json_stream_parse.erl

EXTRA_DIST = $(source_files) couch_api_wrap.hrl couch_db.hrl couch_js_functions.hrl couch_replicator.hrl

compiled_files = \
    couch.app \
    couch.beam \
    couch_api_wrap.beam \
    couch_api_wrap_httpc.beam \
    couch_app.beam \
    couch_auth_cache.beam \
    couch_btree.beam \
    couch_changes.beam \
    couch_compaction_daemon.beam \
    couch_compress.beam \
    couch_config.beam \
    couch_config_writer.beam \
    couch_db.beam \
    couch_db_update_notifier.beam \
    couch_db_update_notifier_sup.beam \
    couch_doc.beam \
    couch_drv.beam \
    couch_ejson_compare.beam \
    couch_event_sup.beam \
    couch_external_manager.beam \
    couch_external_server.beam \
    couch_file.beam \
    couch_httpc_pool.beam \
    couch_httpd.beam \
    couch_httpd_db.beam \
    couch_httpd_auth.beam \
    couch_httpd_oauth.beam \
    couch_httpd_proxy.beam \
    couch_httpd_external.beam \
    couch_httpd_show.beam \
    couch_httpd_view.beam \
    couch_httpd_misc_handlers.beam \
    couch_httpd_replicator.beam \
    couch_httpd_rewrite.beam \
    couch_httpd_stats_handlers.beam \
    couch_httpd_vhost.beam \
    couch_key_tree.beam \
    couch_log.beam \
    couch_native_process.beam \
    couch_os_daemons.beam \
    couch_os_process.beam \
    couch_primary_sup.beam \
    couch_query_servers.beam \
    couch_ref_counter.beam \
    couch_rep_sup.beam \
    couch_replication_manager.beam \
    couch_replication_notifier.beam \
    couch_replicator.beam \
    couch_replicator_worker.beam \
    couch_replicator_utils.beam \
    couch_secondary_sup.beam \
    couch_server.beam \
    couch_server_sup.beam \
    couch_stats_aggregator.beam \
    couch_stats_collector.beam \
    couch_stream.beam \
    couch_task_status.beam \
    couch_users_db.beam \
    couch_util.beam \
    couch_uuids.beam \
    couch_view.beam \
    couch_view_compactor.beam \
    couch_view_updater.beam \
    couch_view_group.beam \
    couch_db_updater.beam \
    couch_work_queue.beam \
    json_stream_parse.beam

# doc_base = \
#     erlang.png \
#     index.html \
#     modules-frame.html \
#     overview-summary.html \
#     packages-frame.html \
#     stylesheet.css

# doc_modules = \
#     couch_btree.html \
#     couch_config.html \
#     couch_config_writer.html \
#     couch_db.html \
#     couch_db_update_notifier.html \
#     couch_db_update_notifier_sup.html \
#     couch_doc.html \
#     couch_event_sup.html \
#     couch_file.html \
#     couch_httpd.html \
#     couch_key_tree.html \
#     couch_log.html \
#     couch_query_servers.html \
#     couch_rep.html \
#     couch_rep_sup.html \
#     couch_server.html \
#     couch_server_sup.html \
#     couch_stream.html \
#     couch_util.html \
#     couch_view.html

if WINDOWS
couch.app: couch.app.tpl
	modules=`find . -name "*.erl" -exec basename {} .erl \; | tr '\n' ',' | sed "s/,$$//"`; \
	sed -e "s|%package_name%|@package_name@|g" \
			-e "s|%version%|@version@|g" \
			-e "s|@modules@|$$modules|g" \
			-e "s|%localconfdir%|../etc/couchdb|g" \
			-e "s|@defaultini@|default.ini|g" \
			-e "s|@localini@|local.ini|g" > \
	$@ < $<
else
couch.app: couch.app.tpl
	modules=`{ find . -name "*.erl" -exec basename {} .erl \; | tr '\n' ','; echo ''; } | sed "s/,$$//"`; \
	sed -e "s|%package_name%|@package_name@|g" \
			-e "s|%version%|@version@|g" \
			-e "s|@modules@|$$modules|g" \
			-e "s|%localconfdir%|@localconfdir@|g" \
			-e "s|@defaultini@|default.ini|g" \
			-e "s|@localini@|local.ini|g" > \
	$@ < $<
	chmod +x $@
endif

# $(dist_devdoc_DATA): edoc-info

# $(ERL) -noshell -run edoc_run files [\"$<\"]

%.beam: %.erl couch_api_wrap.hrl couch_db.hrl couch_js_functions.hrl couch_replicator.hrl
	$(ERLC) $(ERLC_FLAGS) ${TEST} $<;

