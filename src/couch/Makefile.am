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

couchinclude_DATA = include/couch_db.hrl
couchebin_DATA = $(compiled_files)

# dist_devdoc_DATA = $(doc_base) $(doc_modules)

CLEANFILES = $(compiled_files) $(doc_base)

# CLEANFILES = $(doc_modules) edoc-info

source_files = \
    src/couch.erl \
    src/couch_app.erl \
    src/couch_auth_cache.erl \
    src/couch_btree.erl \
    src/couch_changes.erl \
    src/couch_compaction_daemon.erl \
    src/couch_compress.erl \
    src/couch_config.erl \
    src/couch_config_writer.erl \
    src/couch_db.erl \
    src/couch_db_update_notifier.erl \
    src/couch_db_update_notifier_sup.erl \
    src/couch_doc.erl \
    src/couch_drv.erl \
    src/couch_ejson_compare.erl \
    src/couch_event_sup.erl \
    src/couch_external_manager.erl \
    src/couch_external_server.erl \
    src/couch_file.erl \
    src/couch_httpd.erl \
    src/couch_httpd_db.erl \
    src/couch_httpd_auth.erl \
    src/couch_httpd_cors.erl \
    src/couch_httpd_oauth.erl \
    src/couch_httpd_external.erl \
    src/couch_httpd_misc_handlers.erl \
    src/couch_httpd_proxy.erl \
    src/couch_httpd_rewrite.erl \
    src/couch_httpd_stats_handlers.erl \
    src/couch_httpd_vhost.erl \
    src/couch_key_tree.erl \
    src/couch_log.erl \
    src/couch_native_process.erl \
    src/couch_os_daemons.erl \
    src/couch_os_process.erl \
    src/couch_passwords.erl \
    src/couch_primary_sup.erl \
    src/couch_query_servers.erl \
    src/couch_secondary_sup.erl \
    src/couch_server.erl \
    src/couch_server_sup.erl \
    src/couch_stats_aggregator.erl \
    src/couch_stats_collector.erl \
    src/couch_stream.erl \
    src/couch_task_status.erl \
    src/couch_users_db.erl \
    src/couch_util.erl \
    src/couch_uuids.erl \
    src/couch_db_updater.erl \
    src/couch_work_queue.erl

EXTRA_DIST = \
	$(source_files) \
	include/couch_db.hrl \
	src/couch_js_functions.hrl

compiled_files = \
    ebin/couch.app \
    ebin/couch.beam \
    ebin/couch_app.beam \
    ebin/couch_auth_cache.beam \
    ebin/couch_btree.beam \
    ebin/couch_changes.beam \
    ebin/couch_compaction_daemon.beam \
    ebin/couch_compress.beam \
    ebin/couch_config.beam \
    ebin/couch_config_writer.beam \
    ebin/couch_db.beam \
    ebin/couch_db_update_notifier.beam \
    ebin/couch_db_update_notifier_sup.beam \
    ebin/couch_doc.beam \
    ebin/couch_drv.beam \
    ebin/couch_ejson_compare.beam \
    ebin/couch_event_sup.beam \
    ebin/couch_external_manager.beam \
    ebin/couch_external_server.beam \
    ebin/couch_file.beam \
    ebin/couch_httpd.beam \
    ebin/couch_httpd_db.beam \
    ebin/couch_httpd_auth.beam \
    ebin/couch_httpd_oauth.beam \
    ebin/couch_httpd_cors.beam \
    ebin/couch_httpd_proxy.beam \
    ebin/couch_httpd_external.beam \
    ebin/couch_httpd_misc_handlers.beam \
    ebin/couch_httpd_rewrite.beam \
    ebin/couch_httpd_stats_handlers.beam \
    ebin/couch_httpd_vhost.beam \
    ebin/couch_key_tree.beam \
    ebin/couch_log.beam \
    ebin/couch_native_process.beam \
    ebin/couch_os_daemons.beam \
    ebin/couch_os_process.beam \
    ebin/couch_passwords.beam \
    ebin/couch_primary_sup.beam \
    ebin/couch_query_servers.beam \
    ebin/couch_secondary_sup.beam \
    ebin/couch_server.beam \
    ebin/couch_server_sup.beam \
    ebin/couch_stats_aggregator.beam \
    ebin/couch_stats_collector.beam \
    ebin/couch_stream.beam \
    ebin/couch_task_status.beam \
    ebin/couch_users_db.beam \
    ebin/couch_util.beam \
    ebin/couch_uuids.beam \
    ebin/couch_db_updater.beam \
    ebin/couch_work_queue.beam

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
#     couch_util.html

if WINDOWS
ebin/couch.app: src/couch.app.tpl
	@mkdir -p ebin/
	modules=`find ./src -name "*.erl" \! -name ".*" -exec basename {} .erl \; | tr '\n' ',' | sed "s/,$$//"`; \
	sed -e "s|%package_name%|@package_name@|g" \
			-e "s|%version%|@version@|g" \
			-e "s|@modules@|$$modules|g" \
			-e "s|%localconfdir%|../etc/couchdb|g" \
			-e "s|@defaultini@|default.ini|g" \
			-e "s|@localini@|local.ini|g" > \
	$@ < $<
else
ebin/couch.app: src/couch.app.tpl
	@mkdir -p ebin/
	modules=`{ find ./src -name "*.erl" \! -name ".*" -exec basename {} .erl \; | tr '\n' ','; echo ''; } | sed "s/,$$//"`; \
	sed -e "s|%package_name%|@package_name@|g" \
			-e "s|%version%|@version@|g" \
			-e "s|@modules@|$$modules|g" \
			-e "s|%localconfdir%|@localconfdir@|g" \
			-e "s|@defaultini@|default.ini|g" \
			-e "s|@localini@|local.ini|g" > \
	$@ < $<
endif

# $(dist_devdoc_DATA): edoc-info

# $(ERL) -noshell -run edoc_run files [\"$<\"]

ebin/%.beam: src/%.erl include/couch_db.hrl src/couch_js_functions.hrl
	@mkdir -p ebin/
	$(ERLC) -I$(top_srcdir)/src -o ebin/ $(ERLC_FLAGS) ${TEST} $<;

