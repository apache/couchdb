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

ICU_LOCAL_FLAGS = $(ICU_LOCAL_CFLAGS) $(ICU_LOCAL_LDFLAGS)

# devdocdir = $(localdocdir)/developer/couchdb
couchprivlibdir = $(localerlanglibdir)/couch-$(version)/priv/lib
couchincludedir = $(localerlanglibdir)/couch-$(version)/include
couchebindir = $(localerlanglibdir)/couch-$(version)/ebin

couchprivlib_LTLIBRARIES = couch_erl_driver.la
couch_erl_driver_la_SOURCES = couch_erl_driver.c
couch_erl_driver_la_LDFLAGS = -module -avoid-version $(ICU_LOCAL_FLAGS)
couch_erl_driver_la_CFLAGS = $(ICU_LOCAL_FLAGS)
couch_erl_driver_la_LIBADD = -licuuc -licudata -licui18n

locallibbin_PROGRAMS = couchjs
couchjs_SOURCES = couch_js.c curlhelper.c curlhelper.h
couchjs_LDLAGS = $(CURL_LDLAGS)
couchjs_CFLAGS = $(CURL_CFLAGS)
couchjs_LDADD = -lcurl

couchinclude_DATA = couch_db.hrl

couchebin_DATA = $(compiled_files)

# dist_devdoc_DATA = $(doc_base) $(doc_modules)

CLEANFILES = $(compiled_files) $(doc_base)

# CLEANFILES = $(doc_modules) edoc-info

source_files = \
    couch_btree.erl \
    couch_config.erl \
    couch_config_writer.erl \
    couch_db.erl \
    couch_db_update_notifier.erl \
    couch_db_update_notifier_sup.erl \
    couch_doc.erl \
    couch_event_sup.erl \
    couch_external_manager.erl \
    couch_external_server.erl \
    couch_file.erl \
    couch_file_stats.erl \
    couch_httpd.erl \
    couch_httpd_db.erl \
    couch_httpd_external.erl \
    couch_httpd_show.erl \
    couch_httpd_view.erl \
    couch_httpd_misc_handlers.erl \
    couch_key_tree.erl \
    couch_log.erl \
    couch_os_process.erl \
    couch_query_servers.erl \
    couch_ref_counter.erl \
    couch_rep.erl \
    couch_server.erl \
    couch_server_sup.erl \
    couch_stream.erl \
    couch_task_status.erl \
    couch_util.erl \
    couch_view.erl \
    couch_view_updater.erl \
    couch_view_group.erl \
    couch_db_updater.erl

EXTRA_DIST = $(source_files) couch_db.hrl

compiled_files = \
    couch.app \
    couch_btree.beam \
    couch_config.beam \
    couch_config_writer.beam \
    couch_db.beam \
    couch_db_update_notifier.beam \
    couch_db_update_notifier_sup.beam \
    couch_doc.beam \
    couch_event_sup.beam \
    couch_external_manager.beam \
    couch_external_server.beam \
    couch_file.beam \
    couch_file_stats.beam \
    couch_httpd.beam \
    couch_httpd_db.beam \
    couch_httpd_external.beam \
    couch_httpd_show.beam \
    couch_httpd_view.beam \
    couch_httpd_misc_handlers.beam \
    couch_key_tree.beam \
    couch_log.beam \
    couch_os_process.beam \
    couch_query_servers.beam \
    couch_ref_counter.beam \
    couch_rep.beam \
    couch_server.beam \
    couch_server_sup.beam \
    couch_stream.beam \
    couch_task_status.beam \
    couch_util.beam \
    couch_view.beam \
    couch_view_updater.beam \
    couch_view_group.beam \
    couch_db_updater.beam

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
#     couch_server.html \
#     couch_server_sup.html \
#     couch_stream.html \
#     couch_util.html \
#     couch_view.html

couch.app: couch.app.tpl
	sed -e "s|%package_name%|@package_name@|g" \
	    -e "s|%version%|@version@|g" > \
	$@ < $<
	chmod +x $@

# $(dist_devdoc_DATA): edoc-info

# $(ERL) -noshell -run edoc_run files [\"$<\"]

%.beam: %.erl couch_db.hrl
	$(ERLC) $<

install-data-hook:
	if test -f "$(DESTDIR)/$(couchprivlibdir)/couch_erl_driver"; then \
	    rm -f "$(DESTDIR)/$(couchprivlibdir)/couch_erl_driver.so"; \
	    cd "$(DESTDIR)/$(couchprivlibdir)" && \
	        $(LN_S) couch_erl_driver couch_erl_driver.so; \
	fi

uninstall-local:
	if test -f "$(DESTDIR)/$(couchprivlibdir)/couch_erl_driver"; then \
	    rm -f "$(DESTDIR)/$(couchprivlibdir)/couch_erl_driver.so"; \
	fi
