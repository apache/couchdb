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

devdocdir = $(localdocdir)/developer/couchdb
couchprivlibdir = $(localerlanglibdir)/couch-$(version)/priv/lib
couchincludedir = $(localerlanglibdir)/couch-$(version)/include
couchebindir = $(localerlanglibdir)/couch-$(version)/ebin

couchprivlib_LTLIBRARIES = couch_erl_driver.la
couch_erl_driver_la_SOURCES = couch_erl_driver.c
couch_erl_driver_la_LDFLAGS = -module -avoid-version $(ICU_LOCAL_FLAGS)
couch_erl_driver_la_CFLAGS = $(ICU_LOCAL_FLAGS)
couch_erl_driver_la_LIBADD = -licuuc -licudata -licui18n

locallibbin_PROGRAMS = couchjs
couchjs_SOURCES = couch_js.c

couchinclude_DATA = couch_db.hrl

couchebin_DATA = $(compiled_files)

dist_devdoc_DATA = $(doc_base) $(doc_modules)

EXTRA_DIST = $(couch_files)

CLEANFILES = $(compiled_files) $(doc_base) $(doc_modules) edoc-info

source_files = \
    couch_btree.erl \
    couch_config.erl \
    couch_config_writer.erl \
    couch_db.erl \
    couch_db_update_notifier.erl \
    couch_db_update_notifier_sup.erl \
    couch_doc.erl \
    couch_event_sup.erl \
    couch_file.erl \
    couch_httpd.erl \
    couch_ft_query.erl \
    couch_key_tree.erl \
    couch_log.erl \
    couch_query_servers.erl \
    couch_rep.erl \
    couch_server.erl \
    couch_server_sup.erl \
    couch_stream.erl \
    couch_util.erl \
    couch_view.erl \
    couch_db_updater.erl

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
    couch_file.beam \
    couch_ft_query.beam \
    couch_httpd.beam \
    couch_key_tree.beam \
    couch_log.beam \
    couch_query_servers.beam \
    couch_rep.beam \
    couch_server.beam \
    couch_server_sup.beam \
    couch_stream.beam \
    couch_util.beam \
    couch_view.beam \
    couch_db_updater.beam

doc_base = \
    erlang.png \
    index.html \
    modules-frame.html \
    overview-summary.html \
    packages-frame.html \
    stylesheet.css

doc_modules = \
    couch_btree.html \
    couch_config.html \
    couch_config_writer.html \
    couch_db.html \
    couch_db_update_notifier.html \
    couch_db_update_notifier_sup.html \
    couch_doc.html \
    couch_event_sup.html \
    couch_file.html \
    couch_ft_query.html \
    couch_httpd.html \
    couch_key_tree.html \
    couch_log.html \
    couch_query_servers.html \
    couch_rep.html \
    couch_server.html \
    couch_server_sup.html \
    couch_stream.html \
    couch_util.html \
    couch_view.html

couch.app: couch.app.tpl
	sed -e "s|%package_name%|@package_name@|g" \
	    -e "s|%version%|@version@|g" > \
	$@ < $<
	chmod +x $@

$(dist_devdoc_DATA): $(source_files)
	for file in $(source_files); do \
	    if test -n "$$edoc_files"; then \
	        edoc_files="$$edoc_files, "; \
	    fi; \
	    edoc_files="$$edoc_files \"$$file\""; \
	done; \
	erl -noshell -run edoc_run files ["$$edoc_files"]

%.beam: %.erl
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
