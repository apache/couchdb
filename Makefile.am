## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License.  You may obtain a copy
## of the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
## License for the specific language governing permissions and limitations
## under the License.

ICU_LOCAL_FLAGS = $(ICU_LOCAL_CFLAGS) $(ICU_LOCAL_LDFLAGS)

couchprivlibdir = $(localerlanglibdir)/couch-$(version)/priv/lib

couchprivlib_LTLIBRARIES = couch_erl_driver.la
couch_erl_driver_la_SOURCES = couch_erl_driver.c
couch_erl_driver_la_LDFLAGS = -module -avoid-version $(ICU_LOCAL_FLAGS)
couch_erl_driver_la_CFLAGS = $(ICU_LOCAL_FLAGS)
couch_erl_driver_la_LIBADD = -licuuc -licudata -licui18n

locallibbin_PROGRAMS = couchjs
couchjs_SOURCES = couch_js.c

couchebindir = $(localerlanglibdir)/couch-$(version)/ebin
couchincludedir = $(localerlanglibdir)/couch-$(version)/include

couch_file_collection = \
    cjson.erl \
    couch_btree.erl \
    couch_db.erl \
    couch_db_update_notifier.erl \
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
    couch_view.erl

couchebin_DATA = \
    cjson.beam \
    couch.app \
    couch_btree.beam \
    couch_db.beam \
    couch_db_update_notifier.beam \
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
    couch_view.beam

couchinclude_DATA = couch_db.hrl

EXTRA_DIST = $(couch_file_collection) $(couchinclude_DATA)

CLEANFILES = $(couchebin_DATA)

couch.app: couch.app.tpl
	sed -e "s|%package_name%|@package_name@|g" \
	    -e "s|%version%|@version@|g" > \
	$@ < $<
	chmod +x $@

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
