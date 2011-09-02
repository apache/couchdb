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

couchlibdir = $(localerlanglibdir)/couch-$(version)
couchprivdir = $(couchlibdir)/priv
couchprivlibdir = $(couchlibdir)/priv/lib

EXTRA_DIST = \
	spawnkillable/couchspawnkillable.sh \
	stat_descriptions.cfg.in \
	couch_ejson_compare/erl_nif_compat.h \
	couch_js/sm170.c \
	couch_js/sm180.c \
	couch_js/sm185.c

CLEANFILES = stat_descriptions.cfg

couchprivlib_LTLIBRARIES = couch_icu_driver.la
if USE_EJSON_COMPARE_NIF
couchprivlib_LTLIBRARIES += couch_ejson_compare.la
couch_ejson_compare_la_SOURCES = couch_ejson_compare/couch_ejson_compare.c
couch_ejson_compare_la_CFLAGS = -D_BSD_SOURCE $(ICU_CFLAGS) $(ERLANG_FLAGS)
couch_ejson_compare_la_LDFLAGS = -module -avoid-version
couch_ejson_compare_la_LIBADD = $(ICU_LIBS)
if WINDOWS
couch_ejson_compare_la_LDFLAGS += -no-undefined
endif
endif
couch_icu_driver_la_SOURCES = icu_driver/couch_icu_driver.c
couch_icu_driver_la_LDFLAGS = -module -avoid-version
couch_icu_driver_la_CFLAGS = $(ICU_CFLAGS) $(ERLANG_FLAGS)
couch_icu_driver_la_LIBADD = $(ICU_LIBS)

if WINDOWS
couch_icu_driver_la_LDFLAGS += -no-undefined
endif

COUCHJS_SRCS = \
	couch_js/http.c \
	couch_js/http.h \
	couch_js/main.c \
	couch_js/utf8.c \
	couch_js/utf8.h \
	couch_js/util.h \
	couch_js/util.c

locallibbin_PROGRAMS = couchjs
couchjs_SOURCES = $(COUCHJS_SRCS)
couchjs_CFLAGS = -g -Wall -Werror -D_BSD_SOURCE $(CURL_CFLAGS) $(JS_CFLAGS)
couchjs_LDADD = $(CURL_LIBS) $(JS_LIBS)

couchpriv_DATA = stat_descriptions.cfg
couchpriv_PROGRAMS = couchspawnkillable

%.cfg: %.cfg.in
	cp $< $@

if WINDOWS
couchspawnkillable_SOURCES = spawnkillable/couchspawnkillable_win.c
endif

if !WINDOWS
couchspawnkillable: spawnkillable/couchspawnkillable.sh
	cp $< $@
	chmod +x $@
endif

# libtool and automake have defeated markh.  For each of our executables
# we end up with 2 copies - one directly in the 'target' folder (eg, 'priv')
# and another - the correct one - in .libs.  The former doesn't work but is 
# what gets installed for 'couchspawnkillable' - but the correct one for
# couchjs.exe *does* get copied.  *shrug*  So just clobber it with the 
# correct one as the last step. See bug COUCHDB-439
install-data-hook:
	if test -f "$(DESTDIR)$(couchprivlibdir)/couch_icu_driver"; then \
	    rm -f "$(DESTDIR)$(couchprivlibdir)/couch_icu_driver.so"; \
	    cd "$(DESTDIR)$(couchprivlibdir)" && \
	        $(LN_S) couch_icu_driver couch_icu_driver.so; \
	fi
	if test -f "$(DESTDIR)$(couchprivlibdir)/couch_ejson_compare_nif"; then \
	    rm -f "$(DESTDIR)$(couchprivlibdir)/couch_ejson_compare_nif.so"; \
	    cd "$(DESTDIR)$(couchprivlibdir)" && \
	        $(LN_S) couch_ejson_compare_nif couch_ejson_compare_nif.so; \
	fi
if WINDOWS
	$(INSTALL) $(ICU_BIN)/icuuc44.dll $(bindir)
	$(INSTALL) $(ICU_BIN)/icudt44.dll $(bindir)
	$(INSTALL) $(ICU_BIN)/icuin44.dll $(bindir)
	$(INSTALL) $(JS_LIB_BINARY) $(bindir)
	$(INSTALL) .libs/couchspawnkillable.exe \
		"$(DESTDIR)$(couchprivdir)/couchspawnkillable.exe"
endif

uninstall-local:
	if test -f "$(DESTDIR)$(couchprivlibdir)/couch_erl_driver"; then \
	    rm -f "$(DESTDIR)$(couchprivlibdir)/couch_erl_driver.so"; \
	fi
