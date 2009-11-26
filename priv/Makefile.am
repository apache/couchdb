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
	couchspawnkillable.sh \
	stat_descriptions.cfg.in

CLEANFILES = stat_descriptions.cfg

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
if WINDOWS
	$(INSTALL) $(ICU_LOCAL_BIN)/icuuc42.dll $(bindir)
	$(INSTALL) $(ICU_LOCAL_BIN)/icudt42.dll $(bindir)
	$(INSTALL) $(ICU_LOCAL_BIN)/icuin42.dll $(bindir)
	$(INSTALL) $(JS_LIB_BINARY) $(bindir)
	$(INSTALL) .libs/couchspawnkillable.exe \
		"$(DESTDIR)$(couchprivdir)/couchspawnkillable.exe"
endif

uninstall-local:
	if test -f "$(DESTDIR)$(couchprivlibdir)/couch_erl_driver"; then \
	    rm -f "$(DESTDIR)$(couchprivlibdir)/couch_erl_driver.so"; \
	fi
