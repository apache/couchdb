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

SUBDIRS = bin etc src share test var utils

ACLOCAL_AMFLAGS = -I m4

localdoc_DATA = \
    AUTHORS.gz \
    BUGS.gz \
    DEVELOPERS.gz \
    INSTALL.gz \
    INSTALL.Unix.gz \
    INSTALL.Windows.gz \
    LICENSE.gz \
    NOTICE.gz \
    README.gz \
    THANKS.gz

DISTCLEANFILES = $(localdoc_DATA)

EXTRA_DIST = \
    AUTHORS \
    BUGS \
    DEVELOPERS \
    INSTALL \
    INSTALL.Unix \
    INSTALL.Windows \
    LICENSE \
    NOTICE \
    README.rst \
    THANKS \
    build-aux/dist-error \
    build-aux/sphinx-build \
    build-aux/sphinx-touch \
    license.skip

AUTHORS.gz: AUTHORS
	gzip -9 < $< > $@

BUGS.gz: BUGS
	gzip -9 < $< > $@

DEVELOPERS.gz: DEVELOPERS
	gzip -9 < $< > $@

INSTALL.gz: INSTALL
	gzip -9 < $< > $@

INSTALL.Unix.gz: INSTALL.Unix
	gzip -9 < $< > $@

INSTALL.Windows.gz: INSTALL.Windows
	gzip -9 < $< > $@

LICENSE.gz: LICENSE
	gzip -9 < $< > $@

NOTICE.gz: NOTICE
	gzip -9 < $< > $@

README.gz: README.rst
	gzip -9 < $< > $@

THANKS.gz: THANKS
	gzip -9 < $< > $@

check: dev check-js
if TESTS
	$(top_builddir)/test/etap/run $(top_srcdir)/test/etap
endif

check-js: dev
if TESTS
if USE_CURL
	MAKE=$(MAKE) $(top_builddir)/test/javascript/run
endif
endif

check-etap: dev
if TESTS
	$(top_builddir)/test/etap/run $(top_srcdir)/test/etap
endif

cover: dev
if TESTS
	rm -f cover/*.coverdata
	COVER=1 COVER_BIN=./src/couchdb/ $(top_builddir)/test/etap/run
	SRC=./src/couchdb/ \
	    $(ERL) -noshell \
	    -pa src/etap \
	    -eval 'etap_report:create()' \
	    -s init stop > /dev/null 2>&1
endif

dev: all
if TESTS
	@echo "This command is intended for developers to use;"
	@echo "it creates development ini files as well as a"
	@echo "$(top_builddir)/tmp structure for development runtime files."
	@echo "Use ./utils/run to launch CouchDB from the source tree."
	mkdir -p $(top_builddir)/etc/couchdb/default.d
	mkdir -p $(top_builddir)/etc/couchdb/local.d
	mkdir -p $(top_builddir)/tmp/lib
	mkdir -p $(top_builddir)/tmp/log
	mkdir -p $(top_builddir)/tmp/run/couchdb
	mkdir -p $(top_builddir)/tmp/plugins
endif

install-data-hook:
	@echo
	@echo "You have installed Apache CouchDB, time to relax."

distclean-local:
	rm -fr $(top_builddir)/etc/couchdb/default.d
	rm -fr $(top_builddir)/etc/couchdb/local.d
	rm -fr $(top_builddir)/tmp
	rm -f  $(top_builddir)/couchdb.stdout
	rm -f  $(top_builddir)/couchdb.stderr

.PHONY: local-clean
local-clean: maintainer-clean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may need special tools to rebuild."
	rm -f $(top_srcdir)/INSTALL
	rm -f $(top_srcdir)/acinclude.m4
	rm -f $(top_srcdir)/aclocal.m4
	rm -f $(top_srcdir)/config.h.in
	rm -f $(top_srcdir)/configure
	rm -f $(top_srcdir)/test/etap/temp.*
	rm -f $(top_srcdir)/*.tar.gz
	rm -f $(top_srcdir)/*.tar.gz.*
	find $(top_srcdir) -name Makefile.in -exec rm -f {} \;

dist-hook:
	find $(top_srcdir) -type f -name "._*" -exec rm -f {} \;
	find $(top_builddir) -type f -name "._*" -exec rm -f {} \;

distcheck-hook:
	$(MAKE) -C bin distcheck-hook
	$(MAKE) -C src/couchdb/priv distcheck-hook
	$(MAKE) -C share/doc/build distcheck-hook
	grep -rL 'http://www.apache.org/licenses/LICENSE-2.0' * \
	    | grep --include= -vEf $(top_srcdir)/license.skip; \
	    test "$$?" -eq 1
