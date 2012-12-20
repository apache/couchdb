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
    CHANGES.gz \
    DEVELOPERS.gz \
    INSTALL.gz \
    INSTALL.Unix.gz \
    INSTALL.Windows.gz \
    LICENSE.gz \
    NEWS.gz \
    NOTICE.gz \
    README.gz \
    THANKS.gz

DISTCLEANFILES = $(localdoc_DATA)

EXTRA_DIST = \
    AUTHORS \
    BUGS \
    CHANGES \
    DEVELOPERS \
    INSTALL \
    INSTALL.Unix \
    INSTALL.Windows \
    LICENSE \
    NEWS \
    NOTICE \
    README \
    THANKS \
    license.skip

AUTHORS.gz: $(top_srcdir)/AUTHORS
	-gzip -9 < $< > $@

BUGS.gz: $(top_srcdir)/BUGS
	-gzip -9 < $< > $@

CHANGES.gz: $(top_srcdir)/CHANGES
	-gzip -9 < $< > $@

DEVELOPERS.gz: $(top_srcdir)/DEVELOPERS
	-gzip -9 < $< > $@

INSTALL.gz: $(top_srcdir)/INSTALL
	-gzip -9 < $< > $@

INSTALL.Unix.gz: $(top_srcdir)/INSTALL.Unix
	-gzip -9 < $< > $@

INSTALL.Windows.gz: $(top_srcdir)/INSTALL.Windows
	-gzip -9 < $< > $@

LICENSE.gz: $(top_srcdir)/LICENSE
	-gzip -9 < $< > $@

NEWS.gz: $(top_srcdir)/NEWS
	-gzip -9 < $< > $@

NOTICE.gz: $(top_srcdir)/NOTICE
	-gzip -9 < $< > $@

README.gz: $(top_srcdir)/README
	-gzip -9 < $< > $@

THANKS.gz: $(top_srcdir)/THANKS
	-gzip -9 < $< > $@

check: dev
	$(top_builddir)/test/etap/run

cover: dev
	rm -f cover/*.coverdata
	COVER=1 COVER_BIN=./src/couchdb/ $(top_builddir)/test/etap/run
	SRC=./src/couchdb/ \
	    $(ERL) -noshell \
		-pa src/etap \
		-eval 'etap_report:create()' \
	    -s init stop > /dev/null 2>&1

dev: all
	@echo "This command is intended for developers to use;"
	@echo "it creates development ini files as well as a"
	@echo "$(top_builddir)/tmp structure for development runtime files."
	@echo "Use ./utils/run to launch CouchDB from the source tree."
	mkdir -p $(top_builddir)/etc/couchdb/default.d
	mkdir -p $(top_builddir)/etc/couchdb/local.d
	mkdir -p $(top_builddir)/tmp/lib
	mkdir -p $(top_builddir)/tmp/log
	mkdir -p $(top_builddir)/tmp/run/couchdb

install-data-hook:
	@echo
	@echo "You have installed Apache CouchDB, time to relax."

distclean-local:
	rm -fr $(top_builddir)/etc/couchdb/default.d
	rm -fr $(top_builddir)/etc/couchdb/local.d
	rm -fr $(top_builddir)/tmp

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
	grep -rL 'http://www.apache.org/licenses/LICENSE-2.0' * \
            | grep -vEf license.skip; \
        test "$$?" -eq 1

.PHONY: distsign
distsign: distcheck check
	@# @@ unpack archive and run diff -r to double check missing files
	@# @@ does automake have anything that does this?
	gpg --armor --detach-sig $(GPG_ARGS) \
	    < $(top_srcdir)/$(distdir).tar.gz \
	    > $(top_srcdir)/$(distdir).tar.gz.asc
	md5sum $(top_srcdir)/$(distdir).tar.gz \
	    > $(top_srcdir)/$(distdir).tar.gz.md5
	sha1sum $(top_srcdir)/$(distdir).tar.gz \
	    > $(top_srcdir)/$(distdir).tar.gz.sha
