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

SUBDIRS = bin etc src/couchdb src/mochiweb share test var

localdoc_DATA = AUTHORS.gz BUGS.gz CHANGES.gz NEWS.gz README.gz THANKS.gz

DISTCLEANFILES = $(localdoc_DATA)

EXTRA_DIST = AUTHORS BUGS CHANGES LICENSE NEWS NOTICE README THANKS

AUTHORS.gz: $(top_srcdir)/AUTHORS
	-gzip -9 < $< > $@

BUGS.gz: $(top_srcdir)/BUGS
	-gzip -9 < $< > $@

CHANGES.gz: $(top_srcdir)/CHANGES
	-gzip -9 < $< > $@

NEWS.gz: $(top_srcdir)/NEWS
	-gzip -9 < $< > $@

README.gz: $(top_srcdir)/README
	-gzip -9 < $< > $@

THANKS.gz: $(top_srcdir)/THANKS
	-gzip -9 < $< > $@

.PHONY: local-clean
local-clean: maintainer-clean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may need special tools to rebuild."
	rm -f $(top_srcdir)/INSTALL
	rm -f $(top_srcdir)/acinclude.m4
	rm -f $(top_srcdir)/aclocal.m4
	rm -f $(top_srcdir)/config.h.in
	rm -f $(top_srcdir)/configure
	rm -f $(top_srcdir)/*.tar.gz
	rm -f $(top_srcdir)/*.tar.gz.*
	rm -fr $(top_srcdir)/build-aux
	rm -fr $(top_srcdir)/m4
	find $(top_srcdir) -name Makefile.in -exec rm {} \;

.PHONY: distsign
distsign: distcheck
	gpg --armor --detach-sig \
	    < $(top_srcdir)/$(distdir).tar.gz \
	    > $(top_srcdir)/$(distdir).tar.gz.asc
	md5sum $(top_srcdir)/$(distdir).tar.gz \
	    > $(top_srcdir)/$(distdir).tar.gz.md5
	sha1sum $(top_srcdir)/$(distdir).tar.gz \
	    > $(top_srcdir)/$(distdir).tar.gz.sha
