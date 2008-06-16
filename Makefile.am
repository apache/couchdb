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

SUBDIRS = bin etc src share var

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

local-clean: maintainer-clean
	@echo "This command is intended for maintainers to use;"
	@echo "it deletes files that may need special tools to rebuild."
	rm -f INSTALL
	rm -f acinclude.m4
	rm -f aclocal.m4
	rm -fr build-aux
	rm -f config.h.in*
	rm -f configure
	rm -f configure~
	rm -f *.tar.gz
	rm -f *.zip
	rm -fr m4
	find . -name Makefile.in | xargs rm

.PHONY: local-clean
