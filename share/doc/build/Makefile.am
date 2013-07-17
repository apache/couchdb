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

MAKE_SAFE = $(MAKE)

SPHINXOPTS = \
    -n -c $(srcdir)/../src \
    -A local=1 \
    $(srcdir)/../src

info_file_dist = texinfo/CouchDB.info

info_file_inst = CouchDB

if BUILD_INFO
info_file_build = $(info_file_dist)
else
info_file_build =
endif

pdf_file_dist = latex/CouchDB.pdf.gz

pdf_file_inst = CouchDB.pdf.gz

if BUILD_PDF
pdf_file_build = $(pdf_file_dist)
else
pdf_file_build =
endif

# Please keep lists of rst and related generated files sorted

html_files = \
    html/_images/futon-createdb.png \
    html/_images/futon-editdoc.png \
    html/_images/futon-editeddoc.png \
    html/_images/futon-overview.png \
    html/_images/futon-replform.png \
    html/_sources/api/authn.txt \
    html/_sources/api/configuration.txt \
    html/_sources/api/database.txt \
    html/_sources/api/design.txt \
    html/_sources/api/documents.txt \
    html/_sources/api/local.txt \
    html/_sources/api/misc.txt \
    html/_sources/api/reference.txt \
    html/_sources/config/auth.txt \
    html/_sources/config/compaction.txt \
    html/_sources/config/couchdb.txt \
    html/_sources/config/daemons.txt \
    html/_sources/config/externals.txt \
    html/_sources/config/http.txt \
    html/_sources/config/index.txt \
    html/_sources/config/log.txt \
    html/_sources/config/misc.txt \
    html/_sources/config/query-servers.txt \
    html/_sources/config/replicator.txt \
    html/_sources/config/intro.txt \
    html/_sources/config/proxying.txt \
    html/_sources/api-basics.txt \
    html/_sources/changelog.txt \
    html/_sources/changes.txt \
    html/_sources/contributing.txt \
    html/_sources/ddocs.txt \
    html/_sources/index.txt \
    html/_sources/intro.txt \
    html/_sources/json-structure.txt \
    html/_sources/query-servers.txt \
    html/_sources/replication.txt \
    html/_sources/replicator.txt \
    html/_static/ajax-loader.gif \
    html/_static/basic.css \
    html/_static/comment-bright.png \
    html/_static/comment-close.png \
    html/_static/comment.png \
    html/_static/default.css \
    html/_static/doctools.js \
    html/_static/down-pressed.png \
    html/_static/down.png \
    html/_static/favicon.ico \
    html/_static/file.png \
    html/_static/jquery.js \
    html/_static/logo.png \
    html/_static/minus.png \
    html/_static/plus.png \
    html/_static/pygments.css \
    html/_static/rtd.css \
    html/_static/searchtools.js \
    html/_static/sidebar.js \
    html/_static/underscore.js \
    html/_static/up-pressed.png \
    html/_static/up.png \
    html/_static/websupport.js \
    html/api/authn.html \
    html/api/configuration.html \
    html/api/database.html \
    html/api/design.html \
    html/api/documents.html \
    html/api/local.html \
    html/api/misc.html \
    html/api/reference.html \
    html/config/auth.html \
    html/config/compaction.html \
    html/config/couchdb.html \
    html/config/daemons.html \
    html/config/externals.html \
    html/config/http.html \
    html/config/index.html \
    html/config/log.html \
    html/config/misc.html \
    html/config/query-servers.html \
    html/config/replicator.html \
    html/config/intro.html \
    html/config/proxying.html \
    html/api-basics.html \
    html/changelog.html \
    html/changes.html \
    html/ddocs.html \
    html/index.html \
    html/intro.html \
    html/json-structure.html \
    html/query-servers.html \
    html/replication.html \
    html/replicator.html \
    html/objects.inv \
    html/genindex.html \
    html/search.html \
    html/searchindex.js

if BUILD_HTML
html_files_build = $(html_files)
else
html_files_build =
endif

image_files = \
    ../images/epub-icon.png \
    ../images/favicon.ico \
    ../images/futon-createdb.png \
    ../images/futon-editdoc.png \
    ../images/futon-editeddoc.png \
    ../images/futon-overview.png \
    ../images/futon-replform.png \
    ../images/logo.png

src_files = \
    ../src/api/authn.rst \
    ../src/api/configuration.rst \
    ../src/api/database.rst \
    ../src/api/design.rst \
    ../src/api/documents.rst \
    ../src/api/local.rst \
    ../src/api/misc.rst \
    ../src/api/reference.rst \
    ../src/config/auth.rst \
    ../src/config/compaction.rst \
    ../src/config/couchdb.rst \
    ../src/config/daemons.rst \
    ../src/config/externals.rst \
    ../src/config/http.rst \
    ../src/config/index.rst \
    ../src/config/log.rst \
    ../src/config/misc.rst \
    ../src/config/query-servers.rst \
    ../src/config/replicator.rst \
    ../src/config/intro.rst \
    ../src/config/proxying.rst \
    ../src/api-basics.rst \
    ../src/changelog.rst \
    ../src/changes.rst \
    ../src/contributing.rst \
    ../src/ddocs.rst \
    ../src/index.rst \
    ../src/intro.rst \
    ../src/json-structure.rst \
    ../src/query-servers.rst \
    ../src/replication.rst \
    ../src/replicator.rst \
    ../src/conf.py

src_files_html = \
    ../static/rtd.css \
    ../templates/help.html \
    ../templates/searchbox.html \
    ../templates/utilities.html

EXTRA_DIST = \
    $(image_files) \
    $(src_files) \
    $(src_files_html) \
    $(info_file_build) \
    $(pdf_file_build) \
    $(html_files_build)

BUILT_SOURCES = \
    $(info_file_build) \
    $(pdf_file_build) \
    $(html_files_build)

$(pdf_file_dist): pdf

$(html_files): html

$(info_file_dist): info.stamp
	@if test -f $@; then :; else \
	    rm -f info.stamp; \
	    $(MAKE_SAFE) $(AM_MAKEFLAGS) info.stamp; \
	fi

info.stamp: $(image_files) $(src_files)
	trap "rm -rf info.lock info.stamp" 1 2 13 15; \
	if mkdir info.lock 2>/dev/null; then \
	    rm -f info.tmp; \
	    touch info.tmp; \
	    $(top_srcdir)/build-aux/sphinx-build \
	        -b texinfo $(SPHINXOPTS) $(builddir)/texinfo; \
	    $(MAKE_SAFE) -C texinfo info; \
	    $(top_srcdir)/build-aux/sphinx-touch $(info_file_dist); \
	    mv -f info.tmp $@; \
	    rmdir info.lock; \
	else \
	    while test -d info.lock; do \
	        sleep 1; \
	    done; \
	    test -f info.stamp; \
	fi

$(pdf_file_dist): pdf.stamp
	@if test -f $@; then :; else \
	    rm -f pdf.stamp; \
	    $(MAKE_SAFE) $(AM_MAKEFLAGS) pdf.stamp; \
	fi

pdf.stamp: $(image_files) $(src_files)
	trap "rm -rf pdf.lock pdf.stamp" 1 2 13 15; \
	if mkdir pdf.lock 2>/dev/null; then \
	    rm -f pdf.tmp; \
	    touch pdf.tmp; \
	    $(top_srcdir)/build-aux/sphinx-build \
	        -b latex $(SPHINXOPTS) $(builddir)/latex; \
	    $(MAKE_SAFE) -C latex all-pdf; \
	    $(top_srcdir)/build-aux/sphinx-touch latex/CouchDB.pdf; \
	    gzip -9 < latex/CouchDB.pdf > $(pdf_file_dist); \
	    mv -f pdf.tmp $@; \
	    rmdir pdf.lock; \
	else \
	    while test -d pdf.lock; do \
	        sleep 1; \
	    done; \
	    test -f pdf.stamp; \
	fi

$(html_files): html.stamp
	@if test -f $@; then :; else \
	    rm -f html.stamp; \
	    $(MAKE_SAFE) $(AM_MAKEFLAGS) html.stamp; \
	fi

html.stamp: $(image_files) $(src_files) $(src_files_html)
	trap "rm -rf html.lock html.stamp" 1 2 13 15; \
	if mkdir html.lock 2>/dev/null; then \
	    rm -f html.tmp; \
	    touch html.tmp; \
	    $(top_srcdir)/build-aux/sphinx-build \
	        -b html $(SPHINXOPTS) $(builddir)/html; \
	    $(top_srcdir)/build-aux/sphinx-touch $(html_files); \
	    mv -f html.tmp $@; \
	    rmdir html.lock; \
	else \
	    while test -d html.lock; do \
	        sleep 1; \
	    done; \
	    test -f html.stamp; \
	fi

pdf-local:
	$(MAKE) $(AM_MAKEFLAGS) pdf.stamp

html-local:
	$(MAKE) $(AM_MAKEFLAGS) html.stamp

install-data-local:
	if test -s $(info_file_dist); then \
	    $(INSTALL) -d $(DESTDIR)$(infodir); \
	    $(INSTALL_DATA) \
	        $(info_file_dist) $(DESTDIR)$(infodir)/$(info_file_inst); \
	    if test -n "`which install-info`"; then \
	        install-info \
	            $(DESTDIR)$(infodir)/$(info_file_inst) \
	            $(DESTDIR)$(infodir)/dir \
	        || true; \
	    fi \
	fi
	if test -s $(pdf_file_dist); then \
	    $(INSTALL) -d $(DESTDIR)$(localdocdir); \
	    $(INSTALL_DATA) \
	        $(pdf_file_dist) $(DESTDIR)$(localdocdir)/$(pdf_file_inst); \
	fi
	for dist_file in $(html_files); do \
	    if test -s $$dist_file; then \
	        inst_file=`echo $$dist_file | sed s,^html,docs,`; \
	        $(INSTALL) -d `dirname $(DESTDIR)$(localdatadir)/www/$$inst_file`; \
	        $(INSTALL_DATA) \
	            $$dist_file $(DESTDIR)$(localdatadir)/www/$$inst_file; \
	    fi \
	done

uninstall-local:
	rm -f $(DESTDIR)$(infodir)/$(info_file_inst)
	if test -d $(DESTDIR)$(infodir); then \
	    if test -n "`which install-info`"; then\
	        install-info \
	            --delete \
	            $(DESTDIR)$(infodir)/$(info_file_inst) \
	            $(DESTDIR)$(infodir)/dir \
	        || true; \
	    fi \
	fi
	rm -f $(DESTDIR)$(localdocdir)/$(pdf_file_inst)
	for dist_file in $(html_files); do \
	    if test -s $$dist_file; then \
	        inst_file=`echo $$dist_file | sed s,^html,docs,`; \
	        rm -f $(DESTDIR)$(localdatadir)/www/$$inst_file; \
	    fi \
	done

DISTCLEANFILES = \
    info.stamp \
    pdf.stamp \
    html.stamp

distclean-local:
	rm -fr doctrees
	rm -fr html
	rm -fr latex
	rm -fr texinfo

# We handle exit code manually. Please fix if there is a better way.
distcheck-hook:
	if test ! -s $(info_file_dist); then \
	    $(top_srcdir)/build-aux/dist-error $(info_file_dist); \
	fi
	if test ! -s $(pdf_file_dist); then \
	    $(top_srcdir)/build-aux/dist-error $(pdf_file_dist); \
	fi
	exit_code=0; \
	for file in $(html_files); do \
	    if test ! -s $$file; then \
	        exit_code=1; \
	        $(top_srcdir)/build-aux/dist-error $$file || break; \
	    fi \
	done; \
	exit $$exit_code;
