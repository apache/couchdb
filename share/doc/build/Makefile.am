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
    html/_images/intro-consistency-01.png \
    html/_images/intro-consistency-02.png \
    html/_images/intro-consistency-03.png \
    html/_images/intro-consistency-04.png \
    html/_images/intro-consistency-05.png \
    html/_images/intro-consistency-06.png \
    html/_images/intro-consistency-07.png \
    html/_images/intro-tour-01.png \
    html/_images/intro-tour-02.png \
    html/_images/intro-tour-03.png \
    html/_images/intro-tour-04.png \
    html/_images/intro-tour-05.png \
    html/_images/intro-tour-06.png \
    html/_images/intro-tour-07.png \
    html/_images/intro-tour-08.png \
    html/_images/intro-tour-09.png \
    html/_images/intro-tour-10.png \
    html/_images/intro-why-01.png \
    html/_images/intro-why-02.png \
    html/_images/intro-why-03.png \
    html/_images/views-intro-01.png \
    html/_images/views-intro-02.png \
    html/_images/views-intro-03.png \
    html/_images/views-intro-04.png \
    html/_sources/api/basics.txt \
    html/_sources/api/index.txt \
    html/_sources/api/local.txt \
    html/_sources/api/database/bulk-api.txt \
    html/_sources/api/database/changes.txt \
    html/_sources/api/database/common.txt \
    html/_sources/api/database/compact.txt \
    html/_sources/api/database/index.txt \
    html/_sources/api/database/misc.txt \
    html/_sources/api/database/security.txt \
    html/_sources/api/database/temp-views.txt \
    html/_sources/api/document/attachments.txt \
    html/_sources/api/document/common.txt \
    html/_sources/api/document/index.txt \
    html/_sources/api/ddoc/common.txt \
    html/_sources/api/ddoc/index.txt \
    html/_sources/api/ddoc/render.txt \
    html/_sources/api/ddoc/rewrites.txt \
    html/_sources/api/ddoc/views.txt \
    html/_sources/api/server/authn.txt \
    html/_sources/api/server/common.txt \
    html/_sources/api/server/configuration.txt \
    html/_sources/api/server/index.txt \
    html/_sources/config/auth.txt \
    html/_sources/config/compaction.txt \
    html/_sources/config/couchdb.txt \
    html/_sources/config/externals.txt \
    html/_sources/config/http.txt \
    html/_sources/config/http-handlers.txt \
    html/_sources/config/index.txt \
    html/_sources/config/logging.txt \
    html/_sources/config/misc.txt \
    html/_sources/config/query-servers.txt \
    html/_sources/config/replicator.txt \
    html/_sources/config/services.txt \
    html/_sources/config/intro.txt \
    html/_sources/config/proxying.txt \
    html/_sources/couchapp/ddocs.txt \
    html/_sources/couchapp/index.txt \
    html/_sources/couchapp/views/collation.txt \
    html/_sources/couchapp/views/index.txt \
    html/_sources/couchapp/views/intro.txt \
    html/_sources/couchapp/views/joins.txt \
    html/_sources/couchapp/views/nosql.txt \
    html/_sources/couchapp/views/pagination.txt \
    html/_sources/cve/2010-0009.txt \
    html/_sources/cve/2010-2234.txt \
    html/_sources/cve/2010-3854.txt \
    html/_sources/cve/2012-5641.txt \
    html/_sources/cve/2012-5649.txt \
    html/_sources/cve/2012-5650.txt \
    html/_sources/cve/2014-2668.txt \
    html/_sources/cve/index.txt \
    html/_sources/fauxton/addons.txt \
    html/_sources/fauxton/index.txt \
    html/_sources/fauxton/install.txt \
    html/_sources/install/index.txt \
    html/_sources/install/freebsd.txt \
    html/_sources/install/mac.txt \
    html/_sources/install/unix.txt \
    html/_sources/install/windows.txt \
    html/_sources/intro/api.txt \
    html/_sources/intro/consistency.txt \
    html/_sources/intro/curl.txt \
    html/_sources/intro/futon.txt \
    html/_sources/intro/index.txt \
    html/_sources/intro/overview.txt \
    html/_sources/intro/security.txt \
    html/_sources/intro/tour.txt \
    html/_sources/intro/why.txt \
    html/_sources/maintenance/compaction.txt \
    html/_sources/maintenance/index.txt \
    html/_sources/maintenance/performance.txt \
    html/_sources/query-server/index.txt \
    html/_sources/query-server/erlang.txt \
    html/_sources/query-server/javascript.txt \
    html/_sources/query-server/protocol.txt \
    html/_sources/replication/conflicts.txt \
    html/_sources/replication/index.txt \
    html/_sources/replication/intro.txt \
    html/_sources/replication/protocol.txt \
    html/_sources/replication/replicator.txt \
    html/_sources/whatsnew/0.8.txt \
    html/_sources/whatsnew/0.9.txt \
    html/_sources/whatsnew/0.10.txt \
    html/_sources/whatsnew/0.11.txt \
    html/_sources/whatsnew/1.0.txt \
    html/_sources/whatsnew/1.1.txt \
    html/_sources/whatsnew/1.2.txt \
    html/_sources/whatsnew/1.3.txt \
    html/_sources/whatsnew/1.4.txt \
    html/_sources/whatsnew/1.5.txt \
    html/_sources/whatsnew/1.6.txt \
    html/_sources/whatsnew/index.txt \
    html/_sources/about.txt \
    html/_sources/contents.txt \
    html/_sources/contributing.txt \
    html/_sources/experimental.txt \
    html/_sources/externals.txt \
    html/_sources/json-structure.txt \
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
    html/api/basics.html \
    html/api/index.html \
    html/api/local.html \
    html/api/database/bulk-api.html \
    html/api/database/changes.html \
    html/api/database/common.html \
    html/api/database/compact.html \
    html/api/database/index.html \
    html/api/database/misc.html \
    html/api/database/security.html \
    html/api/database/temp-views.html \
    html/api/document/attachments.html \
    html/api/document/common.html \
    html/api/document/index.html \
    html/api/ddoc/common.html \
    html/api/ddoc/index.html \
    html/api/ddoc/render.html \
    html/api/ddoc/rewrites.html \
    html/api/ddoc/views.html \
    html/api/server/authn.html \
    html/api/server/common.html \
    html/api/server/configuration.html \
    html/api/server/index.html \
    html/config/auth.html \
    html/config/compaction.html \
    html/config/couchdb.html \
    html/config/externals.html \
    html/config/http.html \
    html/config/http-handlers.html \
    html/config/index.html \
    html/config/logging.html \
    html/config/misc.html \
    html/config/query-servers.html \
    html/config/replicator.html \
    html/config/services.html \
    html/config/intro.html \
    html/config/proxying.html \
    html/couchapp/ddocs.html \
    html/couchapp/index.html \
    html/couchapp/views/collation.html \
    html/couchapp/views/index.html \
    html/couchapp/views/intro.html \
    html/couchapp/views/joins.html \
    html/couchapp/views/nosql.html \
    html/couchapp/views/pagination.html \
    html/cve/2010-0009.html \
    html/cve/2010-2234.html \
    html/cve/2010-3854.html \
    html/cve/2012-5641.html \
    html/cve/2012-5649.html \
    html/cve/2012-5650.html \
    html/cve/2014-2668.html \
    html/cve/index.html \
    html/fauxton/addons.html \
    html/fauxton/index.html \
    html/fauxton/install.html \
    html/install/index.html \
    html/install/freebsd.html \
    html/install/mac.html \
    html/install/unix.html \
    html/install/windows.html \
    html/intro/api.html \
    html/intro/consistency.html \
    html/intro/curl.html \
    html/intro/futon.html \
    html/intro/index.html \
    html/intro/overview.html \
    html/intro/security.html \
    html/intro/tour.html \
    html/intro/why.html \
    html/maintenance/compaction.html \
    html/maintenance/index.html \
    html/maintenance/performance.html \
    html/query-server/index.html \
    html/query-server/erlang.html \
    html/query-server/javascript.html \
    html/query-server/protocol.html \
    html/replication/conflicts.html \
    html/replication/index.html \
    html/replication/intro.html \
    html/replication/protocol.html \
    html/replication/replicator.html \
    html/whatsnew/0.8.html \
    html/whatsnew/0.9.html \
    html/whatsnew/0.10.html \
    html/whatsnew/0.11.html \
    html/whatsnew/1.0.html \
    html/whatsnew/1.1.html \
    html/whatsnew/1.2.html \
    html/whatsnew/1.3.html \
    html/whatsnew/1.4.html \
    html/whatsnew/1.5.html \
    html/whatsnew/1.6.html \
    html/whatsnew/index.html \
    html/about.html \
    html/config-ref.html \
    html/contents.html \
    html/contributing.html \
    html/download.html \
    html/experimental.html \
    html/externals.html \
    html/http-api.html \
    html/index.html \
    html/json-structure.html \
    html/objects.inv \
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
    ../images/intro-consistency-01.png \
    ../images/intro-consistency-02.png \
    ../images/intro-consistency-03.png \
    ../images/intro-consistency-04.png \
    ../images/intro-consistency-05.png \
    ../images/intro-consistency-06.png \
    ../images/intro-consistency-07.png \
    ../images/intro-tour-01.png \
    ../images/intro-tour-02.png \
    ../images/intro-tour-03.png \
    ../images/intro-tour-04.png \
    ../images/intro-tour-05.png \
    ../images/intro-tour-06.png \
    ../images/intro-tour-07.png \
    ../images/intro-tour-08.png \
    ../images/intro-tour-09.png \
    ../images/intro-tour-10.png \
    ../images/intro-why-01.png \
    ../images/intro-why-02.png \
    ../images/intro-why-03.png \
    ../images/logo.png \
    ../images/views-intro-01.png \
    ../images/views-intro-02.png \
    ../images/views-intro-03.png \
    ../images/views-intro-04.png

src_files = \
    ../src/api/basics.rst \
    ../src/api/index.rst \
    ../src/api/local.rst \
    ../src/api/database/bulk-api.rst \
    ../src/api/database/changes.rst \
    ../src/api/database/common.rst \
    ../src/api/database/compact.rst \
    ../src/api/database/index.rst \
    ../src/api/database/misc.rst \
    ../src/api/database/security.rst \
    ../src/api/database/temp-views.rst \
    ../src/api/document/attachments.rst \
    ../src/api/document/common.rst \
    ../src/api/document/index.rst \
    ../src/api/ddoc/common.rst \
    ../src/api/ddoc/index.rst \
    ../src/api/ddoc/render.rst \
    ../src/api/ddoc/rewrites.rst \
    ../src/api/ddoc/views.rst \
    ../src/api/server/authn.rst \
    ../src/api/server/common.rst \
    ../src/api/server/configuration.rst \
    ../src/api/server/index.rst \
    ../src/config/auth.rst \
    ../src/config/compaction.rst \
    ../src/config/couchdb.rst \
    ../src/config/externals.rst \
    ../src/config/http.rst \
    ../src/config/http-handlers.rst \
    ../src/config/index.rst \
    ../src/config/logging.rst \
    ../src/config/misc.rst \
    ../src/config/query-servers.rst \
    ../src/config/replicator.rst \
    ../src/config/services.rst \
    ../src/config/intro.rst \
    ../src/config/proxying.rst \
    ../src/couchapp/ddocs.rst \
    ../src/couchapp/index.rst \
    ../src/couchapp/views/collation.rst \
    ../src/couchapp/views/index.rst \
    ../src/couchapp/views/intro.rst \
    ../src/couchapp/views/joins.rst \
    ../src/couchapp/views/nosql.rst \
    ../src/couchapp/views/pagination.rst \
    ../src/cve/2010-0009.rst \
    ../src/cve/2010-2234.rst \
    ../src/cve/2010-3854.rst \
    ../src/cve/2012-5641.rst \
    ../src/cve/2012-5649.rst \
    ../src/cve/2012-5650.rst \
    ../src/cve/2014-2668.rst \
    ../src/cve/index.rst \
    ../src/fauxton/addons.rst \
    ../src/fauxton/index.rst \
    ../src/fauxton/install.rst \
    ../src/install/index.rst \
    ../src/install/freebsd.rst \
    ../src/install/mac.rst \
    ../src/install/unix.rst \
    ../src/install/windows.rst \
    ../src/intro/api.rst \
    ../src/intro/consistency.rst \
    ../src/intro/curl.rst \
    ../src/intro/futon.rst \
    ../src/intro/index.rst \
    ../src/intro/overview.rst \
    ../src/intro/security.rst \
    ../src/intro/tour.rst \
    ../src/intro/why.rst \
    ../src/maintenance/compaction.rst \
    ../src/maintenance/index.rst \
    ../src/maintenance/performance.rst \
    ../src/query-server/index.rst \
    ../src/query-server/erlang.rst \
    ../src/query-server/javascript.rst \
    ../src/query-server/protocol.rst \
    ../src/replication/conflicts.rst \
    ../src/replication/index.rst \
    ../src/replication/intro.rst \
    ../src/replication/protocol.rst \
    ../src/replication/replicator.rst \
    ../src/whatsnew/0.8.rst \
    ../src/whatsnew/0.9.rst \
    ../src/whatsnew/0.10.rst \
    ../src/whatsnew/0.11.rst \
    ../src/whatsnew/1.0.rst \
    ../src/whatsnew/1.1.rst \
    ../src/whatsnew/1.2.rst \
    ../src/whatsnew/1.3.rst \
    ../src/whatsnew/1.4.rst \
    ../src/whatsnew/1.5.rst \
    ../src/whatsnew/1.6.rst \
    ../src/whatsnew/index.rst \
    ../src/about.rst \
    ../src/contents.rst \
    ../src/experimental.rst \
    ../src/contributing.rst \
    ../src/externals.rst \
    ../src/json-structure.rst \
    ../src/conf.py

src_files_html = \
    ../static/rtd.css \
    ../templates/couchdb/domainindex.html \
    ../templates/couchdb/theme.conf \
    ../templates/pages/download.html \
    ../templates/pages/index.html \
    ../templates/layout.html \
    ../templates/help.html \
    ../templates/searchbox.html \
    ../templates/tracking.html \
    ../templates/utilities.html

sphinx_extensions = \
    ../ext/configdomain.py \
    ../ext/github.py \
    ../ext/httpdomain.py

EXTRA_DIST = \
    $(image_files) \
    $(src_files) \
    $(src_files_html) \
    $(info_file_build) \
    $(pdf_file_build) \
    $(html_files_build) \
    $(sphinx_extensions)

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
