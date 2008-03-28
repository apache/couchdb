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

datarootdir = @prefix@/share

couchprivlibdir = $(erlanglibdir)/couch-$(version)/priv/lib

SUBDIRS = default init launchd logrotate.d

default_sysconf_data = logrotate.d/couchdb.dist

pkgconf_DATA = couch.ini.dist couch_httpd.conf.dist

nobase_dist_pkgconf_DATA = conf/mime.types

if INIT
init_sysconf_data = default/couchdb.dist
init_DATA = init/couchdb.dist
endif

if LAUNCHD
launchd_DATA = launchd/org.apache.couchdb.plist.dist
endif

nobase_sysconf_DATA = $(default_sysconf_data) $(init_sysconf_data)

CLEANFILES = $(init_DATA) $(nobase_sysconf_DATA) $(launchd_DATA) $(pkgconf_DATA)

transform = @program_transform_name@
couchdb_command_name = `echo couchdb | sed '$(transform)'`
couchjs_command_name = `echo couchjs | sed '$(transform)'`

couch.ini.dist: couch.ini.tpl
	sed -e "s|%bindir%|@bindir@|g" \
	    -e "s|%pkgconfdir%|@pkgconfdir@|g" \
	    -e "s|%pkgdatadir%|@pkgdatadir@|g" \
	    -e "s|%pkgstatelibdir%|@pkgstatelibdir@|g" \
	    -e "s|%pkgstatelogdir%|@pkgstatelogdir@|g" \
	    -e "s|%couchprivlibdir%|$(couchprivlibdir)|g" \
	    -e "s|%couchjs_command_name%|$(couchjs_command_name)|g" \
	< $< > $@

couch_httpd.conf.dist: couch_httpd.conf.tpl
	sed -e "s|%pkgconfdir%|@pkgconfdir@|g" \
	    -e "s|%pkgdatadir%|@pkgdatadir@|g" \
	    -e "s|%pkgstatelogdir%|@pkgstatelogdir@|g" \
	< $< > $@

default/couchdb.dist: default/couchdb.tpl
	if test "$(mkdir_p)"; then \
	    $(mkdir_p) default; \
	else \
	    if test ! -d default; then \
	        mkdir default; \
	    fi \
	fi
	sed -e "s|%configure_input%|$@. Generated from $< by configure.|" \
	    -e "s|%pkgconfdir%|@pkgconfdir@|g" \
	    -e "s|%sysconfdir%|@sysconfdir@|" \
	    -e "s|%localstatedir%|@localstatedir@|" \
	< $< > $@

init/couchdb.dist: init/couchdb.tpl
	if test "$(mkdir_p)"; then \
	    $(mkdir_p) init; \
	else \
	    if test ! -d init; then \
	        mkdir init; \
	    fi \
	fi
	sed -e "s|%configure_input%|$@. Generated from $< by configure.|" \
	    -e "s|%bindir%|@bindir@|" \
	    -e "s|%sysconfdir%|@sysconfdir@|" \
	    -e "s|%erlangbindir%|@erlangbindir@|" \
	    -e "s|%couchdb_command_name%|$(couchdb_command_name)|" \
	< $< > $@

logrotate.d/couchdb.dist: logrotate.d/couchdb.tpl
	sed -e "s|%pkgstatelogdir%|@pkgstatelogdir@|g" < $< > $@

launchd/org.apache.couchdb.plist.dist: launchd/org.apache.couchdb.plist.tpl
	if test "$(mkdir_p)"; then \
	    $(mkdir_p) launchd; \
	else \
	    if test ! -d launchd; then \
	        mkdir launchd; \
	    fi \
	fi
	sed -e "s|%configure_input%|$@. Generated from $< by configure.|" \
	    -e "s|%bindir%|@bindir@|" \
	    -e "s|%couchdb_command_name%|$(couchdb_command_name)|" \
	< $< > $@

install-data-hook:
	if test $(init_DATA); then \
	    chmod 755 "$(DESTDIR)$(initdir)/couchdb"; \
	    mv -i "$(DESTDIR)$(sysconfdir)/default/couchdb.dist" \
	        "$(DESTDIR)$(sysconfdir)/default/couchdb"; \
	    mv -i "$(DESTDIR)$(initdir)/couchdb.dist" \
	        "$(DESTDIR)$(initdir)/couchdb"; \
	fi
	if test $(launchd_DATA); then \
	    mv -i "$(DESTDIR)$(launchddir)/org.apache.couchdb.plist.dist" \
	        "$(DESTDIR)$(launchddir)/org.apache.couchdb.plist"; \
	fi
	mv -i "$(DESTDIR)/$(sysconfdir)/logrotate.d/couchdb.dist" \
	    "$(DESTDIR)/$(sysconfdir)/logrotate.d/couchdb"
	mv -i "$(DESTDIR)/$(pkgconfdir)/couch.ini.dist" \
	    "$(DESTDIR)/$(pkgconfdir)/couch.ini"
	mv -i "$(DESTDIR)/$(pkgconfdir)/couch_httpd.conf.dist" \
	    "$(DESTDIR)/$(pkgconfdir)/couch_httpd.conf"

uninstall-hook:
	rm -f $(DESTDIR)$(initdir)/couchdb
	rm -f $(DESTDIR)$(launchddir)/org.apache.couchdb.plist
	rm -f $(DESTDIR)$(sysconfdir)/default/couchdb
	rm -f $(DESTDIR)$(sysconfdir)/logrotate.d/couchdb
	rm -f $(DESTDIR)/$(pkgconfdir)/couch.ini
	rm -f $(DESTDIR)/$(pkgconfdir)/couch_httpd.conf
