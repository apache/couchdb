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

SUBDIRS = couchdb default init launchd logrotate.d

default_sysconf_data = logrotate.d/couchdb

if INIT
init_sysconf_data = default/couchdb
init_DATA = init/couchdb
endif

if LAUNCHD
launchd_DATA = launchd/org.apache.couchdb.plist
endif

nobase_sysconf_DATA = $(default_sysconf_data) $(init_sysconf_data)

CLEANFILES = $(init_DATA) $(nobase_sysconf_DATA) $(launchd_DATA)

transform = @program_transform_name@
couchdb_command_name = `echo couchdb | sed '$(transform)'`

default/couchdb: default/couchdb.tpl
	if test "$(mkdir_p)"; then \
	    $(mkdir_p) default; \
	else \
	    if test ! -d default; then \
	        mkdir default; \
	    fi \
	fi
	sed -e "s|%configure_input%|$@. Generated from $< by configure.|" \
	    -e "s|%localconfdir%|$(localconfdir)|g" \
	    -e "s|%sysconfdir%|$(sysconfdir)|" \
	    -e "s|%localstatedir%|$(localstatedir)|" \
	< $< > $@

init/couchdb: init/couchdb.tpl
	if test "$(mkdir_p)"; then \
	    $(mkdir_p) init; \
	else \
	    if test ! -d init; then \
	        mkdir init; \
	    fi \
	fi
	sed -e "s|%configure_input%|$@. Generated from $< by configure.|" \
	    -e "s|%bindir%|$(bindir)|" \
	    -e "s|%sysconfdir%|$(sysconfdir)|" \
	    -e "s|%localerlangbindir%|$(localerlangbindir)|" \
	    -e "s|%couchdb_command_name%|$(couchdb_command_name)|" \
	< $< > $@

logrotate.d/couchdb: logrotate.d/couchdb.tpl
	sed -e "s|%localstatelogdir%|@localstatelogdir@|g" < $< > $@

launchd/org.apache.couchdb.plist: launchd/org.apache.couchdb.plist.tpl
	if test "$(mkdir_p)"; then \
	    $(mkdir_p) launchd; \
	else \
	    if test ! -d launchd; then \
	        mkdir launchd; \
	    fi \
	fi
	sed -e "s|%configure_input%|$@. Generated from $< by configure.|" \
	    -e "s|%bindir%|$(bindir)|" \
	    -e "s|%couchdb_command_name%|$(couchdb_command_name)|" \
	< $< > $@

install-data-hook:
	if test -n "$(init_DATA)"; then \
	    chmod 755 "$(DESTDIR)$(initdir)/couchdb"; \
	fi
