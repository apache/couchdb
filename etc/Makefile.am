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

SUBDIRS = couchdb default init launchd logrotate.d windows

default_sysconf_data = logrotate.d/couchdb

if INIT
init_sysconf_data = default/couchdb
init_DATA = init/couchdb
endif

if LAUNCHD
launchd_DATA = launchd/org.apache.couchdb.plist
endif

nobase_sysconf_DATA = $(default_sysconf_data) $(init_sysconf_data)

EXTRA_DIST = \
    default/couchdb \
    windows/couchdb.iss.tpl \
    windows/README.txt.tpl

if WINDOWS
EXTRA_DIST += \
    windows/setup-couchdb-@version@.exe.md5 \
    windows/setup-couchdb-@version@.exe.sha
endif

CLEANFILES = $(init_DATA) $(default_sysconf_data) $(launchd_DATA)

transform = @program_transform_name@
couchdb_command_name = `echo couchdb | sed '$(transform)'`

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
	    -e "s|%localstaterundir%|$(localstaterundir)|" \
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

if WINDOWS

# README.txt has \n line endings in the repo and must have \r\n
# when installed as notepad is used to view it.
# Also: the targets below don't seem to update after an svn-up (which
# changes the version string in the generated files) so we trick make
# into always building it with the FORCE pattern...
windows/README.txt: windows/README.txt.tpl FORCE
	sed -e "s|%package_name%|@package_name@|g" \
	    -e "s|%version%|@version@|g" \
	< $< | unix2dos > $@

windows/couchdb.iss: windows/couchdb.iss.tpl FORCE
	sed -e "s|%configure_input%|$@. Generated from $< by configure.|" \
	    -e "s|%package_name%|@package_name@|g" \
	    -e "s|%locallibbindir%|`cygpath -m @locallibbindir@`|g" \
	    -e "s|%version%|@version@|g" \
	    -e "s|%erts_version%|`$(ERL) -version 2>&1 | $(SED) 's/[^0-9\.]//g'`|g" \
	    -e "s|%openssl_bin_dir%|@openssl_bin_dir@|g" \
	    -e "s|%msvc_redist_dir%|@msvc_redist_dir@|g" \
	    -e "s|%msvc_redist_name%|@msvc_redist_name@|g" \
	< $< > $@

# The installer depends on all files, not just the source .iss file,
# so we trick make into always building it with the FORCE pattern...
windows/setup-couchdb-@version@.exe: windows/couchdb.iss windows/README.txt FORCE
	"$(INNO_COMPILER_EXECUTABLE)" /q $<
	@echo Windows Installer is at: `cygpath -a -w windows/setup-couchdb-@version@.exe`

windows/setup-couchdb-@version@.exe.md5: windows/setup-couchdb-@version@.exe
	cd windows && md5sum ./$(<F) > $(@F)

windows/setup-couchdb-@version@.exe.sha: windows/setup-couchdb-@version@.exe
	cd windows && sha1sum ./$(<F) > $(@F)

FORCE:

endif

install-data-hook:
	if test -n "$(init_DATA)"; then \
	    chmod +x "$(DESTDIR)$(initdir)/couchdb"; \
	fi
