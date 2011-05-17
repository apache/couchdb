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

couchprivlibdir = $(localerlanglibdir)/couch-$(version)/priv/lib
devcouchprivlibdir = $(abs_top_builddir)/src/couchdb/priv/.libs

localconf_DATA = default.ini
noinst_DATA = default_dev.ini local_dev.ini

EXTRA_DIST = local.ini

CLEANFILES = $(localconf_DATA) $(noinst_DATA)

transform = @program_transform_name@
couchjs_command_name = `echo couchjs | sed '$(transform)'`
couchjs_dev_command_name = `echo couchjs_dev | sed '$(transform)'`

if WINDOWS
default.ini: default.ini.tpl
	sed -e "s|%bindir%|.|g" \
	    -e "s|%localconfdir%|$(localconfdir)|g" \
	    -e "s|%localdatadir%|../share/couchdb|g" \
		-e "s|%localbuilddatadir%|../share/couchdb|g" \
	    -e "s|%localstatelibdir%|../var/lib/couchdb|g" \
	    -e "s|%localstatelogdir%|../var/log/couchdb|g" \
	    -e "s|%localstaterundir%|../var/run/couchdb|g" \
	    -e "s|%couchprivlibdir%|../lib/couch-$(version)/priv/lib|g" \
	    -e "s|%couchjs_command_name%|couchjs.exe|g" \
	< $< > $@
else
default.ini: default.ini.tpl
	sed -e "s|%bindir%|$(bindir)|g" \
	    -e "s|%localconfdir%|$(localconfdir)|g" \
	    -e "s|%localdatadir%|$(localdatadir)|g" \
		-e "s|%localbuilddatadir%|$(localdatadir)|g" \
	    -e "s|%localstatelibdir%|$(localstatelibdir)|g" \
	    -e "s|%localstatelogdir%|$(localstatelogdir)|g" \
	    -e "s|%localstaterundir%|$(localstaterundir)|g" \
	    -e "s|%couchprivlibdir%|$(couchprivlibdir)|g" \
	    -e "s|%couchjs_command_name%|$(couchjs_command_name)|g" \
	< $< > $@
endif

default_dev.ini: default.ini.tpl
	sed -e "s|%bindir%|$(abs_top_builddir)/bin|g" \
	    -e "s|%localconfdir%|$(abs_top_builddir)/etc/couchdb|g" \
	    -e "s|%localdatadir%|$(abs_top_srcdir)/share|g" \
		-e "s|%localbuilddatadir%|$(abs_top_builddir)/share|g" \
	    -e "s|%localstatelibdir%|$(abs_top_builddir)/tmp/lib|g" \
	    -e "s|%localstatelogdir%|$(abs_top_builddir)/tmp/log|g" \
	    -e "s|%localstaterundir%|$(abs_top_builddir)/tmp/run|g" \
	    -e "s|%couchprivlibdir%|$(devcouchprivlibdir)|g" \
	    -e "s|%couchjs_command_name%|$(couchjs_dev_command_name)|g" \
	< $< > $@

# Noah said to not specify local.ini but it borks
# VPATH builds that make distcheck uses.
local_dev.ini: local.ini
	if test ! -f "$@"; then \
	    cp $< $@; \
	fi

install-data-hook:
	if test ! -f "$(DESTDIR)$(localconfdir)/local.ini"; then \
	    cp $(srcdir)/local.ini "$(DESTDIR)$(localconfdir)/local.ini"; \
	fi
	if test ! "$(mkdir_p)" = ""; then \
	    $(mkdir_p) "$(DESTDIR)$(localconfdir)/default.d"; \
	    $(mkdir_p) "$(DESTDIR)$(localconfdir)/local.d"; \
	else \
	    echo "WARNING: You may have to create these directories by hand."; \
	    mkdir -p "$(DESTDIR)$(localconfdir)/default.d"; \
	    mkdir -p "$(DESTDIR)$(localconfdir)/local.d"; \
	fi

uninstall-local:
	rm -f "$(DESTDIR)/$(localconfdir)/local.ini"
