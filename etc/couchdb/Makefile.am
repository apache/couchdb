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

couchprivlibdir = $(localerlanglibdir)/couch-$(version)/priv/lib
devcouchprivlibdir = $(abs_top_srcdir)/src/couchdb/.libs

localconf_DATA = default.ini
dist_noinst_DATA = local.ini
noinst_DATA = default_dev.ini

CLEANFILES = $(localconf_DATA) $(noinst_DATA)

transform = @program_transform_name@
couchjs_command_name = `echo couchjs | sed '$(transform)'`
couchjs_dev_command_name = `echo couchjs_dev | sed '$(transform)'`

default.ini: default.ini.tpl
	sed -e "s|%bindir%|$(bindir)|g" \
	    -e "s|%localconfdir%|$(localconfdir)|g" \
	    -e "s|%localdatadir%|$(localdatadir)|g" \
	    -e "s|%localstatelibdir%|$(localstatelibdir)|g" \
	    -e "s|%localstatelogdir%|$(localstatelogdir)|g" \
	    -e "s|%couchprivlibdir%|$(couchprivlibdir)|g" \
	    -e "s|%couchjs_command_name%|$(couchjs_command_name)|g" \
	< $< > $@

default_dev.ini: default.ini.tpl
	sed -e "s|%bindir%|$(abs_top_srcdir)/bin|g" \
	    -e "s|%localconfdir%|$(abs_top_srcdir)/etc/couchdb|g" \
	    -e "s|%localdatadir%|$(abs_top_srcdir)/share|g" \
	    -e "s|%localstatelibdir%|$(abs_top_srcdir)/tmp/lib|g" \
	    -e "s|%localstatelogdir%|$(abs_top_srcdir)/tmp/log|g" \
	    -e "s|%couchprivlibdir%|$(devcouchprivlibdir)|g" \
	    -e "s|%couchjs_command_name%|$(couchjs_dev_command_name)|g" \
	< $< > $@

install-data-hook:
	if test ! -f "$(DESTDIR)/$(localconfdir)/local.ini"; then \
	    cp $(srcdir)/local.ini "$(DESTDIR)/$(localconfdir)/local.ini"; \
	fi

uninstall-local:
	rm -f "$(DESTDIR)/$(localconfdir)/local.ini"
