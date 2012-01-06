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

if WINDOWS
bin_SCRIPTS = couchdb.bat
else
bin_SCRIPTS = couchdb couch-config
endif

noinst_SCRIPTS = couchjs_dev couch-config_dev

if HELP2MAN
dist_man1_MANS = couchdb.1
endif

CLEANFILES = $(bin_SCRIPTS) $(dist_man1_MANS) $(noinst_SCRIPTS)

transform = @program_transform_name@
couchdb_command_name = `echo couchdb | sed '$(transform)'`
couchjs_command_name = `echo couchjs | sed '$(transform)'`

couchdb: couchdb.tpl
	sed -e "s|%ERL%|$(ERL)|g" \
	    -e "s|%ICU_CONFIG%|$(ICU_CONFIG)|g" \
	    -e "s|%bindir%|@bindir@|g" \
	    -e "s|%localerlanglibdir%|@localerlanglibdir@|g" \
	    -e "s|%defaultini%|default.ini|g" \
	    -e "s|%localini%|local.ini|g" \
	    -e "s|%localconfdir%|@localconfdir@|g" \
	    -e "s|%localstatelogdir%|@localstatelogdir@|g" \
	    -e "s|%localstatelibdir%|@localstatelibdir@|g" \
	    -e "s|%localstatedir%|@localstatedir@|g" \
	    -e "s|%bug_uri%|@bug_uri@|g" \
	    -e "s|%package_author_address%|@package_author_address@|g" \
	    -e "s|%package_author_name%|@package_author_name@|g" \
	    -e "s|%package_name%|@package_name@|g" \
	    -e "s|%version%|@version@|g" \
	    -e "s|%couchdb_command_name%|$(couchdb_command_name)|g" > \
	$@ < $<
	chmod +x $@

if !WINDOWS
install-exec-hook:
	$(LN_S) -f "$(locallibbindir)/$(couchjs_command_name)" \
		"$(DESTDIR)$(bindir)/$(couchjs_command_name)"
endif

couchjs_dev:
	$(LN_S) -f "$(abs_top_builddir)/src/couchdb/priv/couchjs" "$@"

couchdb.bat: couchdb.bat.tpl
	sed -e "s|%ICU_CONFIG%|$(ICU_CONFIG)|g" \
	    -e "s|%version%|@version@|g" \
	$< > $@

couch-config: couch-config.tpl
	sed -e "s|%ERL%|$(ERL)|g" \
	    -e "s|%bindir%|@bindir@|g" \
	    -e "s|%localerlanglibdir%|@localerlanglibdir@|g" \
	    -e "s|%localconfdir%|@localconfdir@|g" \
		-e "s|%localdatadir%|$(localdatadir)|g" \
		-e "s|%localbuilddatadir%|$(localdatadir)|g" \
	    -e "s|%localstatelogdir%|@localstatelogdir@|g" \
	    -e "s|%localstatelibdir%|@localstatelibdir@|g" \
	    -e "s|%localstatedir%|@localstatedir@|g" \
	    -e "s|%localstaterundir%|@localstaterundir@|g" \
		-e "s|%couchlibdir%|@localerlanglibdir@/couch-%version%|g"\
		-e "s|%couchincludedir%|@localerlanglibdir@/include|g"\
		-e "s|%couchebindir%|@localerlanglibdir@/ebin|g"\
		-e "s|%couchprivlibdir%|@localerlanglibdir@/couch-%version%/priv/lib|g"\
	    -e "s|%bug_uri%|@bug_uri@|g" \
	    -e "s|%package_author_address%|@package_author_address@|g" \
	    -e "s|%package_author_name%|@package_author_name@|g" \
	    -e "s|%package_name%|@package_name@|g" \
	    -e "s|%version%|@version@|g" \
	    -e "s|%couchdb_command_name%|$(couchdb_command_name)|g" > \
	$@ < $<
	chmod +x $@

couch-config_dev: couch-config.tpl
	sed -e "s|%ERL%|$(ERL)|g" \
	    -e "s|%bindir%|$(abs_top_builddir)/bin|g" \
	    -e "s|%localerlanglibdir%|$(abs_top_builddir)\/src\/couchdb|g" \
		-e "s|%couchlibdir%|$(abs_top_builddir)\/src\/couchdb|g"\
		-e "s|%couchincludedir%|$(abs_top_builddir)\/src\/couchdb|g"\
		-e "s|%couchebindir%|$(abs_top_builddir)\/src\/couchdb|g"\
		-e "s|%couchprivlibdir%|$(abs_top_builddir)/src/couchdb/priv/.libs|g"\
		-e "s|%localdatadir%|$(abs_top_srcdir)/share|g" \
		-e "s|%localbuilddatadir%|$(abs_top_builddir)/share|g" \
	    -e "s|%localstatelibdir%|$(abs_top_builddir)/tmp/lib|g" \
	    -e "s|%localstatelogdir%|$(abs_top_builddir)/tmp/log|g" \
		-e "s|%localstatedir%|$(abs_top_builddir)/tmp|g" \
	    -e "s|%localstaterundir%|$(abs_top_builddir)/tmp/run|g" \
		-e "s|%bug_uri%|@bug_uri@|g" \
	    -e "s|%package_author_address%|@package_author_address@|g" \
	    -e "s|%package_author_name%|@package_author_name@|g" \
	    -e "s|%package_name%|@package_name@|g" \
	    -e "s|%version%|@version@|g" \
	    -e "s|%couchdb_command_name%|$(abs_top_builddir)/utils/run|g" > \
	$@ < $<
	chmod +x $@


HELP2MAN_OPTION=--no-info --help-option="-h" --version-option="-V"

# XXX: Because the scripts are made at build time for the user we need to
# XXX: depend on the original templates so as not to cause the rebuilding of
# XXX: the man pages.

couchdb.1: couchdb.tpl.in
	touch $@
	if test -x "$(HELP2MAN_EXECUTABLE)"; then \
	    $(MAKE) -f Makefile couchdb; \
	    $(HELP2MAN_EXECUTABLE) $(HELP2MAN_OPTION) \
	        --name="Apache CouchDB database server" ./couchdb --output $@; \
	fi
