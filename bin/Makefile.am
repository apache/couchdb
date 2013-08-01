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

if WINDOWS
bin_SCRIPTS = couchdb.bat
else
bin_SCRIPTS = couchdb couch-config
endif

man1dir = $(mandir)/man1

noinst_SCRIPTS = couchjs_dev couch-config_dev

man_file = couchdb.1

if BUILD_MAN
man_file_build = $(man_file)
else
man_file_build =
endif

BUILT_SOURCES = $(man_file_build)

EXTRA_DIST = $(man_file_build)

CLEANFILES = $(bin_SCRIPTS) $(man_file_build) $(noinst_SCRIPTS)

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
	    -e "s|%locallibdir%|@locallibdir@|g" \
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
	    -e "s|%erlangversion%|@erlangversion@|g" \
	    -e "s|%erlangbin%|@erlangbin@|g" \
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
	    -e "s|%erlangversion%|@erlangversion@|g" \
	    -e "s|%erlangbin%|@erlangbin@|g" \
	    -e "s|%couchdb_command_name%|$(abs_top_builddir)/utils/run|g" > \
	$@ < $<
	chmod +x $@

# Depend on source files so distributed man pages are not rebuilt for end user.

$(man_file): couchdb.tpl.in
	$(MAKE_SAFE) -f Makefile couchdb; \
	$(top_srcdir)/build-aux/missing --run \
	    help2man \
	        --no-info \
	        --help-option="-h" \
	        --version-option="-V" \
	        --name="$(package_name) database server" \
	        ./couchdb --output $@

install-data-local:
	if test -s $(man_file); then \
	    if test `cat $(man_file) | wc -l` -gt 1; then \
	        $(INSTALL) -d $(DESTDIR)$(man1dir); \
	        $(INSTALL_DATA) $(man_file) $(DESTDIR)$(man1dir)/$(man_file); \
	    fi \
	fi

uninstall-local:
	rm -f $(DESTDIR)$(man1dir)/$(man_file)

distcheck-hook:
	if test ! -s $(man_file); then \
	    $(top_srcdir)/build-aux/dist-error $(man_file); \
	else \
	    if test ! `cat $(man_file) | wc -l` -gt 1; then \
	        $(top_srcdir)/build-aux/dist-error $(man_file); \
	    fi \
	fi
