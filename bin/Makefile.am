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

bin_SCRIPTS = couchdb couchjs

if HELP2MAN
dist_man1_MANS = couchdb.1 couchjs.1
endif

CLEANFILES = $(bin_SCRIPTS) $(dist_man1_MANS)

transform = @program_transform_name@
couchdb_command_name = `echo couchdb | sed '$(transform)'`
couchjs_command_name = `echo couchjs | sed '$(transform)'`

couchdb: couchdb.tpl
	sed -e "s|%ERL%|$(ERL)|g" \
	    -e "s|%ICU_CONFIG%|$(ICU_CONFIG)|g" \
	    -e "s|%bindir%|@bindir@|g" \
	    -e "s|%localerlangbindir%|@localerlangbindir@|g" \
	    -e "s|%localerlanglibdir%|@localerlanglibdir@|g" \
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

couchjs: couchjs.tpl
	sed -e "s|%locallibbindir%|@locallibbindir@|g" \
	    -e "s|%bug_uri%|@bug_uri@|g" \
	    -e "s|%package_author_address%|@package_author_address@|g" \
	    -e "s|%package_author_name%|@package_author_name@|g" \
	    -e "s|%package_name%|@package_name@|g" \
	    -e "s|%version%|@version@|g" \
	    -e "s|%couchjs_command_name%|$(couchjs_command_name)|g" > \
	$@ < $<
	chmod +x $@

HELP2MAN_OPTION=--no-info --help-option="-h" --version-option="-V"

# XXX: Because the scripts are made at bulid time for the user we need to
# XXX: depend on the original templates so as not to cause the rebuilding of
# XXX: the man pages.

couchdb.1: couchdb.tpl.in
	touch $@
	if test -x "$(HELP2MAN_EXECUTABLE)"; then \
	    $(MAKE) -f Makefile couchdb; \
	    $(HELP2MAN_EXECUTABLE) $(HELP2MAN_OPTION) \
	        --name="Apache CouchDB database server" ./couchdb --output $@; \
	fi

couchjs.1: couchjs.tpl.in
	touch $@
	if test -x "$(HELP2MAN_EXECUTABLE)"; then \
	    $(MAKE) -f Makefile couchjs; \
	    $(HELP2MAN_EXECUTABLE) $(HELP2MAN_OPTION) \
	        --name="Apache CouchDB JavaScript interpreter" ./couchjs --output $@; \
	fi
