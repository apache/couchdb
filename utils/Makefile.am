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

noinst_SCRIPTS = run

CLEANFILES = $(noinst_SCRIPTS)

transform = @program_transform_name@
couchdb_command_name = `echo couchdb | sed '$(transform)'`

run: ../bin/couchdb.tpl
	sed -e "s|%ERL%|$(ERL)|g" \
	    -e "s|%ICU_CONFIG%|$(ICU_CONFIG)|g" \
	    -e "s|%bindir%|$(abs_top_builddir)/bin|g" \
	    -e "s|%defaultini%|default_dev.ini|g" \
	    -e "s|%localini%|local_dev.ini|g" \
	    -e "s|%localerlanglibdir%|foo \
			-pa $(abs_top_builddir)\/src\/couchdb \
			-pa $(abs_top_builddir)\/src\/ejson \
			-pa $(abs_top_builddir)\/src\/erlang-oauth \
			-pa $(abs_top_builddir)\/src\/ibrowse \
			-pa $(abs_top_builddir)\/src\/mochiweb \
			-pa $(abs_top_builddir)\/src\/snappy|g" \
	    -e "s|%localconfdir%|$(abs_top_builddir)/etc/couchdb|g" \
	    -e "s|%localstatelogdir%|$(abs_top_builddir)/tmp/log|g" \
	    -e "s|%localstatelibdir%|$(abs_top_builddir)/tmp/lib|g" \
	    -e "s|%localstatedir%|$(abs_top_builddir)/tmp|g" \
	    -e "s|%bug_uri%|@bug_uri@|g" \
	    -e "s|%package_author_address%|@package_author_address@|g" \
	    -e "s|%package_author_name%|@package_author_name@|g" \
	    -e "s|%package_name%|@package_name@|g" \
	    -e "s|%version%|@version@|g" \
	    -e "s|%couchdb_command_name%|$(couchdb_command_name)|g" > \
	$@ < $<
	chmod +x $@
