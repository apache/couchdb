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

datarootdir = @prefix@/share

pkgconf_DATA = couch.ini couch_httpd.conf

nobase_dist_pkgconf_DATA = conf/mime.types

CLEANFILES = $(pkgconf_DATA) $(dist_pkgconf_DATA)

transform = @program_transform_name@
couchjs_command_name = `echo couchjs | sed '$(transform)'`

couch.ini: couch.ini.tpl
	sed -e "s|%bindir%|@bindir@|g" \
	    -e "s|%pkgconfdir%|@pkgconfdir@|g" \
	    -e "s|%pkgdatadir%|@pkgdatadir@|g" \
	    -e "s|%pkgstatelibdir%|@pkgstatelibdir@|g" \
	    -e "s|%pkgstatelogdir%|@pkgstatelogdir@|g" \
	    -e "s|%couchprivlibdir%|$(couchprivlibdir)|g" \
	    -e "s|%couchjs_command_name%|$(couchjs_command_name)|g" \
	< $< > $@

couch_httpd.conf: couch_httpd.conf.tpl
	sed -e "s|%pkgconfdir%|@pkgconfdir@|g" \
	    -e "s|%pkgdatadir%|@pkgdatadir@|g" \
	    -e "s|%pkgstatelogdir%|@pkgstatelogdir@|g" \
	< $< > $@

