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

pkgconf_DATA = couch.ini

CLEANFILES = $(pkgconf_DATA)

transform = @program_transform_name@
couchjs_command_name = `echo couchjs | sed '$(transform)'`

couchprivlibdir = $(erlanglibdir)/couch-$(version)/priv/lib

abs_bindir = $(abspath $(bindir))
abs_pkgdatadir = $(abspath $(pkgdatadir))
abs_pkgconfdir = $(abspath $(pkgconfdir))
abs_pkgstatelibdir = $(abspath $(pkgstatelibdir))
abs_pkgstatelogdir = $(abspath $(pkgstatelogdir))

couch.ini: couch.ini.tpl
	sed -e "s|%bindir%|$(abs_bindir)|g" \
	    -e "s|%pkgconfdir%|$(abs_pkgconfdir)|g" \
	    -e "s|%pkgdatadir%|$(abs_pkgdatadir)|g" \
	    -e "s|%pkgstatelibdir%|$(abs_pkgstatelibdir)|g" \
	    -e "s|%pkgstatelogdir%|$(abs_pkgstatelogdir)|g" \
	    -e "s|%couchprivlibdir%|$(couchprivlibdir)|g" \
	    -e "s|%couchjs_command_name%|$(couchjs_command_name)|g" \
	< $< > $@
