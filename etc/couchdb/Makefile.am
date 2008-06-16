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

localconf_DATA = couch.ini

CLEANFILES = $(localconf_DATA)

transform = @program_transform_name@
couchjs_command_name = `echo couchjs | sed '$(transform)'`

couchprivlibdir = $(localerlanglibdir)/couch-$(version)/priv/lib

couch.ini: couch.ini.tpl
	sed -e "s|%bindir%|$(bindir)|g" \
	    -e "s|%localconfdir%|$(localconfdir)|g" \
	    -e "s|%localdatadir%|$(localdatadir)|g" \
	    -e "s|%localstatelibdir%|$(localstatelibdir)|g" \
	    -e "s|%localstatelogdir%|$(localstatelogdir)|g" \
	    -e "s|%couchprivlibdir%|$(couchprivlibdir)|g" \
	    -e "s|%couchjs_command_name%|$(couchjs_command_name)|g" \
	< $< > $@
