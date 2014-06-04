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
noinst_DATA = test_util.beam

%.beam: %.erl
	$(ERLC) $<

run: run.tpl
	sed -e "s|%abs_top_srcdir%|@abs_top_srcdir@|g" \
	    -e "s|%abs_top_builddir%|@abs_top_builddir@|g" > \
	$@ < $<
	chmod +x $@

# @@ wildcards are NOT portable, please replace with clean-local rules
CLEANFILES = run *.beam

DISTCLEANFILES = temp.*

fixture_files = \
    fixtures/3b835456c235b1827e012e25666152f3.view \
    fixtures/test.couch

tap_files = \
    200-view-group-no-db-leaks.t \
    201-view-group-shutdown.t \
    210-os-proc-pool.t \
    220-compaction-daemon.t \
    230-pbkfd2.t \
    231-cors.t \
    250-upgrade-legacy-view-files.t

EXTRA_DIST = \
    run.tpl \
    $(fixture_files) \
    $(tap_files)
