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
noinst_DATA = test_util.beam test_web.beam

noinst_PROGRAMS = test_cfg_register
test_cfg_register_SOURCES = test_cfg_register.c
test_cfg_register_CFLAGS = -D_BSD_SOURCE

%.beam: %.erl
	$(ERLC) $<

run: run.tpl
	sed -e "s|%abs_top_srcdir%|@abs_top_srcdir@|g" \
	    -e "s|%abs_top_builddir%|@abs_top_builddir@|g" > \
	$@ < $<
	chmod +x $@

CLEANFILES = run *.beam
DISTCLEANFILES = temp.*

EXTRA_DIST = \
    run.tpl \
    test_web.erl \
    random_port.ini \
    001-load.t \
    002-icu-driver.t \
    010-file-basics.t \
    011-file-headers.t \
    020-btree-basics.t \
    021-btree-reductions.t \
    030-doc-from-json.t \
    031-doc-to-json.t \
    040-util.t \
    041-uuid-gen-seq.ini \
    041-uuid-gen-utc.ini \
    041-uuid-gen.t \
    042-work-queue.t \
    050-stream.t \
    060-kt-merging.t \
    061-kt-missing-leaves.t \
    062-kt-remove-leaves.t \
    063-kt-get-leaves.t \
    064-kt-counting.t \
    065-kt-stemming.t \
    070-couch-db.t \
    072-cleanup.t \
    073-changes.t \
    075-auth-cache.t \
    076-file-compression.t \
    080-config-get-set.t \
    081-config-override.1.ini \
    081-config-override.2.ini \
    081-config-override.t \
    082-config-register.t \
    083-config-no-files.t \
    090-task-status.t \
    100-ref-counter.t \
    120-stats-collect.t \
    121-stats-aggregates.cfg \
    121-stats-aggregates.ini \
    121-stats-aggregates.t \
    130-attachments-md5.t \
    140-attachment-comp.t \
    150-invalid-view-seq.t \
    160-vhosts.t \
    170-os-daemons.es \
    170-os-daemons.t \
    171-os-daemons-config.es \
    171-os-daemons-config.t \
    172-os-daemon-errors.1.sh \
    172-os-daemon-errors.2.sh \
    172-os-daemon-errors.3.sh \
    172-os-daemon-errors.4.sh \
    172-os-daemon-errors.t \
    173-os-daemon-cfg-register.t \
    180-http-proxy.ini \
    180-http-proxy.t \
    190-json-stream-parse.t \
    200-view-group-no-db-leaks.t \
    201-view-group-shutdown.t \
    210-os-proc-pool.t \
    220-compaction-daemon.t \
    230-httpc-pool.t \
    240-replication-compact.t \
    241-replication-large-atts.t \
    242-replication-many-leaves.t \
    243-doc-missing-stubs.t
