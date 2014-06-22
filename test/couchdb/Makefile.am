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

SUBDIRS = fixtures

noinst_SCRIPTS = run

eunit_files = \
    couch_auth_cache_tests.erl \
    couch_btree_tests.erl \
    couch_changes_tests.erl \
    couch_config_tests.erl \
    couch_db_tests.erl \
    couch_doc_json_tests.erl \
    couch_file_tests.erl \
    couch_key_tree_tests.erl \
    couch_passwords_tests.erl \
    couch_ref_counter_tests.erl \
    couch_stream_tests.erl \
    couch_stats_tests.erl \
    couch_task_status_tests.erl \
    couch_util_tests.erl \
    couch_uuids_tests.erl \
    couch_work_queue_tests.erl \
    couchdb_attachments_tests.erl \
    couchdb_compaction_daemon.erl \
    couchdb_cors_tests.erl \
    couchdb_csp_tests.erl \
    couchdb_file_compression_tests.erl \
    couchdb_http_proxy_tests.erl \
    couchdb_modules_load_tests.erl \
    couchdb_os_daemons_tests.erl \
    couchdb_os_proc_pool.erl \
    couchdb_update_conflicts_tests.erl \
    couchdb_vhosts_tests.erl \
    couchdb_views_tests.erl \
    json_stream_parse_tests.erl \
    test_request.erl \
    test_web.erl \
    include/couch_eunit.hrl

fixture_files = \
    fixtures/couch_config_tests_1.ini \
    fixtures/couch_config_tests_2.ini \
    fixtures/couch_stats_aggregates.cfg \
    fixtures/couch_stats_aggregates.ini \
    fixtures/os_daemon_looper.escript \
    fixtures/os_daemon_configer.escript \
    fixtures/os_daemon_bad_perm.sh \
    fixtures/os_daemon_can_reboot.sh \
    fixtures/os_daemon_die_on_boot.sh \
    fixtures/os_daemon_die_quickly.sh \
    fixtures/logo.png \
    fixtures/3b835456c235b1827e012e25666152f3.view \
    fixtures/test.couch

EXTRA_DIST = \
    run.in \
    eunit.ini \
    $(eunit_files) \
    $(fixture_files)

all:
	@mkdir -p ebin/
	@mkdir -p temp/
	$(ERLC) -Wall -I$(top_srcdir)/src -I$(top_srcdir)/test/couchdb/include \
            -o $(top_builddir)/test/couchdb/ebin/ $(ERLC_FLAGS) ${TEST} \
            $(top_srcdir)/test/couchdb/test_request.erl \
            $(top_srcdir)/test/couchdb/test_web.erl
	chmod +x run

clean-local:
	rm -rf ebin/
	rm -rf temp/
