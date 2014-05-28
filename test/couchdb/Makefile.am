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

all:
	mkdir -p ebin
	mkdir -p temp
	${ERLC} -oebin test_request.erl
	chmod +x run

eunit_files = \
    couch_auth_cache_tests.erl \
    couch_btree_tests.erl \
    couch_changes_tests.erl \
    couch_config_tests.erl \
    couch_db_tests.erl \
    couch_doc_json_tests.erl \
    couch_file_tests.erl \
    couch_key_tree_tests.erl \
    couch_ref_counter_tests.erl \
    couch_stream_tests.erl \
    couch_stats_tests.erl \
    couch_task_status_tests.erl \
    couch_util_tests.erl \
    couch_uuids_tests.erl \
    couch_work_queue_tests.erl \
    couchdb_attachments_tests.erl \
    couchdb_file_compression_tests.erl \
    couchdb_modules_load_tests.erl \
    couchdb_update_conflicts_tests.erl \
    couchdb_views_tests.erl \
    test_request.erl \
    couch_eunit.hrl

fixture_files = \
    fixtures/couch_config_tests_1.ini \
    fixtures/couch_config_tests_2.ini \
    fixtures/couch_stats_aggregates.cfg \
    fixtures/couch_stats_aggregates.ini

EXTRA_DIST = \
    run.in \
    eunit.ini \
    $(eunit_files) \
    $(fixture_files)

clean-local:
	rm -rf ebin
	rm -rf temp
