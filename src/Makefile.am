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

SUBDIRS = \
    couch_dbupdates \
    couch_index \
    couch_mrview \
    couch_plugins \
    couch_replicator \
    couchdb \
    ejson \
    erlang-oauth \
    ibrowse \
    mochiweb \
    snappy \
    couchjs-node

EXTRA_DIST = \
    $(MY_FIRST_COUCHDB_PLUGIN_FILES) \
    $(FAUXTON_FILES)

MY_FIRST_COUCHDB_PLUGIN_FILES = \
  my-first-couchdb-plugin/.gitignore \
  my-first-couchdb-plugin/Makefile \
  my-first-couchdb-plugin/priv/default.d/my_first_couchdb_plugin.ini \
  my-first-couchdb-plugin/README.md \
  my-first-couchdb-plugin/src/my_first_couchdb_plugin.app.src \
  my-first-couchdb-plugin/src/my_first_couchdb_plugin.erl \
  my-first-couchdb-plugin/test/my_first_couchdb_plugin_tests.erl
