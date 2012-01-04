// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.


CouchDB.urlPrefix = "..";
var couchTests = {};

function loadTest(file) {
  loadScript("script/test/"+file);
};
// keep first
loadTest("basics.js");

// keep sorted
loadTest("all_docs.js");
loadTest("attachments.js");
loadTest("attachments_multipart.js");
loadTest("attachment_names.js");
loadTest("attachment_paths.js");
loadTest("attachment_ranges.js");
loadTest("attachment_views.js");
loadTest("auth_cache.js");
loadTest("batch_save.js");
loadTest("bulk_docs.js");
loadTest("changes.js");
loadTest("coffee.js");
loadTest("compact.js");
loadTest("config.js");
loadTest("conflicts.js");
loadTest("content_negotiation.js");
loadTest("cookie_auth.js");
loadTest("copy_doc.js");
loadTest("delayed_commits.js");
loadTest("design_docs.js");
loadTest("design_options.js");
loadTest("design_paths.js");
loadTest("erlang_views.js");
loadTest("etags_head.js");
loadTest("etags_views.js");
loadTest("form_submit.js");
loadTest("http.js");
loadTest("invalid_docids.js");
loadTest("jsonp.js");
loadTest("large_docs.js");
loadTest("list_views.js");
loadTest("lots_of_docs.js");
loadTest("method_override.js");
loadTest("multiple_rows.js");
loadScript("script/oauth.js");
loadScript("script/sha1.js");
loadTest("oauth.js");
loadTest("oauth_users_db.js");
loadTest("proxyauth.js");
loadTest("purge.js");
loadTest("reader_acl.js");
loadTest("recreate_doc.js");
loadTest("reduce.js");
loadTest("reduce_builtin.js");
loadTest("reduce_false.js");
loadTest("reduce_false_temp.js");
loadTest("replication.js");
loadTest("replicator_db.js");
loadTest("replicator_db_security.js");
loadTest("rev_stemming.js");
loadTest("rewrite.js");
loadTest("security_validation.js");
loadTest("show_documents.js");
loadTest("stats.js");
loadTest("update_documents.js");
loadTest("users_db.js");
loadTest("users_db_security.js");
loadTest("utf8.js");
loadTest("uuids.js");
loadTest("view_collation.js");
loadTest("view_collation_raw.js");
loadTest("view_conflicts.js");
loadTest("view_compaction.js");
loadTest("view_errors.js");
loadTest("view_include_docs.js");
loadTest("view_multi_key_all_docs.js");
loadTest("view_multi_key_design.js");
loadTest("view_multi_key_temp.js");
loadTest("view_offsets.js");
loadTest("view_pagination.js");
loadTest("view_sandboxing.js");
loadTest("view_update_seq.js");
loadTest("view_xml.js");
// keep sorted

