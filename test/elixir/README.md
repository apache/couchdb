# Elixir Test Suite

Proof of concept porting the JS test suite to Elixir.

Currently the basics.js suite has been partially ported over.

To run the suite:

```
mix deps.get
mix test --trace
```

# Tests to port

X means done, - means partially

  - [X] Port all_docs.js
  - [ ] Port attachment_names.js
  - [ ] Port attachment_paths.js
  - [ ] Port attachment_ranges.js
  - [ ] Port attachments.js
  - [ ] Port attachments_multipart.js
  - [ ] Port attachment_views.js
  - [ ] Port auth_cache.js
  - [X] Port basics.js
  - [ ] Port batch_save.js
  - [ ] Port bulk_docs.js
  - [X] Port changes.js
  - [ ] Port coffee.js
  - [ ] Port compact.js
  - [X] Port config.js
  - [ ] Port conflicts.js
  - [ ] Port cookie_auth.js
  - [ ] Port copy_doc.js
  - [ ] Port delayed_commits.js
  - [ ] Port design_docs.js
  - [ ] Port design_options.js
  - [ ] Port design_paths.js
  - [ ] Port erlang_views.js
  - [ ] Port etags_head.js
  - [ ] Port etags_views.js
  - [ ] Port form_submit.js
  - [ ] Port http.js
  - [ ] Port invalid_docids.js
  - [ ] Port jsonp.js
  - [X] Port large_docs.js
  - [ ] Port list_views.js
  - [ ] Port lorem_b64.txt
  - [ ] Port lorem.txt
  - [X] Port lots_of_docs.js
  - [ ] Port method_override.js
  - [ ] Port multiple_rows.js
  - [ ] Port proxyauth.js
  - [ ] Port purge.js
  - [ ] Port reader_acl.js
  - [ ] Port recreate_doc.js
  - [ ] Port reduce_builtin.js
  - [ ] Port reduce_false.js
  - [ ] Port reduce_false_temp.js
  - [X] Port reduce.js
  - [-] Port replication.js
  - [ ] Port replicator_db_bad_rep_id.js
  - [ ] Port replicator_db_by_doc_id.js
  - [ ] Port replicator_db_compact_rep_db.js
  - [ ] Port replicator_db_continuous.js
  - [ ] Port replicator_db_credential_delegation.js
  - [ ] Port replicator_db_field_validation.js
  - [ ] Port replicator_db_filtered.js
  - [ ] Port replicator_db_identical_continuous.js
  - [ ] Port replicator_db_identical.js
  - [ ] Port replicator_db_invalid_filter.js
  - [ ] Port replicator_db_security.js
  - [ ] Port replicator_db_simple.js
  - [ ] Port replicator_db_successive.js
  - [ ] Port replicator_db_survives.js
  - [ ] Port replicator_db_swap_rep_db.js
  - [ ] Port replicator_db_update_security.js
  - [ ] Port replicator_db_user_ctx.js
  - [ ] Port replicator_db_write_auth.js
  - [ ] Port rev_stemming.js
  - [X] Port rewrite.js
  - [ ] Port rewrite_js.js
  - [ ] Port security_validation.js
  - [ ] Port show_documents.js
  - [ ] Port stats.js
  - [ ] Port update_documents.js
  - [ ] Port users_db.js
  - [ ] Port users_db_security.js
  - [ ] Port utf8.js
  - [X] Port uuids.js
  - [X] Port view_collation.js
  - [ ] Port view_collation_raw.js
  - [ ] Port view_compaction.js
  - [ ] Port view_conflicts.js
  - [ ] Port view_errors.js
  - [ ] Port view_include_docs.js
  - [ ] Port view_multi_key_all_docs.js
  - [ ] Port view_multi_key_design.js
  - [ ] Port view_multi_key_temp.js
  - [ ] Port view_offsets.js
  - [ ] Port view_pagination.js
  - [ ] Port view_sandboxing.js
  - [ ] Port view_update_seq.js
