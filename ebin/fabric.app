%% fabric app resource file

{application, fabric,
 [{description, "clustered couchdb functions"},
  {vsn, "0.1.0"},
  {modules, [
       fabric,
       fabric_all_databases,
       fabric_all_docs,
       fabric_create_db,
       fabric_delete_db,
       fabric_doc_count,
       fabric_get_db_info,
       fabric_missing_revs,
       fabric_open_doc,
       fabric_open_revs,
       fabric_rpc,
       fabric_update_docs,
       fabric_util
   ]},
  {registered, []},
  {included_applications, []},
  {applications, [kernel, stdlib, couch, rexi, membership]},
  {start_phases, []}
 ]}.
