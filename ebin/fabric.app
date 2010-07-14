%% fabric app resource file

{application, fabric,
 [{description, "Routing and proxying layer for CouchDB cluster"},
  {vsn, "1.0"},
  {modules, [
       fabric,
       fabric_db_create,
       fabric_db_delete,
       fabric_db_doc_count,
       fabric_db_info,
       fabric_db_meta,
       fabric_dict,
       fabric_doc_attachments,
       fabric_doc_missing_revs,
       fabric_doc_open,
       fabric_doc_open_revs,
       fabric_doc_update,
       fabric_group_info,
       fabric_rpc,
       fabric_util,
       fabric_view,
       fabric_view_all_docs,
       fabric_view_changes,
       fabric_view_map,
       fabric_view_reduce
   ]},
  {applications, [kernel, stdlib, couch, rexi, mem3]}
 ]}.
