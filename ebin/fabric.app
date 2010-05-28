%% fabric app resource file

{application, fabric,
 [{description, "clustered couchdb functions"},
  {vsn, "0.1.0"},
  {modules, [
       fabric,
       fabric_db,
       fabric_doc,
       fabric_missing_revs,
       fabric_open_doc,
       fabric_open_revs,
       fabric_rpc,
       fabric_update_docs,
       fabric_util
   ]},
  {registered, []},
  {included_applications, []},
  {applications, [kernel, stdlib, couch, rexi, dynomite]},
  {start_phases, []}
 ]}.
