%% fabric app resource file

{application, fabric,
 [{description, "clustered couchdb functions"},
  {vsn, "0.1.0"},
  {modules, [
       fabric,
       fabric_api,
       fabric_db,
       fabric_doc,
       fabric_rpc,
       fabric_util
   ]},
  {registered, []},
  {included_applications, []},
  {applications, [kernel, stdlib, couch, rexi, dynomite]},
  {start_phases, []}
 ]}.
