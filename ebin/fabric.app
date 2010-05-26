%% fabric app resource file

{application, fabric,
 [{description, "clustered couchdb functions"},
  {vsn, "0.1.0"},
  {modules, [
       fabric,
       fabric_api,
       fabric_create,
       fabric_delete,
       fabric_info,
       fabric_open,
       fabric_util
   ]},
  {registered, []},
  {included_applications, []},
  {applications, [kernel, stdlib, couch, rexi, dynomite]},
  {start_phases, []}
 ]}.
