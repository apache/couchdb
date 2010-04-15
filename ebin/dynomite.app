%% dynomite app resource file

{application, dynomite,
  [{description, "Dynomite Clustering System"},
   {mod, {dynomite_app, []}},
   {vsn, "0.9.5-cloudant"},
   {modules,
   [
      bootstrap_manager,
      bootstrap_receiver,
      cluster_ops,
      configuration,
      dynomite,
      dynomite_app,
      dynomite_couch_api,
      dynomite_couch_storage,
      dynomite_prof,
      dynomite_sup,
      lib_misc,
      mem3,
      mem_utils,
      membership2,
      node,
      partitions,
      replication,
      vector_clock
    ]},
    {registered, [membership]},
    {applications, [kernel, stdlib, sasl, crypto, mochiweb]}
  ]}.
