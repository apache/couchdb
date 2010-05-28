%% membership app resource file

{application, membership,
  [{description, "cluster membership"},
   {mod, {membership_app, []}},
   {vsn, "0.9.6"},
   {modules,
   [
      dbs,
      dbs_cache,
      membership,
      membership_app,
      membership_sup,
      mem3,
      partitions,
      vector_clock
    ]},
    {registered, [membership]},
    {applications, [kernel, stdlib, sasl, crypto, mochiweb]}
  ]}.
