{application, rexi, [
    {description, "Lightweight RPC server"},
    {vsn, "1.2"},
    {modules, [rexi,
               rexi_app,
               rexi_sup,
               rexi_monitor,
               rexi_server,
               rexi_utils]},
    {registered, [rexi_sup, rexi_server]},
    {applications, [kernel, stdlib]},
    {mod, {rexi_app,[]}}
]}.
