{application, rexi, [
    {description, "lightweight RPC server"},
    {vsn, "1.0"},
    {modules, [rexi, rexi_app, rexi_sup, rexi_monitor, rexi_server]},
    {registered, [rexi_sup, rexi_server]},
    {applications, [kernel, stdlib]},
    {mod, {rexi_app,[]}},
    {start_phases, []}
]}.