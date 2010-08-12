{application, mem3, [
    {description, "CouchDB Cluster Membership"},
    {mod, {mem3_app, []}},
    {vsn, "1.0.1"},
    {modules, [
        mem3,
        mem3_app,
        mem3_cache,
        mem3_httpd,
        mem3_nodes,
        mem3_sup,
        mem3_sync,
        mem3_sync_event,
        mem3_util
    ]},
    {registered, [
        mem3_cache,
        mem3_events,
        mem3_nodes,
        mem3_sync,
        mem3_sup
    ]},
    {applications, [kernel, stdlib, sasl, crypto, mochiweb, couch]}
]}.
