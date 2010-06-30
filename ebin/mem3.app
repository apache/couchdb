{application, mem3, [
    {description, "CouchDB Cluster Membership"},
    {mod, {mem3_app, []}},
    {vsn, "0.9.6"},
    {modules, [
        mem3,
        mem3_app,
        mem3_cache,
        mem3_event,
        mem3_httpd,
        mem3_server,
        mem3_sup,
        mem3_sync,
        mem3_util,
        mem3_vclock
    ]},
    {registered, [
        mem3_cache,
        mem3_event,
        mem3_server,
        mem3_sync,
        mem3_sup
    ]},
    {applications, [kernel, stdlib, sasl, crypto, mochiweb, couch]}
]}.
