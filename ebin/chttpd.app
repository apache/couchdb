{application, chttpd, [
    {description, "HTTP interface for CouchDB cluster"},
    {vsn, "1.0"},
    {modules, [
        chttpd,
        chttpd_app,
        chttpd_auth,
        chttpd_db,
        chttpd_external,
        chttpd_misc,
        chttpd_oauth,
        chttpd_show,
        chttpd_stats,
        chttpd_sup,
        chttpd_view,
        cloudant_auth
    ]},
    {registered, [chttpd_sup, chttpd]},
    {applications, [kernel, stdlib, couch, fabric]},
    {mod, {chttpd_app,[]}}
]}.