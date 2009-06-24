#!/usr/bin/env escript
%% -*- erlang -*-

default_config() ->
    "etc/couchdb/default_dev.ini".

main(_) ->
    code:add_pathz("src/couchdb"),
    etap:plan(3),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_config:start_link([]),

    etap:fun_is(
        fun(KVPairs) -> length(KVPairs) == 0 end,
        couch_config:all(),
        "No INI files specified returns 0 key/value pairs."
    ),
    
    ok = couch_config:set("httpd", "port", "80", false),
    
    etap:is(
        couch_config:get("httpd", "port"),
        "80",
        "Created a new non-persisted k/v pair."
    ),

    ok = couch_config:set("httpd", "bind_address", "127.0.0.1"),
    etap:is(
        couch_config:get("httpd", "bind_address"),
        "127.0.0.1",
        "Asking for a persistent key/value pair doesn't choke."
    ),

    ok.