#!/usr/bin/env escript
%% -*- erlang -*-

default_config() ->
    "etc/couchdb/default_dev.ini".

main(_) ->
    code:add_pathz("src/couchdb"),
    etap:plan(5),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    ok.

test() ->
    couch_config:start_link([default_config()]),

    etap:is(
        couch_config:get("httpd", "port"),
        "5984",
        "{httpd, port} is 5984 by default."
    ),
    
    ok = couch_config:set("httpd", "port", "4895", false),
    
    etap:is(
        couch_config:get("httpd", "port"),
        "4895",
        "{httpd, port} changed to 4895"
    ),

    SentinelFunc = fun() ->
        % Ping/Pong to make sure we wait for this
        % process to die
        receive {ping, From} -> From ! pong end
    end,
    SentinelPid = spawn(SentinelFunc),

    couch_config:register(
        fun("httpd", "port", Value) ->
            etap:is(Value, "8080", "Registered function got notification.")
        end,
        SentinelPid
    ),
    
    ok = couch_config:set("httpd", "port", "8080", false),
    
    % Implicitly checking that we *don't* call the function
    etap:is(
        couch_config:get("httpd", "bind_address"),
        "127.0.0.1",
        "{httpd, bind_address} is not '0.0.0.0'"
    ),
    ok = couch_config:set("httpd", "bind_address", "0.0.0.0", false),
    
    % Ping-Pong kill process
    SentinelPid ! {ping, self()},
    receive
        _Any -> ok
    after 1000 ->
        throw({timeout_error, registered_pid})
    end,

    ok = couch_config:set("httpd", "port", "80", false),
    etap:is(
        couch_config:get("httpd", "port"),
        "80",
        "Implicitly test that the function got de-registered"
    ),
   
    ok.