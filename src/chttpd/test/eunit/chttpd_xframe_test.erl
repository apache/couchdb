-module(chttpd_xframe_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

setup_all() ->
    ok = meck:new(config),
    ok = meck:expect(config, get, fun(_, _, _) -> "X-Forwarded-Host" end),
    ok.

teardown_all(_) ->
    meck:unload().

setup() ->
    meck:reset([config]).

teardown(_) ->
    ok.

mock_request() ->
    Headers = mochiweb_headers:make([{"Host", "examples.com"}]),
    MochiReq = mochiweb_request:new(nil, 'GET', '/', {1, 1}, Headers),
    #httpd{mochi_req = MochiReq}.

config_disabled() ->
    [
        {enabled, false}
    ].

config_sameorigin() ->
    [
        {enabled, true},
        {same_origin, true}
    ].

config_wildcard() ->
    [
        {enabled, true},
        {same_origin, false},
        {hosts, ["*"]}
    ].

config_specific_hosts() ->
    [
        {enabled, true},
        {same_origin, false},
        {hosts, ["http://couchdb.org", "http://examples.com"]}
    ].

config_diffent_specific_hosts() ->
    [
        {enabled, true},
        {same_origin, false},
        {hosts, ["http://couchdb.org"]}
    ].

no_header_if_xframe_disabled_test() ->
    Headers = chttpd_xframe_options:header(mock_request(), [], config_disabled()),
    ?assertEqual(Headers, []).

enabled_with_same_origin_test() ->
    Headers = chttpd_xframe_options:header(mock_request(), [], config_sameorigin()),
    ?assertEqual(Headers, [{"X-Frame-Options", "SAMEORIGIN"}]).

xframe_host_test_() ->
    {
        "xframe host tests",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun allow_with_wildcard_host/1,
                    fun allow_with_specific_host/1,
                    fun deny_with_different_host/1
                ]
            }
        }
    }.

allow_with_wildcard_host(_) ->
    Headers = chttpd_xframe_options:header(mock_request(), [], config_wildcard()),
    ?_assertEqual([{"X-Frame-Options", "ALLOW-FROM http://examples.com"}], Headers).

allow_with_specific_host(_) ->
    Headers = chttpd_xframe_options:header(mock_request(), [], config_specific_hosts()),
    ?_assertEqual([{"X-Frame-Options", "ALLOW-FROM http://examples.com"}], Headers).

deny_with_different_host(_) ->
    Headers = chttpd_xframe_options:header(mock_request(), [], config_diffent_specific_hosts()),
    ?_assertEqual([{"X-Frame-Options", "DENY"}], Headers).
