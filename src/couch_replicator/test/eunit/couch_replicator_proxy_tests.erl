% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_replicator_proxy_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").
-include_lib("couch_replicator/include/couch_replicator_api_wrap.hrl").
-include_lib("fabric/test/fabric2_test.hrl").

replicator_proxy_test_() ->
    {
        "replicator proxy tests",
        {
            setup,
            fun couch_replicator_test_helper:start_couch/0,
            fun couch_replicator_test_helper:stop_couch/1,
            with([
                ?TDEF(parse_rep_doc_without_proxy),
                ?TDEF(parse_rep_doc_with_proxy),
                ?TDEF(parse_rep_source_target_proxy),
                ?TDEF(mutually_exclusive_proxy_and_source_proxy),
                ?TDEF(mutually_exclusive_proxy_and_target_proxy),
                ?TDEF(sock5_proxy_in_db_from_json)
            ])
        }
    }.

parse_rep_doc_without_proxy(_) ->
    NoProxyDoc =
        {[
            {<<"source">>, <<"http://unproxied.com">>},
            {<<"target">>, <<"http://otherunproxied.com">>}
        ]},
    Rep = couch_replicator_parse:parse_rep_doc(NoProxyDoc),
    Src = maps:get(?SOURCE, Rep),
    Tgt = maps:get(?TARGET, Rep),
    ?assertEqual(null, maps:get(<<"proxy_url">>, Src)),
    ?assertEqual(null, maps:get(<<"proxy_url">>, Tgt)).

parse_rep_doc_with_proxy(_) ->
    ProxyURL = <<"http://myproxy.com">>,
    ProxyDoc =
        {[
            {<<"source">>, <<"http://unproxied.com">>},
            {<<"target">>, <<"http://otherunproxied.com">>},
            {<<"proxy">>, ProxyURL}
        ]},
    Rep = couch_replicator_parse:parse_rep_doc(ProxyDoc),
    Src = maps:get(?SOURCE, Rep),
    Tgt = maps:get(?TARGET, Rep),
    ?assertEqual(ProxyURL, maps:get(<<"proxy_url">>, Src)),
    ?assertEqual(ProxyURL, maps:get(<<"proxy_url">>, Tgt)).

parse_rep_source_target_proxy(_) ->
    SrcProxyURL = <<"http://mysrcproxy.com">>,
    TgtProxyURL = <<"http://mytgtproxy.com:9999">>,
    ProxyDoc =
        {[
            {<<"source">>, <<"http://unproxied.com">>},
            {<<"target">>, <<"http://otherunproxied.com">>},
            {<<"source_proxy">>, SrcProxyURL},
            {<<"target_proxy">>, TgtProxyURL}
        ]},
    Rep = couch_replicator_parse:parse_rep_doc(ProxyDoc),
    Src = maps:get(?SOURCE, Rep),
    Tgt = maps:get(?TARGET, Rep),
    ?assertEqual(SrcProxyURL, maps:get(<<"proxy_url">>, Src)),
    ?assertEqual(TgtProxyURL, maps:get(<<"proxy_url">>, Tgt)).

mutually_exclusive_proxy_and_source_proxy(_) ->
    ProxyDoc =
        {[
            {<<"source">>, <<"http://unproxied.com">>},
            {<<"target">>, <<"http://otherunproxied.com">>},
            {<<"proxy">>, <<"oldstyleproxy.local">>},
            {<<"source_proxy">>, <<"sourceproxy.local">>}
        ]},
    ?assertThrow(
        {bad_rep_doc, _},
        couch_replicator_parse:parse_rep_doc(ProxyDoc)
    ).

mutually_exclusive_proxy_and_target_proxy(_) ->
    ProxyDoc =
        {[
            {<<"source">>, <<"http://unproxied.com">>},
            {<<"target">>, <<"http://otherunproxied.com">>},
            {<<"proxy">>, <<"oldstyleproxy.local">>},
            {<<"target_proxy">>, <<"targetproxy.local">>}
        ]},
    ?assertThrow(
        {bad_rep_doc, _},
        couch_replicator_parse:parse_rep_doc(ProxyDoc)
    ).

sock5_proxy_in_db_from_json(_) ->
    SrcProxyURL = <<"socks5://u1:p1@mysrcproxy.com:1234">>,
    TgtProxyURL = <<"http://u2:p2@mytgtproxy.com:9999">>,
    ProxyDoc =
        {[
            {<<"source">>, <<"http://unproxied.com">>},
            {<<"target">>, <<"http://otherunproxied.com">>},
            {<<"source_proxy">>, SrcProxyURL},
            {<<"target_proxy">>, TgtProxyURL}
        ]},
    Rep = couch_replicator_parse:parse_rep_doc(ProxyDoc),
    Src = maps:get(?SOURCE, Rep),
    Tgt = maps:get(?TARGET, Rep),
    ?assertEqual(SrcProxyURL, maps:get(<<"proxy_url">>, Src)),
    ?assertEqual(TgtProxyURL, maps:get(<<"proxy_url">>, Tgt)),
    SrcHttpDb = couch_replicator_api_wrap:db_from_json(Src),
    TgtHttpDb = couch_replicator_api_wrap:db_from_json(Tgt),
    SrcOpts = SrcHttpDb#httpdb.ibrowse_options,
    TgtOpts = TgtHttpDb#httpdb.ibrowse_options,
    ?assertEqual(undefined, couch_util:get_value(proxy_protocol, SrcOpts)),
    ?assertEqual(undefined, couch_util:get_value(proxy_protocol, TgtOpts)),
    ?assertEqual("mysrcproxy.com", couch_util:get_value(socks5_host, SrcOpts)),
    ?assertEqual("mytgtproxy.com", couch_util:get_value(proxy_host, TgtOpts)),
    ?assertEqual(1234, couch_util:get_value(socks5_port, SrcOpts)),
    ?assertEqual(9999, couch_util:get_value(proxy_port, TgtOpts)),
    ?assertEqual("u1", couch_util:get_value(socks5_user, SrcOpts)),
    ?assertEqual("u2", couch_util:get_value(proxy_user, TgtOpts)),
    ?assertEqual("p1", couch_util:get_value(socks5_password, SrcOpts)),
    ?assertEqual("p2", couch_util:get_value(proxy_password, TgtOpts)).
