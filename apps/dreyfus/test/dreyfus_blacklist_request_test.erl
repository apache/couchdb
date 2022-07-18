% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(dreyfus_blacklist_request_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_log/include/couch_log.hrl").
-include_lib("dreyfus/include/dreyfus.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(TIMEOUT, 1000).

start() ->
    test_util:start_couch([dreyfus]),
    ok = meck:new(fabric, [passthrough]),
    ok = meck:expect(fabric, open_doc, fun(_, _, _) ->
        {ok, ddoc}
    end).

stop(_) ->
    ok = meck:unload(fabric),
    test_util:stop_couch([dreyfus]).

setup() ->
    ok.

teardown(_) ->
    ok.

dreyfus_blacklist_request_test_() ->
    {
        "dreyfus blacklist request tests",
        {
            setup,
            fun start/0,
            fun stop/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun deny_fabric_requests/0,
                    fun allow_fabric_request/0
                ]
            }
        }
    }.

deny_fabric_requests() ->
    Reason = <<"Index <mydb, myddocid, myindexname>, is BlackListed">>,
    QueryArgs = #index_query_args{},
    IndexQueryArgs = #index_query_args{},
    DDoc = #doc{id = <<"_design/myddocid">>},
    Denied = "mydb.myddocid.myindexname",
    config:set("dreyfus_blacklist", Denied, "true"),
    dreyfus_test_util:wait_config_change(Denied, "true"),
    ?assertThrow(
        {bad_request, Reason},
        dreyfus_fabric_search:go(
            <<"mydb">>,
            <<"myddocid">>,
            <<"myindexname">>,
            QueryArgs
        )
    ),
    ?assertThrow(
        {bad_request, Reason},
        dreyfus_fabric_group1:go(
            <<"mydb">>,
            <<"myddocid">>,
            <<"myindexname">>,
            QueryArgs
        )
    ),
    ?assertThrow(
        {bad_request, Reason},
        dreyfus_fabric_group2:go(
            <<"mydb">>,
            <<"myddocid">>,
            <<"myindexname">>,
            QueryArgs
        )
    ),
    ?assertThrow(
        {bad_request, Reason},
        dreyfus_fabric_info:go(
            <<"mydb">>,
            <<"myddocid">>,
            <<"myindexname">>,
            QueryArgs
        )
    ),
    ?assertThrow(
        {bad_request, Reason},
        dreyfus_fabric_search:go(
            <<"mydb">>,
            DDoc,
            <<"myindexname">>,
            IndexQueryArgs
        )
    ),
    ?assertThrow(
        {bad_request, Reason},
        dreyfus_fabric_group1:go(
            <<"mydb">>,
            DDoc,
            <<"myindexname">>,
            IndexQueryArgs
        )
    ),
    ?assertThrow(
        {bad_request, Reason},
        dreyfus_fabric_group2:go(
            <<"mydb">>,
            DDoc,
            <<"myindexname">>,
            IndexQueryArgs
        )
    ),
    ?assertThrow(
        {bad_request, Reason},
        dreyfus_fabric_info:go(
            <<"mydb">>,
            DDoc,
            <<"myindexname">>,
            IndexQueryArgs
        )
    ).

allow_fabric_request() ->
    ok = meck:new(dreyfus_fabric_search, [passthrough]),
    ok = meck:expect(
        dreyfus_fabric_search,
        go,
        fun(A, GroupId, B, C) when is_binary(GroupId) ->
            meck:passthrough([A, GroupId, B, C])
        end
    ),
    ok = meck:expect(dreyfus_fabric_search, go, fun(_, _, _, _) ->
        ok
    end),
    Denied = "mydb2.myddocid2.myindexname2",
    QueryArgs = #index_query_args{},
    config:set("dreyfus_blacklist", Denied, "true"),
    dreyfus_test_util:wait_config_change(Denied, "true"),
    ?assertEqual(
        ok,
        dreyfus_fabric_search:go(
            <<"mydb">>,
            <<"myddocid">>,
            <<"indexnotthere">>,
            QueryArgs
        )
    ),
    ok = meck:unload(dreyfus_fabric_search).
