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

-module(couch_mrview_index_changes_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), map),
    Db.

teardown(Db) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok.

changes_index_test() ->
    {
        "changes index tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun test_normal_changes/1,
                    fun test_stream_once/1,
                    fun test_stream_once_since/1,
                    fun test_stream_once_timeout/1,
                    fun test_stream_once_heartbeat/1,
                    fun test_stream/1,
                    fun test_indexer/1
                ]
            }
        }
    }.


test_normal_changes(Db) ->
    Result = run_query(Db, []),
    Expect = {ok, 11, [
                {{2, 1, <<"1">>}, 1},
                {{3, 10, <<"10">>}, 10},
                {{4, 2, <<"2">>}, 2},
                {{5, 3, <<"3">>}, 3},
                {{6, 4, <<"4">>}, 4},
                {{7, 5, <<"5">>}, 5},
                {{8, 6, <<"6">>}, 6},
                {{9, 7, <<"7">>}, 7},
                {{10, 8, <<"8">>}, 8},
                {{11, 9, <<"9">>}, 9}
    ]},
    ?_assertEqual(Result, Expect).

test_stream_once(Db) ->
    Result = run_query(Db, [{stream, once}]),
    Expect = {ok, 11, [
                {{2, 1, <<"1">>}, 1},
                {{3, 10, <<"10">>}, 10},
                {{4, 2, <<"2">>}, 2},
                {{5, 3, <<"3">>}, 3},
                {{6, 4, <<"4">>}, 4},
                {{7, 5, <<"5">>}, 5},
                {{8, 6, <<"6">>}, 6},
                {{9, 7, <<"7">>}, 7},
                {{10, 8, <<"8">>}, 8},
                {{11, 9, <<"9">>}, 9}
    ]},
    ?_assertEqual(Result, Expect).


test_stream_once_since(Db) ->
    Self = self(),
    spawn(fun() ->
                Result = run_query(Db, [{since, 11},
                                        {stream, once}]),
                Self ! {result, Result}
        end),

    spawn(fun() ->
                timer:sleep(1000),
                {ok, Db1} = save_doc(Db, 11),
                couch_mrview:refresh(Db1, <<"_design/bar">>)
        end),

    Expect = {ok,12,[{{12,11,<<"11">>},11}]},

    receive
        {result, Result} ->
            ?_assertEqual(Result, Expect)
    after 5000 ->
            io:format("never got the change", [])
    end.


test_stream_once_timeout(Db) ->
    Self = self(),
    spawn(fun() ->
                Result = run_query(Db, [{since, 12},
                                        {stream, once},
                                        {timeout, 3000}]),
                Self ! {result, Result}
        end),



    Expect = {ok, 12, []},

    receive
        {result, Result} ->
            ?_assertEqual(Result, Expect)
    after 5000 ->
            io:format("never got the change", [])
    end.

test_stream_once_heartbeat(Db) ->
    Self = self(),
    spawn(fun() ->
                Result = run_query(Db, [{since, 12},
                                        {stream, once},
                                        {heartbeat, 1000}]),
                Self ! {result, Result}
        end),

    spawn(fun() ->
                timer:sleep(3000),
                {ok, Db1} = save_doc(Db, 12),
                couch_mrview:refresh(Db1, <<"_design/bar">>)
        end),

    Expect = {ok,13,[heartbeat,
                     heartbeat,
                     heartbeat,
                     {{13,12,<<"12">>},12}]},



    receive
        {result, Result} ->
            ?_assertEqual(Result, Expect)
    after 5000 ->
            io:format("never got the change", [])
    end.


test_stream(Db) ->
    Self = self(),
    spawn(fun() ->
                Result = run_query(Db, [{since, 13},
                                        stream,
                                        {timeout, 3000}]),
                Self ! {result, Result}
        end),

    spawn(fun() ->
                timer:sleep(1000),
                {ok, Db1} = save_doc(Db, 13),
                couch_mrview:refresh(Db1, <<"_design/bar">>),
                {ok, Db2} = save_doc(Db1, 14),
                couch_mrview:refresh(Db2, <<"_design/bar">>)
        end),

    Expect = {ok, 15,[{{14,13,<<"13">>},13},
                     {{15,14,<<"14">>},14}]},

    receive
        {result, Result} ->
            ?_assertEqual(Result, Expect)
    after 5000 ->
            io:format("never got the change", [])
    end.


test_indexer(Db) ->
    Result = run_query(Db, [{since, 14}, refresh]),
    Expect = {ok, 15, [{{15,14,<<"14">>},14}]},
    ?_assertEqual(Result, Expect),

    {ok, Db1} = save_doc(Db, 15),
    timer:sleep(1500),
    Result1 = run_query(Db1, [{since, 14}], false),
    Expect1 = {ok, 16, [{{15,14,<<"14">>},14},
                       {{16,15,<<"15">>},15}]},
    ?_assertEqual(Result1, Expect1),
    ok.


save_doc(Db, Id) ->
    Doc = couch_mrview_test_util:doc(Id),
    {ok, _Rev} = couch_db:update_doc(Db, Doc, []),
    {ok, _} =  couch_db:ensure_full_commit(Db),
    couch_db:reopen(Db).

run_query(Db, Opts) ->
    run_query(Db, Opts, true).

run_query(Db, Opts, Refresh) ->
    Fun = fun
        (stop, {LastSeq, Acc}) ->
            {ok, LastSeq, Acc};
        (heartbeat, Acc) ->
            {ok, [heartbeat | Acc]};
        (Event, Acc) ->
            {ok, [Event | Acc]}
    end,
    case Refresh of
        true ->
            couch_mrview:refresh(Db, <<"_design/bar">>);
        false ->
            ok
    end,
    {ok, LastSeq, R} = couch_mrview_changes:handle_changes(Db, <<"_design/bar">>,
                                                  <<"baz">>, Fun, [], Opts),
    {ok, LastSeq, lists:reverse(R)}.
