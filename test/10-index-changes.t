#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./deps/*/ebin -pa ./apps/*/ebin -pa ./test/etap

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

main(_) ->
    etap:plan(6),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            etap:bail(Other)
    end,
    timer:sleep(300),
    ok.

test() ->
    test_util:start_couch(),
    {ok, Db} = couch_mrview_test_util:init_db(<<"foo">>, changes),
    test_normal_changes(Db),
    test_stream_once(Db),
    test_stream_once_since(Db),
    test_stream_once_timeout(Db),
    test_stream_once_heartbeat(Db),
    test_stream(Db),
    test_util:stop_couch(),
    ok.

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
    etap:is(Result, Expect, "normal changes worked.").

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
    etap:is(Result, Expect, "stream once since 0 worked.").


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
            etap:is(Result, Expect, "normal changes worked.")
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
            etap:is(Result, Expect, "got timeout.")
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
            etap:is(Result, Expect, "heartbeat OK.")
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
            etap:is(Result, Expect, "stream OK.")
    after 5000 ->
            io:format("never got the change", [])
    end.


save_doc(Db, Id) ->
    Doc = couch_mrview_test_util:doc(Id),
    {ok, _Rev} = couch_db:update_doc(Db, Doc, []),
    {ok, _} =  couch_db:ensure_full_commit(Db),
    couch_db:reopen(Db).

run_query(Db, Opts) ->
    Fun = fun
        (stop, {LastSeq, Acc}) ->
            {ok, LastSeq, Acc};
        (heartbeat, Acc) ->
            {ok, [heartbeat | Acc]};
        (Event, Acc) ->
            {ok, [Event | Acc]}
    end,
    couch_mrview:refresh(Db, <<"_design/bar">>),
    {ok, LastSeq, R} = couch_mrview_changes:handle_changes(Db, <<"_design/bar">>,
                                                  <<"baz">>, Fun, [], Opts),
    {ok, LastSeq, lists:reverse(R)}.
