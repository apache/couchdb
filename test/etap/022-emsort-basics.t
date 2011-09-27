#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./src/couchdb -sasl errlog_type error -boot start_sasl -noshell

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

filename() -> "./test/etap/temp.022".
group() -> 100.
rows() -> 1000.
total() -> group() * rows().

main(_) ->
    test_util:init_code_path(),
    etap:plan(4),
    case (catch test()) of
        ok ->
            etap:end_tests();
        Other ->
            etap:diag(io_lib:format("Test died abnormally: ~p", [Other])),
            timer:sleep(1000),
            etap:bail()
    end,
    ok.

test()->
    {ok, Fd} = couch_file:open(filename(), [create,overwrite]),

    test_basic(Fd),

    couch_file:close(Fd).

test_basic(Fd) ->
    {ok, Ems} = couch_emsort:open(Fd),
    Ems1 = lists:foldl(fun(_, EAcc0) ->
       KVs = [{random:uniform(), val_here} || _ <- lists:seq(1, group())],
       {ok, EAcc1} = couch_emsort:add(EAcc0, KVs),
       EAcc1
    end, Ems, lists:seq(1, rows())),

    {ok, Iter1} = couch_emsort:sort(Ems1),
    {Bad1, Total1} = read_sorted(Iter1, {-1, 0, 0}),
    etap:is(Bad1, 0, "No rows out of order."),
    etap:is(Total1, total(), "Found exactly as many rows as expected."),

    {ok, Ems2} = couch_emsort:merge(Ems1),
    {ok, Iter2} = couch_emsort:iter(Ems2),
    {Bad2, Total2} = read_sorted(Iter2, {-1, 0, 0}),
    etap:is(Bad2, 0, "No rows out of order."),
    etap:is(Total2, total(), "Found exactly as many rows as expected."),

    ok.

read_sorted(Iter, {Prev, Bad, Count}) ->
    case couch_emsort:next(Iter) of
        {ok, {K, _}, NextIter} when K > Prev ->
            read_sorted(NextIter, {K, Bad, Count+1});
        {ok, {K, _}, NextIter} ->
            read_sorted(NextIter, {K, Bad+1, Count+1});
        finished ->
            {Bad, Count}
    end.

