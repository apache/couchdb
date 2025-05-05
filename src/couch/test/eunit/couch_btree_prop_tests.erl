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

-module(couch_btree_prop_tests).

-ifdef(WITH_PROPER).

-export([
    command/1,
    initial_state/0,
    next_state/3,
    precondition/2,
    postcondition/3,
    query_modify/3,
    lookup/1
]).

% Process dict keys
-define(BTREE, btree).
-define(BTREE_FILENAME, btree_filename).

-include_lib("couch/include/couch_eunit_proper.hrl").
-include_lib("couch/include/couch_eunit.hrl").

btree_property_test_() ->
    ?EUNIT_QUICKCHECK(90, 3000).

%
% Properties
%

prop_btree_() ->
    ?FORALL(
        Cmds,
        commands(?MODULE),
        begin
            setup_btree(),
            {Hist, St, Res} = run_commands(?MODULE, Cmds),
            cleanup_btree(),
            ?WHENFAIL(
                on_failure(Hist, St, Res),
                aggregate(command_names(Cmds), Res =:= ok)
            )
        end
    ).

%
% Setup, teardown and proxy calls to couch_btree.
%

% PropEr is a bit awkward when it comes to test setup and teardown, so to make
% everything easier use plain function calls in the test and keep the btree
% state in the process dictionary.

setup_btree() ->
    Filename = ?tempfile(),
    {ok, Fd} = couch_file:open(Filename, [create, overwrite]),
    {ok, Bt} = couch_btree:open(nil, Fd, [{compression, none}]),
    put(?BTREE, Bt),
    put(?BTREE_FILENAME, Filename).

cleanup_btree() ->
    Bt = get(?BTREE),
    Fd = couch_btree:get_fd(Bt),
    ok = couch_file:close(Fd),
    ok = file:delete(get(?BTREE_FILENAME)),
    erase(?BTREE),
    erase(?BTREE_FILENAME).

get_btree() ->
    Bt = get(?BTREE),
    % Sanity check. The btree didn't somehow disappear.
    true = couch_btree:is_btree(Bt),
    Bt.

update_btree(NewBt) ->
    OldBt = get(?BTREE),
    % Sanity check. Expect an old btree instance to exist.
    true = couch_btree:is_btree(OldBt),
    true = couch_btree:is_btree(NewBt),
    put(?BTREE, NewBt),
    ok.

% add/2 and add_remove/3 call query_modify/4, so just test that
%
query_modify(Lookups, Inserts, Removes) ->
    Bt = get_btree(),
    {ok, QueryResults, Bt1} = couch_btree:query_modify(Bt, Lookups, Inserts, Removes),
    ok = update_btree(Bt1),
    {ok, QueryResults, Bt1}.

lookup(Keys) ->
    Bt = get_btree(),
    couch_btree:lookup(Bt, Keys).

foldl() ->
    Fun = fun({K, V}, Acc) -> {ok, [{K, V} | Acc]} end,
    {ok, _, Acc1} = couch_btree:foldl(get_btree(), Fun, []),
    lists:reverse(Acc1).

%
% PropEr state callbacks
%

% Model the btree as a simple orddict.
%
initial_state() ->
    orddict:new().

command(Model) when is_list(Model) ->
    frequency([
        {1, {call, ?MODULE, query_modify, [keys(), kvs(), remove_keys()]}},
        {1, {call, ?MODULE, lookup, [keys()]}}
    ]).

% These are called during command generation, the test with an actual model and
% during shrinking to guide the shrinking behavior.
%
precondition(_Model, {call, ?MODULE, query_modify, [_, _, _]}) ->
    true;
precondition(Model, {call, ?MODULE, lookup, [Keys]}) ->
    % Avoid exploring too many useless cases and only look up keys if we know
    % we have some keys to look at
    orddict:size(Model) > 0 andalso length(Keys) > 0.

% Assuming the postcondition passed, advance to the next state
%
next_state(Model, _, {call, ?MODULE, query_modify, [_Lookups, Inserts, Removes]}) ->
    model_add_remove(Model, Inserts, Removes);
next_state(Model, _, {call, ?MODULE, lookup, [_]}) ->
    Model.

% The Model is *before* the call is applied. The Result is *after* the command
% was applied to the actual btree we're testing. This is where the "model" vs
% "real" btree check happens.
%
postcondition(Model, {call, ?MODULE, query_modify, [Lookups, Inserts, Removes]}, Result) ->
    {ok, QueryResults, _Bt} = Result,
    ModelUpdate = model_add_remove(Model, Inserts, Removes),
    ModelLookup = model_query(Model, Lookups),
    ModelLookup == lists:sort(QueryResults) andalso ModelUpdate == foldl();
postcondition(Model, {call, ?MODULE, lookup, [Lookups]}, Result) ->
    model_lookup(Model, Lookups) == Result.

%
% Generators
%

key() ->
    integer(1, 1000).

val() ->
    elements([a, b, c]).

kvs() ->
    list({key(), val()}).

keys() ->
    list(key()).

remove_keys() ->
    % Bias a bit towards not removing keys
    frequency([{4, []}, {1, keys()}]).

%
% Helper functions
%

% Model (orddict) helpers

model_add_remove(Model, Inserts, Removes) ->
    % Keep this in sync with the op_order/1 from the
    % couch_btree model:
    %  op_order(fetch) -> 1;
    %  op_order(remove) -> 2;
    %  op_order(insert) -> 3.
    Model1 = model_remove(Model, Removes),
    Model2 = model_insert(Model1, Inserts),
    Model2.

model_insert(Model, KVs) ->
    UsortFun = fun({K1, _}, {K2, _}) -> K1 =< K2 end,
    FoldFun = fun({K, V}, M) -> orddict:store(K, V, M) end,
    lists:foldl(FoldFun, Model, lists:usort(UsortFun, KVs)).

model_remove(Model, Keys) ->
    FoldFun = fun(K, M) -> orddict:erase(K, M) end,
    lists:foldl(FoldFun, Model, lists:usort(Keys)).

model_lookup(Model, Keys) ->
    Fun =
        fun(K) ->
            case orddict:find(K, Model) of
                {ok, V} -> {ok, {K, V}};
                error -> not_found
            end
        end,
    lists:map(Fun, Keys).

model_query(Model, Keys) ->
    Fun =
        fun(K) ->
            case orddict:find(K, Model) of
                {ok, V} -> {ok, {K, V}};
                error -> {not_found, {K, nil}}
            end
        end,
    lists:sort(lists:map(Fun, Keys)).

% Other helpers

on_failure(History, St, Res) ->
    Msg = "~nWHENFAIL: History: ~p\nState: ~p\nResult: ~p\n",
    io:format(standard_error, Msg, [History, St, Res]).

-endif.
