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

-module(ioq_kv_tests).
-ifdef(WITH_PROPER).
-behaviour(proper_statem).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([
    initial_state/0,
    command/1,
    precondition/2,
    postcondition/3,
    next_state/3,
    random_key/1
]).

-record(st, {kvs}).

proper_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 5},
        {numtests, 1000}
    ],
    {timeout, 3600, ?_assertEqual([], proper:module(?MODULE, PropErOpts))}.


prop_ioq_kvs_almost_any() ->
    ?FORALL({K, V}, kvs(), begin
        case (catch ioq_kv:put(K, V)) of
            ok -> ioq_kv:get(K) == V;
            {'EXIT', {invalid_term, _}} -> true;
            _ -> false
        end
    end).


prop_ioq_kvs() ->
    ?FORALL(Cmds, commands(?MODULE),
        begin
            cleanup(),
            {H, S, R} = run_commands(?MODULE, Cmds),
            ?WHENFAIL(
                io:format("History: ~p\nState: ~p\nRes: ~p\n", [H,S,R]),
                R =:= ok
            )
        end
    ).

initial_state() ->
    #st{kvs=dict:new()}.


command(S) ->
    Key = {call, ioq_kv_tests, random_key, [S]},
    frequency([
        {1, {call, ioq_kv, init, []}},
        {9, {call, ioq_kv, get, [Key]}},
        {1, {call, ioq_kv, get, [key()]}},
        {9, {call, ioq_kv, put, [Key, val()]}},
        {1, {call, ioq_kv, put, [key(), val()]}},
        {2, {call, ioq_kv, delete, [Key]}},
        {1, {call, ioq_kv, delete, [key()]}}
    ]).


precondition(_, _) ->
    true.


postcondition(_S, {call, _, init, []}, ok) ->
    true;
postcondition(S, {call, _, get, [Key]}, Val) ->
    case dict:is_key(Key, S#st.kvs) of
        true ->
            case dict:find(Key, S#st.kvs) of
                {ok, Val} -> true;
                _ -> false
            end;
        false ->
            case Val of
                undefined -> true;
                _ -> false
            end
    end;
postcondition(_S, {call, _, put, [_Key, _Val]}, ok) ->
    true;
postcondition(_S, {call, _, delete, [_Key]}, ok) ->
    true;
postcondition(_S, _, _) ->
    false.


next_state(S, _V, {call, _, init, []}) ->
    S;
next_state(S, _V, {call, _, get, [_Key]}) ->
    S;
next_state(S, _V, {call, _, put, [Key, Val]}) ->
    S#st{
        kvs={call, dict, store, [Key, Val, S#st.kvs]}
    };
next_state(S, _V, {call, _, delete, [Key]}) ->
    S#st{
        kvs={call, dict, erase, [Key, S#st.kvs]}
    }.


random_key(#st{kvs=KVs}) ->
    Keys0 = dict:fetch_keys(KVs),
    Keys = lists:append(Keys0, [foo]),
    NumKeys = erlang:length(Keys),
    KeyPos = random:uniform(NumKeys),
    lists:nth(KeyPos, Keys).

cleanup() ->
    code:purge(ioq_kv_dyn),
    code:delete(ioq_kv_dyn).


% Generators

key() -> almost_any().
val() -> almost_any().
kvs() -> {any(), any()}.

% ioq_kv can't handle storing bitstrings that don't have
% a length divisible by 8. Instead of being clever I
% just define an almost any.
almost_any() ->
    oneof([
        integer(),
        float(),
        atom(),
        binary(),
        ?LAZY(loose_tuple(almost_any())),
        ?LAZY(list(almost_any()))
    ]).
-endif.
