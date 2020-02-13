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

-module(couch_rate_ets).

-include("couch_rate.hrl").

-export([
    create_tables/0,
    delete_tables/0,
    create_if_missing/2,
    new/2,
    lookup/2,
    update/3
]).


-define(SHARDS_N, 16).

-type id() :: term().
-type state() :: term().
-type result() :: term().
-type store_state() :: term().


-spec create_if_missing(couch_rate:id(), state()) ->
    store_state().

create_if_missing(Id, State) ->
    Tid = term_to_table(Id),
    case ets:lookup(Tid, Id) of
        [_ | _] -> ok;
        _ -> ets:insert(Tid, {Id, State})
    end,
    ok.


-spec new(couch_rate:id(), state()) ->
    store_state()
    | {error, term()}.

new(Id, State) ->
    Tid = term_to_table(Id),
    case ets:insert_new(Tid, {Id, State}) of
        true -> ok;
        false -> {error, #{reason => already_exists, id => Id}}
    end.


-spec update(id(), store_state(), fun(
            (id(), state()) -> {state(), result()}
        )) ->
            result()
            | {error, term()}.

update(Id, _StoreState, Fun) ->
    Tid = term_to_table(Id),
    case ets:lookup(Tid, Id) of
        [{Id, State0}] ->
            case Fun(Id, State0) of
                {Result, State1} ->
                    ets:insert(Tid, {Id, State1}),
                    Result;
                Error ->
                    Error
            end;
        _ ->
            {error, #{reason => cannot_find, id => Id}}
    end.


-spec lookup(id(), store_state()) ->
    state()
    | {error, term()}.

lookup(Id, _StoreState) ->
    Tid = term_to_table(Id),
    case ets:lookup(Tid, Id) of
        [{Id, State}] ->
            State;
        _ ->
            {error, #{reason => cannot_find, id => Id}}
    end.


create_tables() ->
    Opts = [named_table, public, {read_concurrency, true}],
    [ets:new(TableName, Opts) || TableName <- table_names()],
    ok.

delete_tables() ->
    [ets:delete(TableName) || TableName <- table_names()],
    ok.


-spec term_to_table(any()) -> atom().
term_to_table(Term) ->
    PHash = erlang:phash2(Term),
    table_name(PHash rem ?SHARDS_N).


-dialyzer({no_return, table_names/0}).

-spec table_names() -> [atom()].
table_names() ->
    [table_name(N) || N <- lists:seq(0, ?SHARDS_N - 1)].

-spec table_name(non_neg_integer()) -> atom().
table_name(Id) when is_integer(Id), Id >= 0 andalso Id < ?SHARDS_N ->
    list_to_atom(atom_to_list(?MODULE) ++ "_" ++ integer_to_list(Id)).