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

-module(fabric2_index).


-behaviour(gen_server).


-export([
    register_index/1,
    db_updated/1,
    start_link/0
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-include_lib("couch/include/couch_db.hrl").


-callback build_indices(Db :: map(), DDocs :: list(#doc{})) ->
    [{ok, JobId::binary()} | {error, any()}].


-define(SHARDS, 32).
-define(DEFAULT_DELAY_MSEC, 60000).
-define(DEFAULT_RESOLUTION_MSEC, 10000).


register_index(Mod) when is_atom(Mod) ->
    Indices = lists:usort([Mod | registrations()]),
    application:set_env(fabric, indices, Indices).


db_updated(DbName) when is_binary(DbName) ->
    Table = table(erlang:phash2(DbName) rem ?SHARDS),
    ets:insert_new(Table, {DbName, now_msec()}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    lists:foreach(fun(T) ->
        spawn_link(fun() -> process_loop(T) end)
    end, create_tables()),
    {ok, nil}.


terminate(_M, _St) ->
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {bad_cast, Msg}, St}.


handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


create_tables() ->
    Opts = [
        named_table,
        public,
        {write_concurrency, true},
        {read_concurrency, true}
    ],
    Tables = [table(N) || N <- lists:seq(0, ?SHARDS - 1)],
    [ets:new(T, Opts) || T <- Tables].


table(Id) when is_integer(Id), Id >= 0 andalso Id < ?SHARDS ->
    list_to_atom("fabric2_index_" ++ integer_to_list(Id)).


process_loop(Table) ->
    Now = now_msec(),
    Delay = delay_msec(),
    Since = Now - Delay,
    case is_enabled() of
        true ->
            process_updates(Table, Since),
            clean_stale(Table, Since);
        false ->
            clean_stale(Table, Now)
    end,
    Resolution = resolution_msec(),
    Jitter = rand:uniform(1 + Resolution div 2),
    timer:sleep(Resolution + Jitter),
    process_loop(Table).


clean_stale(Table, Since) ->
    Head = {'_', '$1'},
    Guard = {'<', '$1', Since},
    % Monotonic is not strictly monotonic, so we process items using `=<` but
    % clean with `<` in case there was an update with the same timestamp after
    % we started processing already at that timestamp.
    ets:select_delete(Table, [{Head, [Guard], [true]}]).


process_updates(Table, Since) ->
    Head = {'$1', '$2'},
    Guard = {'=<', '$2', Since},
    case ets:select(Table, [{Head, [Guard], ['$1']}], 25) of
        '$end_of_table' -> ok;
        {Match, Cont} -> process_updates_iter(Match, Cont)
    end.


process_updates_iter([], Cont) ->
    case ets:select(Cont) of
        '$end_of_table' -> ok;
        {Match, Cont1} -> process_updates_iter(Match, Cont1)
    end;

process_updates_iter([Db | Rest], Cont) ->
    try
        process_db(Db)
    catch
        error:database_does_not_exist ->
            ok;
        Tag:Reason ->
            Stack = erlang:get_stacktrace(),
            LogMsg = "~p failed to build indices for `~s` ~p:~p ~p",
            couch_log:error(LogMsg, [?MODULE, Db, Tag, Reason, Stack])
    end,
    process_updates_iter(Rest, Cont).


build_indices(_TxDb, []) ->
    [];

build_indices(TxDb, DDocs) ->
    lists:flatmap(fun(Mod) ->
        Mod:build_indices(TxDb, DDocs)
    end, registrations()).


registrations() ->
    application:get_env(fabric, indices, []).


process_db(DbName) when is_binary(DbName) ->
    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    fabric2_fdb:transactional(Db, fun(TxDb) ->
        DDocs1 = get_design_docs(TxDb),
        DDocs2 = lists:filter(fun should_update/1, DDocs1),
        DDocs3 = shuffle(DDocs2),
        build_indices(TxDb, DDocs3)
    end).


get_design_docs(Db) ->
    Callback = fun
        ({meta, _}, Acc) ->  {ok, Acc};
        (complete, Acc) ->  {ok, Acc};
        ({row, Row}, Acc) -> {ok, [get_doc(Db, Row) | Acc]}
    end,
    {ok, DDocs} = fabric2_db:fold_design_docs(Db, Callback, [], []),
    DDocs.


get_doc(Db, Row) ->
    {_, DocId} = lists:keyfind(id, 1, Row),
    {ok, #doc{deleted = false} = Doc} = fabric2_db:open_doc(Db, DocId, []),
    Doc.


should_update(#doc{body = {Props}}) ->
    couch_util:get_value(<<"autoupdate">>, Props, true).


shuffle(Items) ->
    Tagged = [{rand:uniform(), I} || I <- Items],
    Sorted = lists:sort(Tagged),
    [I || {_T, I} <- Sorted].


now_msec() ->
    erlang:monotonic_time(millisecond).


is_enabled() ->
    config:get_boolean("fabric", "index_updater_enabled", true).


delay_msec() ->
    config:get_integer("fabric", "index_updater_delay_msec",
        ?DEFAULT_DELAY_MSEC).


resolution_msec() ->
    config:get_integer("fabric", "index_updater_resolution_msec",
        ?DEFAULT_RESOLUTION_MSEC).
