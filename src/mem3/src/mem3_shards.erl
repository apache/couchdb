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

-module(mem3_shards).
-behaviour(gen_server).
-vsn(3).
-behaviour(config_listener).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([handle_config_change/5, handle_config_terminate/3]).

-export([start_link/0]).
-export([opts_for_db/1]).
-export([for_db/1, for_db/2, for_docid/2, for_docid/3, get/3, local/1, fold/2]).
-export([for_shard_range/1]).
-export([set_max_size/1]).
-export([get_changes_pid/0]).

-record(st, {
    max_size = 25000,
    cur_size = 0,
    changes_pid,
    update_seq,
    write_timeout
}).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DBS, mem3_dbs).
-define(SHARDS, mem3_shards).
-define(ATIMES, mem3_atimes).
-define(OPENERS, mem3_openers).
-define(RELISTEN_DELAY, 5000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

opts_for_db(DbName) ->
    {ok, Db} = mem3_util:ensure_exists(mem3_sync:shards_db()),
    case couch_db:open_doc(Db, DbName, [ejson_body]) of
        {ok, #doc{body = {Props}}} ->
            mem3_util:get_shard_opts(Props);
        {not_found, _} ->
            erlang:error(database_does_not_exist, ?b2l(DbName))
    end.

for_db(DbName) ->
    for_db(DbName, []).

for_db(DbName, Options) ->
    Shards = try ets:lookup(?SHARDS, DbName) of
        [] ->
            load_shards_from_disk(DbName);
        Else ->
            gen_server:cast(?MODULE, {cache_hit, DbName}),
            Else
    catch error:badarg ->
        load_shards_from_disk(DbName)
    end,
    case lists:member(ordered, Options) of
        true  -> Shards;
        false -> mem3_util:downcast(Shards)
    end.

for_docid(DbName, DocId) ->
    for_docid(DbName, DocId, []).

for_docid(DbName, DocId, Options) ->
    HashKey = mem3_hash:calculate(DbName, DocId),
    ShardHead = #shard{
        dbname = DbName,
        range = ['$1', '$2'],
        _ = '_'
    },
    OrderedShardHead = #ordered_shard{
        dbname = DbName,
        range = ['$1', '$2'],
        _ = '_'
    },
    Conditions = [{'=<', '$1', HashKey}, {'=<', HashKey, '$2'}],
    ShardSpec = {ShardHead, Conditions, ['$_']},
    OrderedShardSpec = {OrderedShardHead, Conditions, ['$_']},
    Shards = try ets:select(?SHARDS, [ShardSpec, OrderedShardSpec]) of
        [] ->
            load_shards_from_disk(DbName, DocId);
        Else ->
            gen_server:cast(?MODULE, {cache_hit, DbName}),
            Else
    catch error:badarg ->
        load_shards_from_disk(DbName, DocId)
    end,
    case lists:member(ordered, Options) of
        true  -> Shards;
        false -> mem3_util:downcast(Shards)
    end.

for_shard_range(ShardName) ->
    DbName = mem3:dbname(ShardName),
    [B, E] = mem3:range(ShardName),
    ShardHead = #shard{
        dbname = DbName,
        range = ['$1', '$2'],
        _ = '_'
    },
    OrderedShardHead = #ordered_shard{
        dbname = DbName,
        range = ['$1', '$2'],
        _ = '_'
    },
    % see mem3_util:range_overlap/2 for an explanation how it works
    Conditions = [{'=<', '$1', E}, {'=<', B, '$2'}],
    ShardSpec = {ShardHead, Conditions, ['$_']},
    OrderedShardSpec = {OrderedShardHead, Conditions, ['$_']},
    Shards = try ets:select(?SHARDS, [ShardSpec, OrderedShardSpec]) of
        [] ->
            filter_shards_by_range([B, E], load_shards_from_disk(DbName));
        Else ->
            gen_server:cast(?MODULE, {cache_hit, DbName}),
            Else
    catch error:badarg ->
        filter_shards_by_range([B, E], load_shards_from_disk(DbName))
    end,
    mem3_util:downcast(Shards).


get(DbName, Node, Range) ->
    Res = lists:foldl(fun(#shard{node=N, range=R}=S, Acc) ->
        case {N, R} of
            {Node, Range} -> [S | Acc];
            _ -> Acc
        end
    end, [], for_db(DbName)),
    case Res of
        [] -> {error, not_found};
        [Shard] -> {ok, Shard};
        [_|_] -> {error, duplicates}
    end.

local(DbName) when is_list(DbName) ->
    local(list_to_binary(DbName));
local(DbName) ->
    Pred = fun(#shard{node=Node}) when Node == node() -> true; (_) -> false end,
    lists:filter(Pred, for_db(DbName)).

fold(Fun, Acc) ->
    {ok, Db} = mem3_util:ensure_exists(mem3_sync:shards_db()),
    FAcc = {Db, Fun, Acc},
    try
        {ok, LastAcc} = couch_db:fold_docs(Db, fun fold_fun/2, FAcc),
        {_Db, _UFun, UAcc} = LastAcc,
        UAcc
    after
        couch_db:close(Db)
    end.

set_max_size(Size) when is_integer(Size), Size > 0 ->
    gen_server:call(?MODULE, {set_max_size, Size}).

get_changes_pid() ->
    gen_server:call(?MODULE, get_changes_pid).

handle_config_change("mem3", "shard_cache_size", SizeList, _, _) ->
    Size = list_to_integer(SizeList),
    {ok, gen_server:call(?MODULE, {set_max_size, Size}, infinity)};
handle_config_change("mem3", "shards_db", _DbName, _, _) ->
    {ok, gen_server:call(?MODULE, shard_db_changed, infinity)};
handle_config_change("mem3", "shard_write_timeout", Timeout, _, _) ->
    Timeout = try
        list_to_integer(Timeout)
    catch _:_ ->
        1000
    end,
    {ok, gen_server:call(?MODULE, {set_write_timeout, Timeout})};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_, stop, _) ->
    ok;
handle_config_terminate(_Server, _Reason, _State) ->
    erlang:send_after(?RELISTEN_DELAY, whereis(?MODULE), restart_config_listener).

init([]) ->
    couch_util:set_mqd_off_heap(?MODULE),
    ets:new(?SHARDS, [
        bag,
        public,
        named_table,
        {keypos,#shard.dbname},
        {read_concurrency, true}
    ]),
    ets:new(?DBS, [set, protected, named_table]),
    ets:new(?ATIMES, [ordered_set, protected, named_table]),
    ets:new(?OPENERS, [bag, public, named_table]),
    ok = config:listen_for_changes(?MODULE, nil),
    SizeList = config:get("mem3", "shard_cache_size", "25000"),
    WriteTimeout = config:get_integer("mem3", "shard_write_timeout", 1000),
    UpdateSeq = get_update_seq(),
    {ok, #st{
        max_size = list_to_integer(SizeList),
        cur_size = 0,
        changes_pid = start_changes_listener(UpdateSeq),
        update_seq = UpdateSeq,
        write_timeout = WriteTimeout
    }}.

handle_call({set_max_size, Size}, _From, St) ->
    {reply, ok, cache_free(St#st{max_size=Size})};
handle_call(shard_db_changed, _From, St) ->
    exit(St#st.changes_pid, shard_db_changed),
    {reply, ok, St};
handle_call({set_write_timeout, Timeout}, _From, St) ->
    {reply, ok, St#st{write_timeout = Timeout}};
handle_call(get_changes_pid, _From, St) ->
    {reply, {ok, St#st.changes_pid}, St};
handle_call(_Call, _From, St) ->
    {noreply, St}.

handle_cast({cache_hit, DbName}, St) ->
    couch_stats:increment_counter([mem3, shard_cache, hit]),
    cache_hit(DbName),
    {noreply, St};
handle_cast({cache_insert, DbName, Writer, UpdateSeq}, St) ->
    % This comparison correctly uses the `<` operator
    % and not `=<`. The easiest way to understand why is
    % to think of when a _dbs db doesn't change. If it used
    % `=<` it would be impossible to insert anything into
    % the cache.
    NewSt = case UpdateSeq < St#st.update_seq of
        true ->
            Writer ! cancel,
            St;
        false ->
            cache_free(cache_insert(St, DbName, Writer, St#st.write_timeout))
    end,
    {noreply, NewSt};
handle_cast({cache_remove, DbName}, St) ->
    couch_stats:increment_counter([mem3, shard_cache, eviction]),
    {noreply, cache_remove(St, DbName)};
handle_cast({cache_insert_change, DbName, Writer, UpdateSeq}, St) ->
    Msg = {cache_insert, DbName, Writer, UpdateSeq},
    {noreply, NewSt} = handle_cast(Msg, St),
    {noreply, NewSt#st{update_seq = UpdateSeq}};
handle_cast({cache_remove_change, DbName, UpdateSeq}, St) ->
    {noreply, NewSt} = handle_cast({cache_remove, DbName}, St),
    {noreply, NewSt#st{update_seq = UpdateSeq}};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({'DOWN', _, _, Pid, Reason}, #st{changes_pid=Pid}=St) ->
    {NewSt, Seq} = case Reason of
        {seq, EndSeq} ->
            {St, EndSeq};
        shard_db_changed ->
            {cache_clear(St), get_update_seq()};
        _ ->
            couch_log:notice("~p changes listener died ~p", [?MODULE, Reason]),
            {St, get_update_seq()}
    end,
    erlang:send_after(5000, self(), {start_listener, Seq}),
    {noreply, NewSt#st{changes_pid=undefined}};
handle_info({start_listener, Seq}, St) ->
    {noreply, St#st{
        changes_pid = start_changes_listener(Seq)
    }};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, #st{changes_pid=Pid}) ->
    exit(Pid, kill),
    ok.

code_change(_OldVsn, #st{}=St, _Extra) ->
    {ok, St}.

%% internal functions

start_changes_listener(SinceSeq) ->
    Self = self(),
    {Pid, _} = erlang:spawn_monitor(fun() ->
        erlang:spawn_link(fun() ->
            Ref = erlang:monitor(process, Self),
            receive
                {'DOWN', Ref, _, _, _} ->
                    ok
            end,
            exit(shutdown)
        end),
        listen_for_changes(SinceSeq)
    end),
    Pid.

fold_fun(#full_doc_info{}=FDI, Acc) ->
    DI = couch_doc:to_doc_info(FDI),
    fold_fun(DI, Acc);
fold_fun(#doc_info{}=DI, {Db, UFun, UAcc}) ->
    case couch_db:open_doc(Db, DI, [ejson_body, conflicts]) of
        {ok, Doc} ->
            {Props} = Doc#doc.body,
            Shards = mem3_util:build_shards(Doc#doc.id, Props),
            NewUAcc = lists:foldl(UFun, UAcc, Shards),
            {ok, {Db, UFun, NewUAcc}};
        _ ->
            {ok, {Db, UFun, UAcc}}
    end.

get_update_seq() ->
    {ok, Db} = mem3_util:ensure_exists(mem3_sync:shards_db()),
    Seq = couch_db:get_update_seq(Db),
    couch_db:close(Db),
    Seq.

listen_for_changes(Since) ->
    {ok, Db} = mem3_util:ensure_exists(mem3_sync:shards_db()),
    Args = #changes_args{
        feed = "continuous",
        since = Since,
        heartbeat = true,
        include_docs = true
    },
    ChangesFun = couch_changes:handle_db_changes(Args, Since, Db),
    ChangesFun(fun changes_callback/2).

changes_callback(start, Acc) ->
    {ok, Acc};
changes_callback({stop, EndSeq}, _) ->
    exit({seq, EndSeq});
changes_callback({change, {Change}, _}, _) ->
    DbName = couch_util:get_value(<<"id">>, Change),
    Seq = couch_util:get_value(<<"seq">>, Change),
    case DbName of <<"_design/", _/binary>> -> ok; _Else ->
        case mem3_util:is_deleted(Change) of
        true ->
            gen_server:cast(?MODULE, {cache_remove_change, DbName, Seq});
        false ->
            case couch_util:get_value(doc, Change) of
            {error, Reason} ->
                couch_log:error("missing partition table for ~s: ~p",
                    [DbName, Reason]);
            {Doc} ->
                Shards = mem3_util:build_ordered_shards(DbName, Doc),
                IdleTimeout = config:get_integer(
                        "mem3", "writer_idle_timeout", 30000),
                Writer = spawn_shard_writer(DbName, Shards, IdleTimeout),
                ets:insert(?OPENERS, {DbName, Writer}),
                Msg = {cache_insert_change, DbName, Writer, Seq},
                gen_server:cast(?MODULE, Msg),
                Opts = mem3_util:get_shard_opts(Doc),
                [create_if_missing(mem3:name(S), Opts) || S
                    <- Shards, mem3:node(S) =:= node()]
            end
        end
    end,
    {ok, Seq};
changes_callback(timeout, _) ->
    ok.

load_shards_from_disk(DbName) when is_binary(DbName) ->
    couch_stats:increment_counter([mem3, shard_cache, miss]),
    {ok, Db} = mem3_util:ensure_exists(mem3_sync:shards_db()),
    try
        load_shards_from_db(Db, DbName)
    after
        couch_db:close(Db)
    end.

load_shards_from_db(ShardDb, DbName) ->
    case couch_db:open_doc(ShardDb, DbName, [ejson_body]) of
    {ok, #doc{body = {Props}}} ->
        Seq = couch_db:get_update_seq(ShardDb),
        Shards = mem3_util:build_ordered_shards(DbName, Props),
        IdleTimeout = config:get_integer("mem3", "writer_idle_timeout", 30000),
        case maybe_spawn_shard_writer(DbName, Shards, IdleTimeout) of
            Writer when is_pid(Writer) ->
                case ets:insert_new(?OPENERS, {DbName, Writer}) of
                    true ->
                        Msg = {cache_insert, DbName, Writer, Seq},
                        gen_server:cast(?MODULE, Msg);
                    false ->
                        Writer ! cancel
                end;
            ignore ->
                ok
        end,
        Shards;
    {not_found, _} ->
        erlang:error(database_does_not_exist, ?b2l(DbName))
    end.

load_shards_from_disk(DbName, DocId)->
    Shards = load_shards_from_disk(DbName),
    HashKey = mem3_hash:calculate(hd(Shards), DocId),
    [S || S <- Shards, in_range(S, HashKey)].

in_range(Shard, HashKey) ->
    [B, E] = mem3:range(Shard),
    B =< HashKey andalso HashKey =< E.

create_if_missing(Name, Options) ->
    case couch_server:exists(Name) of
        true ->
            ok;
        false ->
            case couch_server:create(Name, [?ADMIN_CTX] ++ Options) of
            {ok, Db} ->
                couch_db:close(Db);
            Error ->
                couch_log:error("~p tried to create ~s, got ~p",
                    [?MODULE, Name, Error])
            end
    end.

cache_insert(#st{cur_size=Cur}=St, DbName, Writer, Timeout) ->
    NewATime = couch_util:unique_monotonic_integer(),
    true = ets:delete(?SHARDS, DbName),
    flush_write(DbName, Writer, Timeout),
    case ets:lookup(?DBS, DbName) of
        [{DbName, ATime}] ->
            true = ets:delete(?ATIMES, ATime),
            true = ets:insert(?ATIMES, {NewATime, DbName}),
            true = ets:insert(?DBS, {DbName, NewATime}),
            St;
        [] ->
            true = ets:insert(?ATIMES, {NewATime, DbName}),
            true = ets:insert(?DBS, {DbName, NewATime}),
            St#st{cur_size=Cur + 1}
    end.

cache_remove(#st{cur_size=Cur}=St, DbName) ->
    true = ets:delete(?SHARDS, DbName),
    case ets:lookup(?DBS, DbName) of
        [{DbName, ATime}] ->
            true = ets:delete(?DBS, DbName),
            true = ets:delete(?ATIMES, ATime),
            St#st{cur_size=Cur-1};
        [] ->
            St
    end.

cache_hit(DbName) ->
    case ets:lookup(?DBS, DbName) of
        [{DbName, ATime}] ->
            NewATime = couch_util:unique_monotonic_integer(),
            true = ets:delete(?ATIMES, ATime),
            true = ets:insert(?ATIMES, {NewATime, DbName}),
            true = ets:insert(?DBS, {DbName, NewATime});
        [] ->
            ok
    end.

cache_free(#st{max_size=Max, cur_size=Cur}=St) when Max =< Cur ->
    ATime = ets:first(?ATIMES),
    [{ATime, DbName}] = ets:lookup(?ATIMES, ATime),
    true = ets:delete(?ATIMES, ATime),
    true = ets:delete(?DBS, DbName),
    true = ets:delete(?SHARDS, DbName),
    cache_free(St#st{cur_size=Cur-1});
cache_free(St) ->
    St.

cache_clear(St) ->
    true = ets:delete_all_objects(?DBS),
    true = ets:delete_all_objects(?SHARDS),
    true = ets:delete_all_objects(?ATIMES),
    St#st{cur_size=0}.

maybe_spawn_shard_writer(DbName, Shards, IdleTimeout) ->
    case ets:member(?OPENERS, DbName) of
        true ->
            ignore;
        false ->
            spawn_shard_writer(DbName, Shards, IdleTimeout)
    end.

spawn_shard_writer(DbName, Shards, IdleTimeout) ->
    erlang:spawn(fun() -> shard_writer(DbName, Shards, IdleTimeout) end).

shard_writer(DbName, Shards, IdleTimeout) ->
    try
        receive
            write ->
                true = ets:insert(?SHARDS, Shards);
            cancel ->
                ok
        after IdleTimeout ->
            ok
        end
    after
        true = ets:delete_object(?OPENERS, {DbName, self()})
    end.

flush_write(DbName, Writer, WriteTimeout) ->
    Ref = erlang:monitor(process, Writer),
    Writer ! write,
    receive
        {'DOWN', Ref, _, _, normal} ->
            ok;
        {'DOWN', Ref, _, _, Error} ->
            erlang:exit({mem3_shards_bad_write, Error})
    after WriteTimeout ->
        erlang:exit({mem3_shards_write_timeout, DbName})
    end.


filter_shards_by_range(Range, Shards)->
    lists:filter(fun
        (#ordered_shard{range = R}) -> mem3_util:range_overlap(Range, R);
        (#shard{range = R}) -> mem3_util:range_overlap(Range, R)
    end, Shards).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

-define(DB, <<"eunit_db_name">>).
-define(INFINITY, 99999999).


mem3_shards_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                t_maybe_spawn_shard_writer_already_exists(),
                t_maybe_spawn_shard_writer_new(),
                t_flush_writer_exists_normal(),
                t_flush_writer_times_out(),
                t_flush_writer_crashes(),
                t_writer_deletes_itself_when_done(),
                t_writer_does_not_delete_other_writers_for_same_shard(),
                t_spawn_writer_in_load_shards_from_db(),
                t_cache_insert_takes_new_update(),
                t_cache_insert_ignores_stale_update_and_kills_worker()
            ]
        }
    }.


setup_all() ->
    ets:new(?SHARDS, [bag, public, named_table, {keypos, #shard.dbname}]),
    ets:new(?OPENERS, [bag, public, named_table]),
    ets:new(?DBS, [set, public, named_table]),
    ets:new(?ATIMES, [ordered_set, public, named_table]),
    meck:expect(config, get, ["mem3", "shards_db", '_'], "_dbs"),
    ok.


teardown_all(_) ->
    meck:unload(),
    ets:delete(?ATIMES),
    ets:delete(?DBS),
    ets:delete(?OPENERS),
    ets:delete(?SHARDS).


setup() ->
    ets:delete_all_objects(?ATIMES),
    ets:delete_all_objects(?DBS),
    ets:delete_all_objects(?OPENERS),
    ets:delete_all_objects(?SHARDS).


teardown(_) ->
    ok.


t_maybe_spawn_shard_writer_already_exists() ->
    ?_test(begin
        ets:insert(?OPENERS, {?DB, self()}),
        Shards = mock_shards(),
        WRes = maybe_spawn_shard_writer(?DB, Shards, ?INFINITY),
        ?assertEqual(ignore, WRes)
    end).


t_maybe_spawn_shard_writer_new() ->
    ?_test(begin
        Shards = mock_shards(),
        WPid = maybe_spawn_shard_writer(?DB, Shards, 1000),
        WRef = erlang:monitor(process, WPid),
        ?assert(is_pid(WPid)),
        ?assert(is_process_alive(WPid)),
        WPid ! write,
        ?assertEqual(normal, wait_writer_result(WRef)),
        ?assertEqual(Shards, ets:tab2list(?SHARDS))
    end).


t_flush_writer_exists_normal() ->
    ?_test(begin
        Shards = mock_shards(),
        WPid = spawn_link_mock_writer(?DB, Shards, ?INFINITY),
        ?assertEqual(ok, flush_write(?DB, WPid, ?INFINITY)),
        ?assertEqual(Shards, ets:tab2list(?SHARDS))
    end).


t_flush_writer_times_out() ->
    ?_test(begin
        WPid = spawn(fun() -> receive will_never_receive_this -> ok end end),
        Error = {mem3_shards_write_timeout, ?DB},
        ?assertExit(Error, flush_write(?DB, WPid, 100)),
        exit(WPid, kill)
    end).


t_flush_writer_crashes() ->
    ?_test(begin
        WPid = spawn(fun() -> receive write -> exit('kapow!') end end),
        Error = {mem3_shards_bad_write, 'kapow!'},
        ?assertExit(Error, flush_write(?DB, WPid, 1000))
    end).


t_writer_deletes_itself_when_done() ->
    ?_test(begin
        Shards = mock_shards(),
        WPid = spawn_link_mock_writer(?DB, Shards, ?INFINITY),
        WRef = erlang:monitor(process, WPid),
        ets:insert(?OPENERS, {?DB, WPid}),
        WPid ! write,
        ?assertEqual(normal, wait_writer_result(WRef)),
        ?assertEqual(Shards, ets:tab2list(?SHARDS)),
        ?assertEqual([], ets:tab2list(?OPENERS))
    end).


t_writer_does_not_delete_other_writers_for_same_shard() ->
    ?_test(begin
        Shards = mock_shards(),
        WPid = spawn_link_mock_writer(?DB, Shards, ?INFINITY),
        WRef = erlang:monitor(process, WPid),
        ets:insert(?OPENERS, {?DB, WPid}),
        ets:insert(?OPENERS, {?DB, self()}),  % should not be deleted
        WPid ! write,
        ?assertEqual(normal, wait_writer_result(WRef)),
        ?assertEqual(Shards, ets:tab2list(?SHARDS)),
        ?assertEqual(1, ets:info(?OPENERS, size)),
        ?assertEqual([{?DB, self()}], ets:tab2list(?OPENERS))
    end).


t_spawn_writer_in_load_shards_from_db() ->
    ?_test(begin
        meck:expect(couch_db, open_doc, 3, {ok, #doc{body = {[]}}}),
        meck:expect(couch_db, get_update_seq, 1, 1),
        meck:expect(mem3_util, build_ordered_shards, 2, mock_shards()),
        erlang:register(?MODULE, self()), % register to get cache_insert cast
        load_shards_from_db(test_util:fake_db([{name, <<"testdb">>}]), ?DB),
        meck:validate(couch_db),
        meck:validate(mem3_util),
        Cast = receive
                {'$gen_cast', Msg} -> Msg
            after 1000 ->
                timeout
        end,
        ?assertMatch({cache_insert, ?DB, Pid, 1} when is_pid(Pid), Cast),
        {cache_insert, _, WPid, _} = Cast,
        exit(WPid, kill),
        ?assertEqual([{?DB, WPid}], ets:tab2list(?OPENERS)),
        meck:unload(couch_db),
        meck:unload(mem3_util)
    end).


t_cache_insert_takes_new_update() ->
    ?_test(begin
        Shards = mock_shards(),
        WPid = spawn_link_mock_writer(?DB, Shards, ?INFINITY),
        Msg = {cache_insert, ?DB, WPid, 2},
        {noreply, NewState} = handle_cast(Msg, mock_state(1)),
        ?assertMatch(#st{cur_size = 1}, NewState),
        ?assertEqual(Shards, ets:tab2list(?SHARDS)),
        ?assertEqual([], ets:tab2list(?OPENERS))
    end).


t_cache_insert_ignores_stale_update_and_kills_worker() ->
    ?_test(begin
        Shards = mock_shards(),
        WPid = spawn_link_mock_writer(?DB, Shards, ?INFINITY),
        WRef = erlang:monitor(process, WPid),
        Msg = {cache_insert, ?DB, WPid, 1},
        {noreply, NewState} = handle_cast(Msg, mock_state(2)),
        ?assertEqual(normal, wait_writer_result(WRef)),
        ?assertMatch(#st{cur_size = 0}, NewState),
        ?assertEqual([], ets:tab2list(?SHARDS)),
        ?assertEqual([], ets:tab2list(?OPENERS))
    end).


mock_state(UpdateSeq) ->
    #st{
        update_seq = UpdateSeq,
        changes_pid = self(),
        write_timeout = 1000
    }.


mock_shards() ->
    [
        #ordered_shard{
            name = <<"testshardname">>,
            node = node(),
            dbname = ?DB,
            range = [0,1],
            order = 1
        }
    ].


wait_writer_result(WRef) ->
    receive
        {'DOWN', WRef, _, _, Result} ->
            Result
        after 1000 ->
            timeout
    end.


spawn_link_mock_writer(Db, Shards, Timeout) ->
    erlang:spawn_link(fun() -> shard_writer(Db, Shards, Timeout) end).



mem3_shards_changes_test_() -> {
    "Test mem3_shards changes listener",
    {
        setup,
        fun test_util:start_couch/0, fun test_util:stop_couch/1,
        [
            fun should_kill_changes_listener_on_shutdown/0
        ]
    }
}.


should_kill_changes_listener_on_shutdown() ->
    {ok, Pid} = ?MODULE:start_link(),
    {ok, ChangesPid} = get_changes_pid(),
    ?assert(is_process_alive(ChangesPid)),
    true = erlang:unlink(Pid),
    true = test_util:stop_sync_throw(
        ChangesPid, fun() -> exit(Pid, shutdown) end, wait_timeout),
    ?assertNot(is_process_alive(ChangesPid)),
    exit(Pid, shutdown).

-endif.
