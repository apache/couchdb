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
-export([for_db/1, for_db/2, for_docid/2, for_docid/3, get/3, local/1, fold/2]).
-export([for_shard_name/1]).
-export([set_max_size/1]).

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
    HashKey = mem3_util:hash(DocId),
    ShardHead = #shard{
        name = '_',
        node = '_',
        dbname = DbName,
        range = ['$1','$2'],
        ref = '_'
    },
    OrderedShardHead = #ordered_shard{
        name = '_',
        node = '_',
        dbname = DbName,
        range = ['$1','$2'],
        ref = '_',
        order = '_'
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

for_shard_name(ShardName) ->
    for_shard_name(ShardName, []).

for_shard_name(ShardName, Options) ->
    DbName = mem3:dbname(ShardName),
    ShardHead = #shard{
        name = ShardName,
        node = '_',
        dbname = DbName,
        range = '_',
        ref = '_'
    },
    OrderedShardHead = #ordered_shard{
        name = ShardName,
        node = '_',
        dbname = DbName,
        range = '_',
        ref = '_',
        order = '_'
    },
    ShardSpec = {ShardHead, [], ['$_']},
    OrderedShardSpec = {OrderedShardHead, [], ['$_']},
    Shards = try ets:select(?SHARDS, [ShardSpec, OrderedShardSpec]) of
        [] ->
            filter_shards_by_name(ShardName, load_shards_from_disk(DbName));
        Else ->
            gen_server:cast(?MODULE, {cache_hit, DbName}),
            Else
    catch error:badarg ->
        filter_shards_by_name(ShardName, load_shards_from_disk(DbName))
    end,
    case lists:member(ordered, Options) of
        true  -> Shards;
        false -> mem3_util:downcast(Shards)
    end.

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
    DbName = config:get("mem3", "shards_db", "_dbs"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    FAcc = {Db, Fun, Acc},
    try
        {ok, _, LastAcc} = couch_db:enum_docs(Db, fun fold_fun/3, FAcc, []),
        {_Db, _UFun, UAcc} = LastAcc,
        UAcc
    after
        couch_db:close(Db)
    end.

set_max_size(Size) when is_integer(Size), Size > 0 ->
    gen_server:call(?MODULE, {set_max_size, Size}).

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

fold_fun(#full_doc_info{}=FDI, _, Acc) ->
    DI = couch_doc:to_doc_info(FDI),
    fold_fun(DI, nil, Acc);
fold_fun(#doc_info{}=DI, _, {Db, UFun, UAcc}) ->
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
    DbName = config:get("mem3", "shards_db", "_dbs"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    couch_db:close(Db),
    Db#db.update_seq.

listen_for_changes(Since) ->
    DbName = config:get("mem3", "shards_db", "_dbs"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
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
                [create_if_missing(mem3:name(S)) || S
                    <- Shards, mem3:node(S) =:= node()]
            end
        end
    end,
    {ok, Seq};
changes_callback(timeout, _) ->
    ok.

load_shards_from_disk(DbName) when is_binary(DbName) ->
    couch_stats:increment_counter([mem3, shard_cache, miss]),
    X = ?l2b(config:get("mem3", "shards_db", "_dbs")),
    {ok, Db} = mem3_util:ensure_exists(X),
    try
        load_shards_from_db(Db, DbName)
    after
        couch_db:close(Db)
    end.

load_shards_from_db(#db{} = ShardDb, DbName) ->
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
    HashKey = mem3_util:hash(DocId),
    [S || S <- Shards, in_range(S, HashKey)].

in_range(Shard, HashKey) ->
    [B, E] = mem3:range(Shard),
    B =< HashKey andalso HashKey =< E.

create_if_missing(Name) ->
    DbDir = config:get("couchdb", "database_dir"),
    Filename = filename:join(DbDir, ?b2l(Name) ++ ".couch"),
    case filelib:is_regular(Filename) of
    true ->
        ok;
    false ->
        case couch_server:create(Name, [?ADMIN_CTX]) of
        {ok, Db} ->
            couch_db:close(Db);
        Error ->
            couch_log:error("~p tried to create ~s, got ~p",
                [?MODULE, Name, Error])
        end
    end.

cache_insert(#st{cur_size=Cur}=St, DbName, Writer, Timeout) ->
    NewATime = now(),
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
            NewATime = now(),
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

maybe_spawn_shard_writer(DbName, Shards) ->
    case ets:member(?OPENERS, DbName) of
        true ->
            ignore;
        false ->
            spawn_shard_writer(DbName, Shards)
    end.

spawn_shard_writer(DbName, Shards) ->
    erlang:spawn(fun() -> shard_writer(DbName, Shards) end).

shard_writer(DbName, Shards) ->
    try
        receive
            write ->
                true = ets:insert(?SHARDS, Shards);
            cancel ->
                ok
        after ?WRITE_IDLE_TIMEOUT ->
            ok
        end
    after
        true = ets:delete_object(?OPENERS, {DbName, self()})
    end.

flush_write(DbName, Writer) ->
    Ref = erlang:monitor(process, Writer),
    Writer ! write,
    receive
        {'DOWN', Ref, _, _, normal} ->
            ok;
        {'DOWN', Ref, _, _, Error} ->
            erlang:exit({mem3_shards_bad_write, Error})
    after ?WRITE_TIMEOUT ->
        erlang:exit({mem3_shards_write_timeout, DbName})
    end.

filter_shards_by_name(Name, Shards) ->
    filter_shards_by_name(Name, [], Shards).

filter_shards_by_name(_, Matches, []) ->
    Matches;
filter_shards_by_name(Name, Matches, [#ordered_shard{name=Name}=S|Ss]) ->
    filter_shards_by_name(Name, [S|Matches], Ss);
filter_shards_by_name(Name, Matches, [#shard{name=Name}=S|Ss]) ->
    filter_shards_by_name(Name, [S|Matches], Ss);
filter_shards_by_name(Name, Matches, [_|Ss]) ->
    filter_shards_by_name(Name, Matches, Ss).
