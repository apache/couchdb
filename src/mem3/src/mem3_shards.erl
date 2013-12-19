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
-behaviour(config_listener).

-export([init/1, terminate/2, code_change/3]).
-export([handle_call/3, handle_cast/2, handle_info/2]).
-export([handle_config_change/5]).

-export([start_link/0]).
-export([for_db/1, for_docid/2, get/3, local/1, fold/2]).
-export([set_max_size/1]).

-record(st, {
    max_size = 25000,
    cur_size = 0,
    changes_pid
}).

-include_lib("mem3/include/mem3.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(DBS, mem3_dbs).
-define(SHARDS, mem3_shards).
-define(ATIMES, mem3_atimes).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

for_db(DbName) ->
    try ets:lookup(?SHARDS, DbName) of
        [] ->
            load_shards_from_disk(DbName);
        Else ->
            gen_server:cast(?MODULE, {cache_hit, DbName}),
            Else
    catch error:badarg ->
        load_shards_from_disk(DbName)
    end.

for_docid(DbName, DocId) ->
    HashKey = mem3_util:hash(DocId),
    Head = #shard{
        name = '_',
        node = '_',
        dbname = DbName,
        range = ['$1','$2'],
        ref = '_'
    },
    Conditions = [{'=<', '$1', HashKey}, {'=<', HashKey, '$2'}],
    try ets:select(?SHARDS, [{Head, Conditions, ['$_']}]) of
        [] ->
            load_shards_from_disk(DbName, DocId);
        Shards ->
            gen_server:cast(?MODULE, {cache_hit, DbName}),
            Shards
    catch error:badarg ->
        load_shards_from_disk(DbName, DocId)
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

local(DbName) ->
    Pred = fun(#shard{node=Node}) when Node == node() -> true; (_) -> false end,
    lists:filter(Pred, for_db(DbName)).

fold(Fun, Acc) ->
    DbName = config:get("mem3", "shards_db", "dbs"),
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
handle_config_change("mem3", "shard_db", _DbName, _, _) ->
    {ok, gen_server:call(?MODULE, shard_db_changed, infinity)};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

init([]) ->
    ets:new(?SHARDS, [bag, protected, named_table, {keypos,#shard.dbname}]),
    ets:new(?DBS, [set, protected, named_table]),
    ets:new(?ATIMES, [ordered_set, protected, named_table]),
    ok = config:listen_for_changes(?MODULE, nil),
    SizeList = config:get("mem3", "shard_cache_size", "25000"),
    {Pid, _} = spawn_monitor(fun() -> listen_for_changes(get_update_seq()) end),
    {ok, #st{
        max_size = list_to_integer(SizeList),
        cur_size = 0,
        changes_pid = Pid
    }}.

handle_call({set_max_size, Size}, _From, St) ->
    {reply, ok, cache_free(St#st{max_size=Size})};
handle_call(shard_db_changed, _From, St) ->
    exit(St#st.changes_pid, shard_db_changed),
    {reply, ok, St};
handle_call(_Call, _From, St) ->
    {noreply, St}.

handle_cast({cache_hit, DbName}, St) ->
    cache_hit(DbName),
    {noreply, St};
handle_cast({cache_insert, DbName, Shards}, St) ->
    {noreply, cache_free(cache_insert(St, DbName, Shards))};
handle_cast({cache_remove, DbName}, St) ->
    {noreply, cache_remove(St, DbName)};
handle_cast(_Msg, St) ->
    {noreply, St}.

handle_info({'DOWN', _, _, Pid, Reason}, #st{changes_pid=Pid}=St) ->
    {NewSt, Seq} = case Reason of
        {seq, EndSeq} ->
            {St, EndSeq};
        shard_db_changed ->
            {cache_clear(St), get_update_seq()};
        _ ->
            twig:log(notice, "~p changes listener died ~p", [?MODULE, Reason]),
            {St, get_update_seq()}
    end,
    erlang:send_after(5000, self(), {start_listener, Seq}),
    {noreply, NewSt#st{changes_pid=undefined}};
handle_info({start_listener, Seq}, St) ->
    {NewPid, _} = spawn_monitor(fun() -> listen_for_changes(Seq) end),
    {noreply, St#st{changes_pid=NewPid}};
handle_info({gen_event_EXIT, {config_listener, ?MODULE}, _Reason}, State) ->
    erlang:send_after(5000, self(), restart_config_listener),
    {noreply, State};
handle_info(restart_config_listener, State) ->
    ok = config:listen_for_changes(?MODULE, nil),
    {noreply, State};
handle_info(_Msg, St) ->
    {noreply, St}.

terminate(_Reason, #st{changes_pid=Pid}) ->
    exit(Pid, kill),
    ok.

code_change(_OldVsn, St, _Extra) ->
    {ok, St}.

%% internal functions

fold_fun(#full_doc_info{}=FDI, _, Acc) ->
    DI = couch_doc:to_doc_info(FDI),
    fold_fun(DI, nil, Acc);
fold_fun(#doc_info{}=DI, _, {Db, UFun, UAcc}) ->
    case couch_db:open_doc(Db, DI, [conflicts]) of
        {ok, Doc} ->
            {Props} = Doc#doc.body,
            Shards = mem3_util:build_shards(Doc#doc.id, Props),
            NewUAcc = lists:foldl(UFun, UAcc, Shards),
            {ok, {Db, UFun, NewUAcc}};
        _ ->
            {ok, {Db, UFun, UAcc}}
    end.

get_update_seq() ->
    DbName = config:get("mem3", "shards_db", "dbs"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    couch_db:close(Db),
    Db#db.update_seq.

listen_for_changes(Since) ->
    DbName = config:get("mem3", "shards_db", "dbs"),
    {ok, Db} = mem3_util:ensure_exists(DbName),
    Args = #changes_args{
        feed = "continuous",
        since = Since,
        heartbeat = true,
        include_docs = true
    },
    ChangesFun = couch_changes:handle_changes(Args, Since, Db),
    ChangesFun(fun changes_callback/2).

changes_callback(start, Acc) ->
    {ok, Acc};
changes_callback({stop, EndSeq}, _) ->
    exit({seq, EndSeq});
changes_callback({change, {Change}, _}, _) ->
    DbName = couch_util:get_value(<<"id">>, Change),
    case DbName of <<"_design/", _/binary>> -> ok; _Else ->
        case mem3_util:is_deleted(Change) of
        true ->
            gen_server:cast(?MODULE, {cache_remove, DbName});
        false ->
            case couch_util:get_value(doc, Change) of
            {error, Reason} ->
                twig:log(error, "missing partition table for ~s: ~p",
                    [DbName, Reason]);
            {Doc} ->
                Shards = mem3_util:build_shards(DbName, Doc),
                gen_server:cast(?MODULE, {cache_insert, DbName, Shards}),
                [create_if_missing(Name) || #shard{name=Name, node=Node}
                    <- Shards, Node =:= node()]
            end
        end
    end,
    {ok, couch_util:get_value(<<"seq">>, Change)};
changes_callback(timeout, _) ->
    ok.

load_shards_from_disk(DbName) when is_binary(DbName) ->
    X = ?l2b(config:get("mem3", "shard_db", "dbs")),
    {ok, Db} = mem3_util:ensure_exists(X),
    try
        load_shards_from_db(Db, DbName)
    after
        couch_db:close(Db)
    end.

load_shards_from_db(#db{} = ShardDb, DbName) ->
    case couch_db:open_doc(ShardDb, DbName, []) of
    {ok, #doc{body = {Props}}} ->
        Shards = mem3_util:build_shards(DbName, Props),
        gen_server:cast(?MODULE, {cache_insert, DbName, Shards}),
        Shards;
    {not_found, _} ->
        erlang:error(database_does_not_exist, ?b2l(DbName))
    end.

load_shards_from_disk(DbName, DocId)->
    Shards = load_shards_from_disk(DbName),
    HashKey = mem3_util:hash(DocId),
    [S || #shard{range = [B,E]} = S <- Shards, B =< HashKey, HashKey =< E].

create_if_missing(Name) ->
    DbDir = config:get("couchdb", "database_dir"),
    Filename = filename:join(DbDir, ?b2l(Name) ++ ".couch"),
    case filelib:is_regular(Filename) of
    true ->
        ok;
    false ->
        Options = [{user_ctx, #user_ctx{roles=[<<"_admin">>]}}],
        case couch_server:create(Name, Options) of
        {ok, Db} ->
            couch_db:close(Db);
        Error ->
            twig:log(error, "~p tried to create ~s, got ~p",
                [?MODULE, Name, Error])
        end
    end.

cache_insert(#st{cur_size=Cur}=St, DbName, Shards) ->
    NewATime = now(),
    true = ets:delete(?SHARDS, DbName),
    true = ets:insert(?SHARDS, Shards),
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

