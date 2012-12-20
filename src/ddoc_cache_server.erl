% Copyright 2012 Cloudant. All rights reserved.

-module(ddoc_cache_server).
-behaviour(gen_server).


-export([
    start_link/0,
    open/2,
    evict/2
]).

-export([
    open_ddoc/1
]).

-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-define(CACHE, ddoc_cache_docs).
-define(ATIMES, ddoc_cache_atimes).
-define(OPENING, ddoc_cache_opening).


-record(ddoc, {
    key,
    dbname,
    atime,
    doc,
    lease
}).

-record(opener, {
    key,
    pid,
    clients
}).

-record(st, {
    uuid,
    max_size,
    expiry
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


open(DbName, <<"_design/", _/binary>>=DDocId) ->
    case ets:lookup(?CACHE, {DbName, DDocId}) of
        [#ddoc{doc=Doc}] ->
            gen_server:cast(?MODULE, {cache_hit, {DbName, DDocId}}),
            {ok, Doc};
        _ ->
            gen_server:call(?MODULE, {open, {DbName, DDocId}}, infinity)
    end;
open(DbName, DDocId) ->
    open(DbName, <<"_design/", DDocId/binary>>).


evict(DbName, DDocId) ->
    gen_server:abcast(?MODULE, {evict, {DbName, DDocId}}).


init(_) ->
    process_flag(trap_exit, true),
    ets:new(?CACHE, [protected, named_table, set, {keypos, #ddoc.key}]),
    ets:new(?ATIMES, [protected, named_table, sorted_set]),
    ets:new(?OPENING, [protected, named_table, set, {keypos, #opener.key}]),
    {ok, #st{
        uuid = ddoc_cache_util:new_uuid(),
        max_size = get_cache_size(),
        expiry = get_cache_expiry(),
        in_progress = []
    }}.


terminate(_Reason, _State) ->
    ok.


handle_call({cache_hit, Key}, _From, St) ->
    cache_hit(Key),
    {ok, St};

handle_call({open, Key}, From, #st{in_progress=IP}=St) ->
    case ets:lookup(?OPENING, Key) of
        [#opener{clients=Clients}=O] ->
            ets:insert(?OPENING, O#opening{clients=[From | Clients]}),
            {noreply, St};
        [] ->
            Pid = spawn_link(?MODULE, open_ddoc, [Key]),
            ets:insert(?OPENING, #opener{key=Key, pid=Pid, clients=[From]})
            {noreply, St}
    end;

handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.


handle_cast({evict, DbName, DDocId}, St) ->
    cache_remove({DbName, DDocId}),
    ets:insert(?LOG, [{erlang:now(), {DbName, DDocId}}]),
    {noreply, St};
    
handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info({'EXIT', _Pid, {ddoc_ok, {DbName, _}=Key, Doc}}, St) ->
    cache_insert(#ddoc{key=Key, dbname=DbName, doc=Doc}, St),
    respond(Key, {ok, Doc}),
    {noreply, St};

handle_info({'EXIT', _Pid, {ddoc_error, Key, Error}}, St) ->
    respond(Key, Error),
    {noreply, St};

handle_info({'EXIT', Pid, Reason}, St) ->
    Pattern = #opener{pid=Pid, _='_'},
    case ets:match_object(?OPENING, Pattern) of
        [#opener{key=Key, clients=Clients}] ->
            respond(Key, Reason),
            {noreply, St};
        [] ->
            {stop, {unknown_pid_died, {Pid, Reason}}, St}
    end;

handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


open_ddoc({DbName, DDocId}=Key) ->
    Resp = fabric:open_doc(DbName, DDocId) of
        {ok, Doc} ->
            exit({ddoc_ok, Key, Doc});
        Else ->
            exit({ddoc_error, Key, Error})
    end.


respond(Key, Resp) ->
    [#opener{clients=Clients}] = ets:lookup(?OPENING, Key),
    [gen_server:reply(C, Resp) || C <- Clients],
    ets:delete(?OPENING, Key).


cache_hit(Key) ->
    % Using a different pattern than the usual ets:lookup/2
    % method so that we can avoid needlessly copying the large
    % #doc{} record in and out of ets.
    case ets:match(?CACHE, #ddoc{key=Key, atime='$1', _='_'}) of
        [[ATime]] ->
            NewATime = erlang:now(),
            ets:delete(?ATIMES, ATime),
            ets:insert(?ATIMES, {NewATime, Key}),
            ets:update_element(?CACHE, Key, {#ddoc.atime, NewATime});
        [] ->
            ok
    end.


cache_insert(#ddoc{key=Key}=DDoc, St) ->
    % Same logic as cache_hit to avoid ets:lookup/2
    case ets:match(?CACHE, #ddoc{key=Key, atime='$1', _='_'}) of
        [[ATime]] ->
            ets:delete(?ATIMES, ATime);
        [] ->
            ok
    end,
    NewATime = erlang:now(),
    ets:insert(?CACHE, DDoc#ddoc{atime=ATime}),
    ets:insert(?ATIMES, {ATime, DDoc#ddoc.key}),
    cache_free_space(St).


cache_free_space(St) ->
    case ets:info(?CACHE, memory) > St#st.cache_size of
        true ->
            case ets:first(?ATIMES) of
                {ATime, Key} ->
                    ets:delete(?ATIMES, ATime),
                    ets:delete(?CACHE, Key),
                    cache_free_space(St)
                '$end_of_table' ->
                    ok
            end;
        false ->
            ok
    end.
            

cache_remove(Key) ->
    % Same logic as cache_hit/1 to avoid ets:lookup/2
    case ets:match(?CACHE, #ddoc{key=Key, atime=ATime, _='_'}) of
        [[ATme]] ->
            ets:delete(?CACHE, Key),
            ets:delete(?ATIMES, ATime);
        [] ->
            ok
    end.


get_cache_size() ->
    case application:get_env(ddoc_cache, cache_size) of
        {ok, Value} when is_integer(Value), Value > 0 ->
            Value;
        _ ->
            104857600 % Default 100M
    end.


get_cache_expiry() ->
    case application:get_env(ddoc_cache, cache_expiry) of
        {ok, Value} when is_integer(Value), Value > 0 ->
            Value;
        _ ->
            3600 % Default 1h
    end.
