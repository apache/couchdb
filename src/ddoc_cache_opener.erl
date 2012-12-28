% Copyright 2012 Cloudant. All rights reserved.

-module(ddoc_cache_opener).
-behaviour(gen_server).


-export([
    start_link/0
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


-define(OPENING, ddoc_cache_opening).


-record(opener, {
    key,
    pid,
    clients
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init(_) ->
    process_flag(trap_exit, true),
    ets:new(?OPENING, [set, protected, named_table, {keypos, #opener.key}]),
    {ok, nil}.


terminate(_Reason, _State) ->
    ok.


handle_call({open, {_DbName, _DDocId}=Key}, From, St) ->
    case ets:lookup(?OPENING, Key) of
        [#opener{clients=Clients}=O] ->
            ets:insert(?OPENING, O#opener{clients=[From | Clients]}),
            {noreply, St};
        [] ->
            Pid = spawn_link(?MODULE, open_ddoc, [Key]),
            ets:insert(?OPENING, #opener{key=Key, pid=Pid, clients=[From]}),
            {noreply, St}
    end;

handle_call(Msg, _From, St) ->
    {stop, {invalid_call, Msg}, {invalid_call, Msg}, St}.


handle_cast(Msg, St) ->
    {stop, {invalid_cast, Msg}, St}.


handle_info({'EXIT', _Pid, {ddoc_ok, Key, Doc}}, St) ->
    respond(Key, {ok, Doc}),
    {noreply, St};

handle_info({'EXIT', _Pid, {ddoc_error, Key, Error}}, St) ->
    respond(Key, Error),
    {noreply, St};

handle_info({'EXIT', Pid, Reason}, St) ->
    Pattern = #opener{pid=Pid, _='_'},
    case ets:match_object(?OPENING, Pattern) of
        [#opener{key=Key, clients=Clients}] ->
            [gen_server:reply(C, {error, Reason}) || C <- Clients],
            ets:delete(?OPENING, Key),
            {noreply, St};
        [] ->
            {stop, {unknown_pid_died, {Pid, Reason}}, St}
    end;

handle_info(Msg, St) ->
    {stop, {invalid_info, Msg}, St}.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


open_ddoc({DbName, DDocId}=Key) ->
    case fabric:open_doc(DbName, DDocId) of
        {ok, Doc} ->
            ok = ets_lru:insert(ddoc_cache_lru, {DbName, DDocId}, Doc),
            exit({ddoc_ok, Key, Doc});
        Else ->
            exit({ddoc_error, Key, Else})
    end.


respond(Key, Resp) ->
    [#opener{clients=Clients}] = ets:lookup(?OPENING, Key),
    [gen_server:reply(C, Resp) || C <- Clients],
    ets:delete(?OPENING, Key).
