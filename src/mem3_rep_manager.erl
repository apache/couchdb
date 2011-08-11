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
-module(mem3_rep_manager).
-behaviour(gen_server).

% public API
-export([start_link/0, config_change/3]).
-export([replication_started/1, replication_completed/1, replication_error/2]).

% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-include("mem3.hrl").
-include_lib("couch/include/couch_db.hrl").
-include_lib("couch/include/couch_js_functions.hrl").

-define(DOC_TO_REP, mem3_rep_doc_id_to_rep_id).
-define(REP_TO_STATE, mem3_rep_id_to_rep_state).
-define(DB_TO_SEQ, mem3_db_to_seq).
-define(INITIAL_WAIT, 2.5). % seconds
-define(MAX_WAIT, 600).     % seconds
-define(CTX, {user_ctx, #user_ctx{roles=[<<"_admin">>, <<"_replicator">>]}}).

-record(state, {
    db_notifier = nil,
    max_retries,
    scan_pid = nil,
    rep_start_pids = []
}).

-record(rep_state, {
    dbname,
    doc_id,
    user_ctx,
    doc,
    starting,
    retries_left,
    max_retries,
    wait = ?INITIAL_WAIT
}).

-import(couch_util, [
    get_value/2,
    get_value/3,
    to_binary/1
]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

replication_started({BaseId, _} = RepId) ->
    case rep_state(RepId) of
    nil ->
        ok;
    #rep_state{dbname = DbName, doc_id = DocId} ->
        update_rep_doc(DbName, DocId, [
            {<<"_replication_state">>, <<"triggered">>},
            {<<"_replication_id">>, ?l2b(BaseId)}]),
        ok = gen_server:call(?MODULE, {rep_started, RepId}, infinity),
        twig:log(notice, "Document `~s` triggered replication `~s`",
            [DocId, pp_rep_id(RepId)])
    end.


replication_completed(RepId) ->
    case rep_state(RepId) of
    nil ->
        ok;
    #rep_state{dbname = DbName, doc_id = DocId} ->
        update_rep_doc(DbName, DocId, [{<<"_replication_state">>, <<"completed">>}]),
        ok = gen_server:call(?MODULE, {rep_complete, RepId}, infinity),
        twig:log(notice, "Replication `~s` finished (triggered by document `~s`)",
            [pp_rep_id(RepId), DocId])
    end.


replication_error({BaseId, _} = RepId, Error) ->
    case rep_state(RepId) of
    nil ->
        ok;
    #rep_state{dbname = DbName, doc_id = DocId} ->
        % TODO: maybe add error reason to replication document
        update_rep_doc(DbName, DocId, [
            {<<"_replication_state">>, <<"error">>},
            {<<"_replication_id">>, ?l2b(BaseId)}]),
        ok = gen_server:call(?MODULE, {rep_error, RepId, Error}, infinity)
    end.

init(_) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    ?DOC_TO_REP = ets:new(?DOC_TO_REP, [named_table, set, protected]),
    ?REP_TO_STATE = ets:new(?REP_TO_STATE, [named_table, set, protected]),
    ?DB_TO_SEQ = ets:new(?DB_TO_SEQ, [named_table, set, protected]),
    Server = self(),
    ok = couch_config:register(fun ?MODULE:config_change/3, Server),
    NotifierPid = db_update_notifier(),
    ScanPid = spawn_link(fun() -> scan_all_dbs(Server) end),
    {ok, #state{
       db_notifier = NotifierPid,
       scan_pid = ScanPid,
       max_retries = retries_value(
           couch_config:get("replicator", "max_replication_retry_count", "10"))
      }}.

config_change("replicator", "max_replication_retry_count", V) ->
    ok = gen_server:cast(?MODULE, {set_max_retries, retries_value(V)}).

handle_call({rep_db_update, DbName, {ChangeProps} = Change}, _From, State) ->
    NewState = try
        process_update(State, DbName, Change)
    catch
    _Tag:Error ->
        {RepProps} = get_value(doc, ChangeProps),
        DocId = get_value(<<"_id">>, RepProps),
        rep_db_update_error(Error, DbName, DocId),
        State
    end,
    {reply, ok, NewState};

handle_call({rep_started, RepId}, _From, State) ->
    case rep_state(RepId) of
    nil ->
        ok;
    RepState ->
        NewRepState = RepState#rep_state{
            starting = false,
            retries_left = State#state.max_retries,
            max_retries = State#state.max_retries,
            wait = ?INITIAL_WAIT
        },
        true = ets:insert(?REP_TO_STATE, {RepId, NewRepState})
    end,
    {reply, ok, State};

handle_call({rep_complete, RepId}, _From, State) ->
    true = ets:delete(?REP_TO_STATE, RepId),
    {reply, ok, State};

handle_call({rep_error, RepId, Error}, _From, State) ->
    {reply, ok, replication_error(State, RepId, Error)};

handle_call({resume_scan, DbName}, _From, State) ->
    Since = case ets:lookup(?DB_TO_SEQ, DbName) of
        [] -> 0;
        [{DbName, EndSeq}] -> EndSeq
    end,
    Pid = changes_feed_loop(DbName, Since),
    twig:log(debug, "Scanning ~s from update_seq ~p", [DbName, Since]),
    {reply, ok, State#state{rep_start_pids = [Pid | State#state.rep_start_pids]}};

handle_call({rep_db_checkpoint, DbName, EndSeq}, _From, State) ->
    true = ets:insert(?DB_TO_SEQ, {DbName, EndSeq}),
    {reply, ok, State};

handle_call(Msg, From, State) ->
    twig:log(error, "Replication manager received unexpected call ~p from ~p",
        [Msg, From]),
    {stop, {error, {unexpected_call, Msg}}, State}.


handle_cast({set_max_retries, MaxRetries}, State) ->
    {noreply, State#state{max_retries = MaxRetries}};

handle_cast(Msg, State) ->
    twig:log(error, "Replication manager received unexpected cast ~p", [Msg]),
    {stop, {error, {unexpected_cast, Msg}}, State}.

handle_info({nodeup, _Node}, State) ->
    {noreply, rescan(State)};

handle_info({nodedown, _Node}, State) ->
    {noreply, rescan(State)};

handle_info({'EXIT', From, normal}, #state{scan_pid = From} = State) ->
    twig:log(debug, "Background scan has completed.", []),
    {noreply, State#state{scan_pid=nil}};

handle_info({'EXIT', From, Reason}, #state{scan_pid = From} = State) ->
    twig:log(error, "Background scanner died. Reason: ~p", [Reason]),
    {stop, {scanner_died, Reason}, State};

handle_info({'EXIT', From, Reason}, #state{db_notifier = From} = State) ->
    twig:log(error, "Database update notifier died. Reason: ~p", [Reason]),
    {stop, {db_update_notifier_died, Reason}, State};

handle_info({'EXIT', From, normal}, #state{rep_start_pids = Pids} = State) ->
    % one of the replication start processes terminated successfully
    {noreply, State#state{rep_start_pids = Pids -- [From]}};

handle_info({'DOWN', _Ref, _, _, _}, State) ->
    % From a db monitor created by a replication process. Ignore.
    {noreply, State};

handle_info(Msg, State) ->
    twig:log(error, "Replication manager received unexpected message ~p", [Msg]),
    {stop, {unexpected_msg, Msg}, State}.

terminate(_Reason, State) ->
    #state{
        scan_pid = ScanPid,
        rep_start_pids = StartPids,
        db_notifier = DbNotifier
    } = State,
    stop_all_replications(),
    lists:foreach(
        fun(Pid) ->
            catch unlink(Pid),
            catch exit(Pid, stop)
        end,
        [ScanPid | StartPids]),
    true = ets:delete(?REP_TO_STATE),
    true = ets:delete(?DOC_TO_REP),
    true = ets:delete(?DB_TO_SEQ),
    couch_db_update_notifier:stop(DbNotifier).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

changes_feed_loop(DbName, Since) ->
    Server = self(),
    Pid = spawn_link(
        fun() ->
            fabric:changes(DbName, fun
            ({change, Change}, Acc) ->
                case has_valid_rep_id(Change) of
                true ->
                    ok = gen_server:call(
                        Server, {rep_db_update, DbName, Change}, infinity);
                false ->
                    ok
                end,
                {ok, Acc};
            ({stop, EndSeq}, Acc) ->
                ok = gen_server:call(Server, {rep_db_checkpoint, DbName, EndSeq}),
                {ok, Acc};
            (_, Acc) ->
                {ok, Acc}
            end,
            nil,
            #changes_args{
                include_docs = true,
                feed = "normal",
                since = Since,
                filter = main_only,
                timeout = infinity,
                db_open_options = [sys_db]
                }
            )
        end),
    Pid.

has_valid_rep_id({Change}) ->
    has_valid_rep_id(get_value(<<"id">>, Change));
has_valid_rep_id(<<?DESIGN_DOC_PREFIX, _Rest/binary>>) ->
    false;
has_valid_rep_id(_Else) ->
    true.


db_update_notifier() ->
    Server = self(),
    IsReplicatorDbFun = is_replicator_db_fun(),
    {ok, Notifier} = couch_db_update_notifier:start_link(
        fun({_, DbName}) ->
            case IsReplicatorDbFun(DbName) of
            true ->
                ok = gen_server:call(Server, {resume_scan, mem3:dbname(DbName)});
            _ ->
                ok
            end
        end
    ),
    Notifier.

rescan(#state{scan_pid = nil} = State) ->
    true = ets:delete_all_objects(?DB_TO_SEQ),
    Server = self(),
    NewScanPid = spawn_link(fun() -> scan_all_dbs(Server) end),
    State#state{scan_pid = NewScanPid};
rescan(#state{scan_pid = ScanPid} = State) ->
    unlink(ScanPid),
    exit(ScanPid, exit),
    rescan(State#state{scan_pid = nil}).

process_update(State, DbName, {Change}) ->
    {RepProps} = JsonRepDoc = get_value(doc, Change),
    DocId = get_value(<<"_id">>, RepProps),
    case {owner(DbName, DocId), get_value(deleted, Change, false)} of
    {false, _} ->
        replication_complete(DocId),
        State;
    {true, true} ->
        rep_doc_deleted(DocId),
        State;
    {true, false} ->
        case get_value(<<"_replication_state">>, RepProps) of
        undefined ->
            maybe_start_replication(State, DbName, DocId, JsonRepDoc);
        <<"triggered">> ->
            maybe_start_replication(State, DbName, DocId, JsonRepDoc);
        <<"completed">> ->
            replication_complete(DocId),
            State;
        <<"error">> ->
            case ets:lookup(?DOC_TO_REP, DocId) of
            [] ->
                maybe_start_replication(State, DbName, DocId, JsonRepDoc);
            _ ->
                State
            end
        end
    end.


rep_db_update_error(Error, DbName, DocId) ->
    case Error of
    {bad_rep_doc, Reason} ->
        ok;
    _ ->
        Reason = to_binary(Error)
    end,
    twig:log(error, "Replication manager, error processing document `~s`: ~s",
        [DocId, Reason]),
    update_rep_doc(DbName, DocId, [{<<"_replication_state">>, <<"error">>}]).


rep_user_ctx({RepDoc}) ->
    case get_value(<<"user_ctx">>, RepDoc) of
    undefined ->
        #user_ctx{};
    {UserCtx} ->
        #user_ctx{
            name = get_value(<<"name">>, UserCtx, null),
            roles = get_value(<<"roles">>, UserCtx, [])
        }
    end.


maybe_start_replication(State, DbName, DocId, RepDoc) ->
    UserCtx = rep_user_ctx(RepDoc),
    {BaseId, _} = RepId = make_rep_id(RepDoc, UserCtx),
    case rep_state(RepId) of
    nil ->
        RepState = #rep_state{
            dbname = DbName,
            doc_id = DocId,
            user_ctx = UserCtx,
            doc = RepDoc,
            starting = true,
            retries_left = State#state.max_retries,
            max_retries = State#state.max_retries
        },
        true = ets:insert(?REP_TO_STATE, {RepId, RepState}),
        true = ets:insert(?DOC_TO_REP, {DocId, RepId}),
        twig:log(notice, "Attempting to start replication `~s` (document `~s`).",
            [pp_rep_id(RepId), DocId]),
        Server = self(),
        Pid = spawn_link(fun() ->
            start_replication(Server, RepDoc, RepId, UserCtx, 0)
        end),
        State#state{rep_start_pids = [Pid | State#state.rep_start_pids]};
    #rep_state{doc_id = DocId} ->
        State;
    #rep_state{starting = false, dbname = DbName, doc_id = OtherDocId} ->
        twig:log(notice, "The replication specified by the document `~s` was already"
            " triggered by the document `~s`", [DocId, OtherDocId]),
        maybe_tag_rep_doc(DbName, DocId, RepDoc, ?l2b(BaseId)),
        State;
    #rep_state{starting = true, dbname = DbName, doc_id = OtherDocId} ->
        twig:log(notice, "The replication specified by the document `~s` is already"
            " being triggered by the document `~s`", [DocId, OtherDocId]),
        maybe_tag_rep_doc(DbName, DocId, RepDoc, ?l2b(BaseId)),
        State
    end.


make_rep_id(RepDoc, UserCtx) ->
    try
        couch_rep:make_replication_id(RepDoc, UserCtx)
    catch
    throw:{error, Reason} ->
        throw({bad_rep_doc, Reason});
    Tag:Err ->
        throw({bad_rep_doc, to_binary({Tag, Err})})
    end.


maybe_tag_rep_doc(DbName, DocId, {RepProps}, RepId) ->
    case get_value(<<"_replication_id">>, RepProps) of
    RepId ->
        ok;
    _ ->
        update_rep_doc(DbName, DocId, [{<<"_replication_id">>, RepId}])
    end.


start_replication(Server, RepDoc, RepId, UserCtx, Wait) ->
    ok = timer:sleep(Wait * 1000),
    case (catch couch_rep:start_replication(RepDoc, RepId, UserCtx, ?MODULE)) of
    Pid when is_pid(Pid) ->
        ok = gen_server:call(Server, {rep_started, RepId}, infinity),
        couch_rep:get_result(Pid, RepId, RepDoc, UserCtx);
    Error ->
        replication_error(RepId, Error)
    end.


replication_complete(DocId) ->
    case ets:lookup(?DOC_TO_REP, DocId) of
    [{DocId, RepId}] ->
        case rep_state(RepId) of
        nil ->
            couch_rep:end_replication(RepId);
        #rep_state{} ->
            ok
        end,
        true = ets:delete(?DOC_TO_REP, DocId);
    _ ->
        ok
    end.


rep_doc_deleted(DocId) ->
    case ets:lookup(?DOC_TO_REP, DocId) of
    [{DocId, RepId}] ->
        couch_rep:end_replication(RepId),
        true = ets:delete(?REP_TO_STATE, RepId),
        true = ets:delete(?DOC_TO_REP, DocId),
        twig:log(notice, "Stopped replication `~s` because replication document `~s`"
            " was deleted", [pp_rep_id(RepId), DocId]);
    [] ->
        ok
    end.


replication_error(State, RepId, Error) ->
    case rep_state(RepId) of
    nil ->
        State;
    RepState ->
        maybe_retry_replication(RepId, RepState, Error, State)
    end.

maybe_retry_replication(RepId, #rep_state{retries_left = 0} = RepState, Error, State) ->
    #rep_state{
        doc_id = DocId,
        max_retries = MaxRetries
    } = RepState,
    couch_rep:end_replication(RepId),
    true = ets:delete(?REP_TO_STATE, RepId),
    true = ets:delete(?DOC_TO_REP, DocId),
    twig:log(error, "Error in replication `~s` (triggered by document `~s`): ~s"
        "~nReached maximum retry attempts (~p).",
        [pp_rep_id(RepId), DocId, to_binary(error_reason(Error)), MaxRetries]),
    State;

maybe_retry_replication(RepId, RepState, Error, State) ->
    #rep_state{
        doc_id = DocId,
        user_ctx = UserCtx,
        doc = RepDoc
    } = RepState,
    #rep_state{wait = Wait} = NewRepState = state_after_error(RepState),
    true = ets:insert(?REP_TO_STATE, {RepId, NewRepState}),
    twig:log(error, "Error in replication `~s` (triggered by document `~s`): ~s"
        "~nRestarting replication in ~p seconds.",
        [pp_rep_id(RepId), DocId, to_binary(error_reason(Error)), Wait]),
    Server = self(),
    Pid = spawn_link(fun() ->
        start_replication(Server, RepDoc, RepId, UserCtx, Wait)
    end),
    State#state{rep_start_pids = [Pid | State#state.rep_start_pids]}.

stop_all_replications() ->
    twig:log(notice, "Stopping all ongoing replications.", []),
    ets:foldl(
        fun({_, RepId}, _) ->
            couch_rep:end_replication(RepId)
        end,
        ok, ?DOC_TO_REP),
    true = ets:delete_all_objects(?REP_TO_STATE),
    true = ets:delete_all_objects(?DOC_TO_REP),
    true = ets:delete_all_objects(?DB_TO_SEQ).

update_rep_doc(RepDbName, RepDocId, KVs) when is_binary(RepDocId) ->
    spawn_link(fun() ->
        try
            case fabric:open_doc(mem3:dbname(RepDbName), RepDocId, []) of
                {ok, LatestRepDoc} ->
                    update_rep_doc(RepDbName, LatestRepDoc, KVs);
                _ ->
                    ok
            end
        catch throw:conflict ->
            % Shouldn't happen, as by default only the role _replicator can
            % update replication documents.
            twig:log(error, "Conflict error when updating replication document `~s`."
                " Retrying.", [RepDocId]),
            ok = timer:sleep(5),
            update_rep_doc(RepDbName, RepDocId, KVs)
        end
    end);

update_rep_doc(RepDbName, #doc{body = {RepDocBody}} = RepDoc, KVs) ->
    NewRepDocBody = lists:foldl(
        fun({<<"_replication_state">> = K, State} = KV, Body) ->
                case get_value(K, Body) of
                State ->
                    Body;
                _ ->
                    Body1 = lists:keystore(K, 1, Body, KV),
                    lists:keystore(
                        <<"_replication_state_time">>, 1, Body1,
                        {<<"_replication_state_time">>, timestamp()})
                end;
            ({K, _V} = KV, Body) ->
                lists:keystore(K, 1, Body, KV)
        end,
        RepDocBody, KVs),
    case NewRepDocBody of
    RepDocBody ->
        ok;
    _ ->
        % Might not succeed - when the replication doc is deleted right
        % before this update (not an error, ignore).
        spawn_link(fun() ->
            fabric:update_doc(RepDbName, RepDoc#doc{body = {NewRepDocBody}}, [?CTX])
        end)
    end.


% RFC3339 timestamps.
% Note: doesn't include the time seconds fraction (RFC3339 says it's optional).
timestamp() ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = calendar:now_to_local_time(now()),
    UTime = erlang:universaltime(),
    LocalTime = calendar:universal_time_to_local_time(UTime),
    DiffSecs = calendar:datetime_to_gregorian_seconds(LocalTime) -
        calendar:datetime_to_gregorian_seconds(UTime),
    zone(DiffSecs div 3600, (DiffSecs rem 3600) div 60),
    iolist_to_binary(
        io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w~s",
            [Year, Month, Day, Hour, Min, Sec,
                zone(DiffSecs div 3600, (DiffSecs rem 3600) div 60)])).

zone(Hr, Min) when Hr >= 0, Min >= 0 ->
    io_lib:format("+~2..0w:~2..0w", [Hr, Min]);
zone(Hr, Min) ->
    io_lib:format("-~2..0w:~2..0w", [abs(Hr), abs(Min)]).

% pretty-print replication id
pp_rep_id({Base, Extension}) ->
    Base ++ Extension.


rep_state(RepId) ->
    case ets:lookup(?REP_TO_STATE, RepId) of
    [{RepId, RepState}] ->
        RepState;
    [] ->
        nil
    end.


error_reason({error, Reason}) ->
    Reason;
error_reason(Reason) ->
    Reason.

retries_value("infinity") ->
    infinity;
retries_value(Value) ->
    list_to_integer(Value).

state_after_error(#rep_state{retries_left = Left, wait = Wait} = State) ->
    Wait2 = erlang:min(trunc(Wait * 2), ?MAX_WAIT),
    case Left of
    infinity ->
        State#rep_state{wait = Wait2};
    _ ->
        State#rep_state{retries_left = Left - 1, wait = Wait2}
    end.

scan_all_dbs(Server) when is_pid(Server) ->
    {ok, Db} = mem3_util:ensure_exists(
        couch_config:get("mem3", "shard_db", "dbs")),
    ChangesFun = couch_changes:handle_changes(#changes_args{}, nil, Db),
    IsReplicatorDbFun = is_replicator_db_fun(),
    ChangesFun(fun({change, {Change}, _}, _) ->
        DbName = couch_util:get_value(<<"id">>, Change),
        case DbName of <<"_design/", _/binary>> -> ok; _Else ->
            case couch_util:get_value(<<"deleted">>, Change, false) of
            true ->
                ok;
            false ->
                IsReplicatorDbFun(DbName) andalso
                gen_server:call(Server, {resume_scan, DbName})
            end
        end;
        (_, _) -> ok
    end),
    couch_db:close(Db).

is_replicator_db_fun() ->
    {ok, RegExp} = re:compile("^([a-z][a-z0-9\\_\\$()\\+\\-\\/]*/)?_replicator$"),
    fun(DbName) ->
        match =:= re:run(mem3:dbname(DbName), RegExp, [{capture,none}])
    end.

owner(DbName, DocId) ->
    Shards = mem3:shards(DbName, DocId),
    Nodes = [node()|nodes()],
    LiveShards = [S || #shard{node=Node} = S <- Shards, lists:member(Node, Nodes)],
    [#shard{node=Node}] = lists:usort(fun(#shard{name=A}, #shard{name=B}) ->
                                              A =< B  end, LiveShards),
    node() =:= Node.
