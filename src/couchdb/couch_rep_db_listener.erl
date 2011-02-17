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

-module(couch_rep_db_listener).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_info/2, handle_cast/2]).
-export([code_change/3, terminate/2]).

-include("couch_db.hrl").
-include("couch_replicator.hrl").

-define(DOC_ID_TO_REP_ID, rep_doc_id_to_rep_id).
-define(REP_ID_TO_DOC_ID, rep_id_to_rep_doc_id).
-define(INITIAL_WAIT, 5).

-import(couch_replicator_utils, [
    parse_rep_doc/2,
    update_rep_doc/2
]).
-import(couch_util, [
    get_value/2,
    get_value/3
]).

-record(state, {
    changes_feed_loop = nil,
    db_notifier = nil,
    rep_db_name = nil,
    rep_start_pids = [],
    max_retries
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    _ = ets:new(?DOC_ID_TO_REP_ID, [named_table, set, protected]),
    _ = ets:new(?REP_ID_TO_DOC_ID, [named_table, set, private]),
    Server = self(),
    ok = couch_config:register(
        fun("replicator", "db", NewName) ->
            ok = gen_server:cast(Server, {rep_db_changed, ?l2b(NewName)});
        ("replicator", "max_replication_retry_count", NewMaxRetries1) ->
            NewMaxRetries = list_to_integer(NewMaxRetries1),
            ok = gen_server:cast(Server, {set_max_retries, NewMaxRetries})
        end
    ),
    {Loop, RepDbName} = changes_feed_loop(),
    {ok, #state{
        changes_feed_loop = Loop,
        rep_db_name = RepDbName,
        db_notifier = db_update_notifier(),
        max_retries = list_to_integer(
            couch_config:get("replicator", "max_replication_retry_count", "10"))
    }}.


handle_call({rep_db_update, Change}, _From, State) ->
    {reply, ok, process_update(State, Change)};

handle_call({triggered, {BaseId, _}}, _From, State) ->
    [{BaseId, {DocId, true}}] = ets:lookup(?REP_ID_TO_DOC_ID, BaseId),
    true = ets:insert(?REP_ID_TO_DOC_ID, {BaseId, {DocId, false}}),
    {reply, ok, State};

handle_call({restart_failure, {Props} = RepDoc, Error}, _From, State) ->
    DocId = get_value(<<"_id">>, Props),
    [{DocId, {{BaseId, _} = RepId, MaxRetries}}] = ets:lookup(
        ?DOC_ID_TO_REP_ID, DocId),
    ?LOG_ERROR("Failed to start replication `~s` after ~p attempts using "
        "the document `~s`. Last error reason was: ~p",
        [pp_rep_id(RepId), MaxRetries, DocId, Error]),
    update_rep_doc(
        RepDoc,
        [{<<"_replication_state">>, <<"error">>},
            {<<"_replication_id">>, ?l2b(BaseId)}]),
    true = ets:delete(?REP_ID_TO_DOC_ID, BaseId),
    true = ets:delete(?DOC_ID_TO_REP_ID, DocId),
    {reply, ok, State};

handle_call(Msg, From, State) ->
    ?LOG_ERROR("Replicator DB listener received unexpected call ~p from ~p",
        [Msg, From]),
    {stop, {error, {unexpected_call, Msg}}, State}.


handle_cast({rep_db_changed, NewName},
        #state{rep_db_name = NewName} = State) ->
    {noreply, State};

handle_cast({rep_db_changed, _NewName}, State) ->
    {noreply, restart(State)};

handle_cast({rep_db_created, NewName},
        #state{rep_db_name = NewName} = State) ->
    {noreply, State};

handle_cast({rep_db_created, _NewName}, State) ->
    {noreply, restart(State)};

handle_cast({set_max_retries, MaxRetries}, State) ->
    {noreply, State#state{max_retries = MaxRetries}};

handle_cast(Msg, State) ->
    ?LOG_ERROR("Replicator DB listener received unexpected cast ~p", [Msg]),
    {stop, {error, {unexpected_cast, Msg}}, State}.


handle_info({'EXIT', From, normal}, #state{changes_feed_loop = From} = State) ->
    % replicator DB deleted
    {noreply, State#state{changes_feed_loop = nil, rep_db_name = nil}};

handle_info({'EXIT', From, Reason}, #state{db_notifier = From} = State) ->
    ?LOG_ERROR("Database update notifier died. Reason: ~p", [Reason]),
    {stop, {db_update_notifier_died, Reason}, State};

handle_info({'EXIT', From, normal}, #state{rep_start_pids = Pids} = State) ->
    % one of the replication start processes terminated successfully
    {noreply, State#state{rep_start_pids = Pids -- [From]}};

handle_info(Msg, State) ->
    ?LOG_ERROR("Replicator DB listener received unexpected message ~p", [Msg]),
    {stop, {unexpected_msg, Msg}, State}.


terminate(_Reason, State) ->
    #state{
        rep_start_pids = StartPids,
        changes_feed_loop = Loop,
        db_notifier = Notifier
    } = State,
    stop_all_replications(),
    lists:foreach(
        fun(Pid) ->
            catch unlink(Pid),
            catch exit(Pid, stop)
        end,
        [Loop | StartPids]),
    true = ets:delete(?REP_ID_TO_DOC_ID),
    true = ets:delete(?DOC_ID_TO_REP_ID),
    couch_db_update_notifier:stop(Notifier).


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


changes_feed_loop() ->
    {ok, RepDb} = couch_replicator_utils:ensure_rep_db_exists(),
    Server = self(),
    Pid = spawn_link(
        fun() ->
            ChangesFeedFun = couch_changes:handle_changes(
                #changes_args{
                    include_docs = true,
                    feed = "continuous",
                    timeout = infinity,
                    db_open_options = [sys_db]
                },
                {json_req, null},
                RepDb
            ),
            ChangesFeedFun(
                fun({change, Change, _}, _) ->
                    case has_valid_rep_id(Change) of
                    true ->
                        ok = gen_server:call(
                            Server, {rep_db_update, Change}, infinity);
                    false ->
                        ok
                    end;
                (_, _) ->
                    ok
                end
            )
        end
    ),
    couch_db:close(RepDb),
    {Pid, couch_db:name(RepDb)}.


has_valid_rep_id({Change}) ->
    has_valid_rep_id(get_value(<<"id">>, Change));
has_valid_rep_id(<<?DESIGN_DOC_PREFIX, _Rest/binary>>) ->
    false;
has_valid_rep_id(_Else) ->
    true.


db_update_notifier() ->
    Server = self(),
    {ok, Notifier} = couch_db_update_notifier:start_link(
        fun({created, DbName}) ->
            case ?l2b(couch_config:get("replicator", "db", "_replicator")) of
            DbName ->
                ok = gen_server:cast(Server, {rep_db_created, DbName});
            _ ->
                ok
            end;
        (_) ->
            % no need to handle the 'deleted' event - the changes feed loop
            % dies when the database is deleted
            ok
        end
    ),
    Notifier.


restart(#state{changes_feed_loop = Loop, rep_start_pids = StartPids} = State) ->
    stop_all_replications(),
    lists:foreach(
        fun(Pid) ->
            catch unlink(Pid),
            catch exit(Pid, rep_db_changed)
        end,
        [Loop | StartPids]),
    {NewLoop, NewRepDbName} = changes_feed_loop(),
    State#state{
        changes_feed_loop = NewLoop,
        rep_db_name = NewRepDbName,
        rep_start_pids = []
    }.


process_update(State, {Change}) ->
    {RepProps} = JsonRepDoc = get_value(doc, Change),
    DocId = get_value(<<"_id">>, RepProps),
    case get_value(<<"deleted">>, Change, false) of
    true ->
        rep_doc_deleted(DocId),
        State;
    false ->
        case get_value(<<"_replication_state">>, RepProps) of
        <<"completed">> ->
            replication_complete(DocId),
            State;
        <<"error">> ->
            stop_replication(DocId),
            State;
        <<"triggered">> ->
            maybe_start_replication(State, DocId, JsonRepDoc);
        undefined ->
            maybe_start_replication(State, DocId, JsonRepDoc)
        end
    end.


rep_user_ctx({RepDoc}) ->
    case get_value(<<"user_ctx">>, RepDoc) of
    undefined ->
        #user_ctx{roles = [<<"_admin">>]};
    {UserCtx} ->
        #user_ctx{
            name = get_value(<<"name">>, UserCtx, null),
            roles = get_value(<<"roles">>, UserCtx, [])
        }
    end.


maybe_start_replication(#state{max_retries = MaxRetries} = State,
        DocId, JsonRepDoc) ->
    {ok, #rep{id = {BaseId, _} = RepId} = Rep} =
        parse_rep_doc(JsonRepDoc, rep_user_ctx(JsonRepDoc)),
    case ets:lookup(?REP_ID_TO_DOC_ID, BaseId) of
    [] ->
        true = ets:insert(?REP_ID_TO_DOC_ID, {BaseId, {DocId, true}}),
        true = ets:insert(?DOC_ID_TO_REP_ID, {DocId, {RepId, MaxRetries}}),
        Server = self(),
        Pid = spawn_link(
            fun() -> start_replication(Server, Rep, MaxRetries) end),
        State#state{rep_start_pids = [Pid | State#state.rep_start_pids]};
    [{BaseId, DocId}] ->
        State;
    [{BaseId, {OtherDocId, false}}] ->
        ?LOG_INFO("The replication specified by the document `~s` was already"
            " triggered by the document `~s`", [DocId, OtherDocId]),
        maybe_tag_rep_doc(JsonRepDoc, ?l2b(BaseId)),
        State;
    [{BaseId, {OtherDocId, true}}] ->
        ?LOG_INFO("The replication specified by the document `~s` is already"
            " being triggered by the document `~s`", [DocId, OtherDocId]),
        maybe_tag_rep_doc(JsonRepDoc, ?l2b(BaseId)),
        State
    end.


maybe_tag_rep_doc({Props} = JsonRepDoc, RepId) ->
    case get_value(<<"_replication_id">>, Props) of
    RepId ->
        ok;
    _ ->
        update_rep_doc(JsonRepDoc, [{<<"_replication_id">>, RepId}])
    end.


start_replication(Server, #rep{id = RepId, doc = {RepProps}} = Rep, MaxRetries) ->
    case (catch couch_replicator:async_replicate(Rep)) of
    {ok, _} ->
        ok = gen_server:call(Server, {triggered, RepId}, infinity),
        ?LOG_INFO("Document `~s` triggered replication `~s`",
            [get_value(<<"_id">>, RepProps), pp_rep_id(RepId)]);
    Error ->
        keep_retrying(Server, Rep, Error, ?INITIAL_WAIT, MaxRetries)
    end.


keep_retrying(Server, Rep, Error, _Wait, 0) ->
    ok = gen_server:call(Server, {restart_failure, Rep, Error}, infinity);

keep_retrying(Server, #rep{doc = {RepProps}} = Rep, Error, Wait, RetriesLeft) ->
    DocId = get_value(<<"_id">>, RepProps),
    ?LOG_ERROR("Error starting replication `~s` (document `~s`): ~p. "
        "Retrying in ~p seconds", [pp_rep_id(Rep), DocId, Error, Wait]),
    ok = timer:sleep(Wait * 1000),
    case (catch couch_replicator:async_replicate(Rep)) of
    {ok, _} ->
        ok = gen_server:call(Server, {triggered, Rep#rep.id}, infinity),
        [{DocId, {RepId, MaxRetries}}] = ets:lookup(?DOC_ID_TO_REP_ID, DocId),
        ?LOG_INFO("Document `~s` triggered replication `~s` after ~p attempts",
            [DocId, pp_rep_id(RepId), MaxRetries - RetriesLeft + 1]);
    NewError ->
        keep_retrying(Server, Rep, NewError, Wait * 2, RetriesLeft - 1)
    end.


rep_doc_deleted(DocId) ->
    case stop_replication(DocId) of
    {ok, RepId} ->
        ?LOG_INFO("Stopped replication `~s` because replication document `~s`"
            " was deleted", [pp_rep_id(RepId), DocId]);
    none ->
        ok
    end.


replication_complete(DocId) ->
    case stop_replication(DocId) of
    {ok, RepId} ->
        ?LOG_INFO("Replication `~s` finished (triggered by document `~s`)",
            [pp_rep_id(RepId), DocId]);
    none ->
        ok
    end.


stop_replication(DocId) ->
    case ets:lookup(?DOC_ID_TO_REP_ID, DocId) of
    [{DocId, {{BaseId, _} = RepId, _MaxRetries}}] ->
        couch_replicator:cancel_replication(RepId),
        true = ets:delete(?REP_ID_TO_DOC_ID, BaseId),
        true = ets:delete(?DOC_ID_TO_REP_ID, DocId),
        {ok, RepId};
    [] ->
        none
    end.


stop_all_replications() ->
    ?LOG_INFO("Stopping all ongoing replications because the replicator DB "
        "was deleted or changed", []),
    ets:foldl(
        fun({_, {RepId, _}}, _) -> couch_replicator:cancel_replication(RepId) end,
        ok,
        ?DOC_ID_TO_REP_ID
    ),
    true = ets:delete_all_objects(?REP_ID_TO_DOC_ID),
    true = ets:delete_all_objects(?DOC_ID_TO_REP_ID).


% pretty-print replication id
pp_rep_id(#rep{id = RepId}) ->
    pp_rep_id(RepId);
pp_rep_id({Base, Extension}) ->
    Base ++ Extension.
