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

-define(DOC_TO_REP_ID_MAP, rep_doc_id_to_rep_id).
-define(REP_ID_TO_DOC_ID_MAP, rep_id_to_rep_doc_id).

-record(state, {
    changes_feed_loop = nil,
    changes_queue = nil,
    changes_processor = nil,
    db_notifier = nil
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, Queue} = couch_work_queue:new(
        [{max_size, 1024 * 1024}, {max_items, 1000}]),
    {ok, Processor} = changes_processor(Queue),
    {ok, Loop} = changes_feed_loop(Queue),
    Server = self(),
    ok = couch_config:register(
        fun("replicator", "db") ->
            ok = gen_server:cast(Server, rep_db_changed)
        end
    ),
    {ok, #state{
        changes_feed_loop = Loop,
        changes_queue = Queue,
        changes_processor = Processor,
        db_notifier = db_update_notifier()}
    }.


handle_call(Msg, From, State) ->
    ?LOG_ERROR("Replicator DB listener received unexpected call ~p from ~p",
        [Msg, From]),
    {stop, {error, {unexpected_call, Msg}}, State}.


handle_cast(rep_db_changed, State) ->
    #state{
        changes_feed_loop = Loop,
        changes_queue = Queue
    } = State,
    catch unlink(Loop),
    catch exit(Loop, rep_db_changed),
    couch_work_queue:queue(Queue, stop_all_replications),
    {ok, NewLoop} = changes_feed_loop(Queue),
    {noreply, State#state{changes_feed_loop = NewLoop}};

handle_cast(rep_db_created, #state{changes_feed_loop = Loop} = State) ->
    catch unlink(Loop),
    catch exit(Loop, rep_db_changed),
    {ok, NewLoop} = changes_feed_loop(State#state.changes_queue),
    {noreply, State#state{changes_feed_loop = NewLoop}};

handle_cast(Msg, State) ->
    ?LOG_ERROR("Replicator DB listener received unexpected cast ~p", [Msg]),
    {stop, {error, {unexpected_cast, Msg}}, State}.

handle_info({'EXIT', From, normal}, #state{changes_feed_loop = From} = State) ->
    % replicator DB deleted
    couch_work_queue:queue(State#state.changes_queue, stop_all_replications),
    {noreply, State#state{changes_feed_loop = nil}};

handle_info({'EXIT', From, Reason}, #state{db_notifier = From} = State) ->
    ?LOG_ERROR("Database update notifier died. Reason: ~p", [Reason]),
    {stop, {db_update_notifier_died, Reason}, State};

handle_info({'EXIT', From, Reason}, #state{changes_processor = From} = State) ->
    ?LOG_ERROR("Replicator DB changes processor died. Reason: ~p", [Reason]),
    {stop, {rep_db_changes_processor_died, Reason}, State}.


terminate(_Reason, State) ->
    #state{
        changes_feed_loop = Loop,
        changes_queue = Queue
    } = State,
    exit(Loop, stop),
    % closing the queue will cause changes_processor to shutdown
    couch_work_queue:close(Queue),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


changes_feed_loop(ChangesQueue) ->
    {ok, RepDb} = couch_rep:ensure_rep_db_exists(),
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
                        couch_work_queue:queue(ChangesQueue, Change);
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
    {ok, Pid}.


db_update_notifier() ->
    Server = self(),
    {ok, Notifier} = couch_db_update_notifier:start_link(
        fun({created, DbName}) ->
            case ?l2b(couch_config:get("replicator", "db", "_replicator")) of
            DbName ->
                ok = gen_server:cast(Server, rep_db_created);
            _ ->
                ok
            end;
        (_) ->
            ok
        end
    ),
    Notifier.


changes_processor(ChangesQueue) ->
    Pid = spawn_link(
        fun() ->
            ets:new(?DOC_TO_REP_ID_MAP, [named_table, set, private]),
            ets:new(?REP_ID_TO_DOC_ID_MAP, [named_table, set, private]),
            consume_changes(ChangesQueue),
            true = ets:delete(?REP_ID_TO_DOC_ID_MAP),
            true = ets:delete(?DOC_TO_REP_ID_MAP)
        end
    ),
    {ok, Pid}.


consume_changes(ChangesQueue) ->
    case couch_work_queue:dequeue(ChangesQueue) of
    closed ->
        ok;
    {ok, Changes} ->
        lists:foreach(fun process_change/1, Changes),
        consume_changes(ChangesQueue)
    end.


has_valid_rep_id({Change}) ->
    has_valid_rep_id(couch_util:get_value(<<"id">>, Change));
has_valid_rep_id(<<?DESIGN_DOC_PREFIX, _Rest/binary>>) ->
    false;
has_valid_rep_id(_Else) ->
    true.

process_change(stop_all_replications) ->
    ?LOG_INFO("Stopping all ongoing replications because the replicator DB "
        "was deleted or changed", []),
    stop_all_replications();

process_change({Change}) ->
    {RepProps} = JsonRepDoc = couch_util:get_value(doc, Change),
    DocId = couch_util:get_value(<<"_id">>, RepProps),
    case couch_util:get_value(<<"deleted">>, Change, false) of
    true ->
        rep_doc_deleted(DocId);
    false ->
        case couch_util:get_value(<<"_replication_state">>, RepProps) of
        <<"completed">> ->
            replication_complete(DocId);
        <<"error">> ->
            stop_replication(DocId);
        <<"triggered">> ->
            maybe_start_replication(DocId, JsonRepDoc);
        undefined ->
            maybe_start_replication(DocId, JsonRepDoc);
        _ ->
            ?LOG_ERROR("Invalid value for the `_replication_state` property"
                " of the replication document `~s`", [DocId])
        end
    end,
    ok.


rep_user_ctx({RepDoc}) ->
    case couch_util:get_value(<<"user_ctx">>, RepDoc) of
    undefined ->
        #user_ctx{roles = [<<"_admin">>]};
    {UserCtx} ->
        #user_ctx{
            name = couch_util:get_value(<<"name">>, UserCtx, null),
            roles = couch_util:get_value(<<"roles">>, UserCtx, [])
        }
    end.


maybe_start_replication(DocId, JsonRepDoc) ->
    UserCtx = rep_user_ctx(JsonRepDoc),
    {BaseId, _} = RepId = couch_rep:make_replication_id(JsonRepDoc, UserCtx),
    case ets:lookup(?REP_ID_TO_DOC_ID_MAP, BaseId) of
    [] ->
        true = ets:insert(?REP_ID_TO_DOC_ID_MAP, {BaseId, DocId}),
        true = ets:insert(?DOC_TO_REP_ID_MAP, {DocId, RepId}),
        spawn_link(fun() -> start_replication(JsonRepDoc, RepId, UserCtx) end);
    [{BaseId, DocId}] ->
        ok;
    [{BaseId, OtherDocId}] ->
        maybe_tag_rep_doc(DocId, JsonRepDoc, ?l2b(BaseId), OtherDocId)
    end.


maybe_tag_rep_doc(DocId, {Props} = JsonRepDoc, RepId, OtherDocId) ->
    case couch_util:get_value(<<"_replication_id">>, Props) of
    RepId ->
        ok;
    _ ->
        ?LOG_INFO("The replication specified by the document `~s` was already"
            " triggered by the document `~s`", [DocId, OtherDocId]),
        couch_rep:update_rep_doc(JsonRepDoc, [{<<"_replication_id">>, RepId}])
    end.



start_replication({RepProps} = RepDoc, {Base, Ext} = RepId, UserCtx) ->
    case (catch couch_rep:start_replication(RepDoc, RepId, UserCtx)) of
    RepPid when is_pid(RepPid) ->
        ?LOG_INFO("Document `~s` triggered replication `~s`",
            [couch_util:get_value(<<"_id">>, RepProps), Base ++ Ext]),
        couch_rep:get_result(RepPid, RepId, RepDoc, UserCtx);
    Error ->
        couch_rep:update_rep_doc(
            RepDoc,
            [
                {<<"_replication_state">>, <<"error">>},
                {<<"_replication_id">>, ?l2b(Base)}
            ]
        ),
        ?LOG_ERROR("Error starting replication `~s`: ~p", [Base ++ Ext, Error])
    end.

rep_doc_deleted(DocId) ->
    case stop_replication(DocId) of
    {ok, {Base, Ext}} ->
        ?LOG_INFO("Stopped replication `~s` because replication document `~s`"
            " was deleted", [Base ++ Ext, DocId]);
    none ->
        ok
    end.

replication_complete(DocId) ->
    case stop_replication(DocId) of
    {ok, {Base, Ext}} ->
        ?LOG_INFO("Replication `~s` finished (triggered by document `~s`)",
            [Base ++ Ext, DocId]);
    none ->
        ok
    end.

stop_replication(DocId) ->
    case ets:lookup(?DOC_TO_REP_ID_MAP, DocId) of
    [{DocId, {BaseId, _} = RepId}] ->
        couch_rep:end_replication(RepId),
        true = ets:delete(?REP_ID_TO_DOC_ID_MAP, BaseId),
        true = ets:delete(?DOC_TO_REP_ID_MAP, DocId),
        {ok, RepId};
    [] ->
        none
    end.

stop_all_replications() ->
    ets:foldl(
        fun({_, RepId}, _) -> couch_rep:end_replication(RepId) end,
        ok,
        ?DOC_TO_REP_ID_MAP
    ),
    true = ets:delete_all_objects(?REP_ID_TO_DOC_ID_MAP),
    true = ets:delete_all_objects(?DOC_TO_REP_ID_MAP).
