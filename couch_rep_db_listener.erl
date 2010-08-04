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
    changes_feed_loop,
    changes_queue,
    changes_processor
}).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    process_flag(trap_exit, true),
    {ok, Queue} = couch_work_queue:new(1024 * 1024, 1000),
    {ok, Processor} = changes_processor(Queue),
    {ok, Loop} = changes_feed_loop(Queue),
    Server = self(),
    ok = couch_config:register(
        fun("replicator", "db") ->
            ok = gen_server:call(Server, rep_db_changed, infinity)
        end
    ),
    {ok, #state{
        changes_feed_loop = Loop,
        changes_queue = Queue,
        changes_processor = Processor}
    }.

handle_call(rep_db_changed, _From, State) ->
    #state{
        changes_feed_loop = Loop,
        changes_queue = Queue
    } = State,
    exit(Loop, rep_db_changed),
    {ok, NewLoop} = changes_feed_loop(Queue),
    {reply, ok, State#state{changes_feed_loop = NewLoop}}.


handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info({'EXIT', _OldChangesLoop, rep_db_changed}, State) ->
    {noreply, State};

handle_info({'EXIT', From, Reason}, #state{changes_processor = From} = State) ->
    ?LOG_ERROR("Replicator DB changes processor died. Reason: ~p", [Reason]),
    {stop, rep_db_changes_processor_error, State}.


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


process_change({Change}) ->
    {RepProps} = JsonRepDoc = couch_util:get_value(doc, Change),
    case couch_util:get_value(<<"deleted">>, Change, false) of
    true ->
        maybe_stop_replication(JsonRepDoc);
    false ->
        case couch_util:get_value(<<"state">>, RepProps) of
        <<"completed">> ->
            maybe_stop_replication(JsonRepDoc);
        <<"error">> ->
            % cleanup ets table entries
            maybe_stop_replication(JsonRepDoc);
        <<"triggered">> ->
            maybe_start_replication(JsonRepDoc);
        undefined ->
            case couch_util:get_value(<<"replication_id">>, RepProps) of
            undefined ->
                maybe_start_replication(JsonRepDoc);
            _ ->
                ok
            end
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


maybe_start_replication({RepProps} = JsonRepDoc) ->
    UserCtx = rep_user_ctx(JsonRepDoc),
    RepId = couch_rep:make_replication_id(JsonRepDoc, UserCtx),
    DocId = couch_util:get_value(<<"_id">>, RepProps),
    case ets:lookup(?REP_ID_TO_DOC_ID_MAP, RepId) of
    [] ->
        true = ets:insert(?REP_ID_TO_DOC_ID_MAP, {RepId, DocId}),
        true = ets:insert(?DOC_TO_REP_ID_MAP, {DocId, RepId}),
        spawn_link(fun() -> start_replication(JsonRepDoc, RepId, UserCtx) end);
    [{RepId, DocId}] ->
        ok;
    [{RepId, _OtherDocId}] ->
        couch_rep:update_rep_doc(
            JsonRepDoc, [{<<"replication_id">>, ?l2b(element(1, RepId))}]
        )
    end.


start_replication(RepDoc, RepId, UserCtx) ->
    case (catch couch_rep:start_replication(RepDoc, RepId, UserCtx)) of
    RepPid when is_pid(RepPid) ->
        couch_rep:get_result(RepPid, RepId, RepDoc, UserCtx);
    Error ->
        couch_rep:update_rep_doc(
            RepDoc,
            [
                {<<"state">>, <<"error">>},
                {<<"replication_id">>, ?l2b(element(1, RepId))}
            ]
        ),
        ?LOG_ERROR("Error starting replication ~p: ~p", [RepId, Error])
    end.


maybe_stop_replication({RepProps}) ->
    DocId = couch_util:get_value(<<"_id">>, RepProps),
    case ets:lookup(?DOC_TO_REP_ID_MAP, DocId) of
    [{DocId, RepId}] ->
        couch_rep:end_replication(RepId),
        true = ets:delete(?REP_ID_TO_DOC_ID_MAP, RepId),
        true = ets:delete(?DOC_TO_REP_ID_MAP, DocId);
    [] ->
        ok
    end.
