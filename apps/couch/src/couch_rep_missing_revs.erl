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

-module(couch_rep_missing_revs).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
    code_change/3]).

-export([start_link/4, next/1, stop/1]).

-define(BUFFER_SIZE, 1000).

-include("couch_db.hrl").

-record (state, {
    changes_loop,
    changes_from = nil,
    parent,
    complete = false,
    count = 0,
    reply_to = nil,
    rows = queue:new(),
    high_source_seq = 0,
    high_missing_seq = 0,
    high_committed_seq = 0
}).

start_link(Parent, Target, ChangesFeed, PostProps) ->
    gen_server:start_link(?MODULE, [Parent, Target, ChangesFeed, PostProps], []).

next(Server) ->
    gen_server:call(Server, next_missing_revs, infinity).

stop(Server) ->
    gen_server:call(Server, stop).

init([Parent, _Target, ChangesFeed, _PostProps]) ->
    process_flag(trap_exit, true),
    Self = self(),
    Pid = spawn_link(fun() -> changes_loop(Self, ChangesFeed, Parent) end),
    {ok, #state{changes_loop=Pid, parent=Parent}}.

handle_call({add_missing_revs, {HighSeq, Revs}}, From, State) ->
    State#state.parent ! {update_stats, missing_revs, length(Revs)},
    handle_add_missing_revs(HighSeq, Revs, From, State);

handle_call(next_missing_revs, From, State) ->
    handle_next_missing_revs(From, State).

handle_cast({update_committed_seq, N}, State) ->
    if State#state.high_committed_seq < N ->
        ?LOG_DEBUG("missing_revs updating committed seq to ~p", [N]);
    true -> ok end,
    {noreply, State#state{high_committed_seq=N}}.

handle_info({'EXIT', Pid, Reason}, #state{changes_loop=Pid} = State) ->
    handle_changes_loop_exit(Reason, State);

handle_info(Msg, State) ->
    ?LOG_INFO("unexpected message ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{changes_loop=Pid}) when is_pid(Pid) ->
    exit(Pid, shutdown),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%internal funs

handle_add_missing_revs(HighSeq, [], _From, State) ->
    NewState = State#state{high_source_seq=HighSeq},
    maybe_checkpoint(NewState),
    {reply, ok, NewState};
handle_add_missing_revs(HighSeq, Revs, From, #state{reply_to=nil} = State) ->
    #state{rows=Rows, count=Count} = State,
    NewState = State#state{
        rows = queue:join(Rows, queue:from_list(Revs)),
        count = Count + length(Revs),
        high_source_seq = HighSeq,
        high_missing_seq = HighSeq
    },
    if NewState#state.count < ?BUFFER_SIZE ->
        {reply, ok, NewState};
    true ->
        {noreply, NewState#state{changes_from=From}}
    end;
handle_add_missing_revs(HighSeq, Revs, _From, #state{count=0} = State) ->
    gen_server:reply(State#state.reply_to, {HighSeq, Revs}),
    NewState = State#state{
        high_source_seq = HighSeq,
        high_missing_seq = HighSeq,
        reply_to = nil
    },
    {reply, ok, NewState}.

handle_next_missing_revs(From, #state{count=0} = State) ->
    if State#state.complete ->
        {stop, normal, complete, State};
    true ->
        {noreply, State#state{reply_to=From}}
    end;
handle_next_missing_revs(_From, State) ->
    #state{
        changes_from = ChangesFrom,
        high_missing_seq = HighSeq,
        rows = Rows
    } = State,
    if ChangesFrom =/= nil -> gen_server:reply(ChangesFrom, ok); true -> ok end,
    NewState = State#state{count=0, changes_from=nil, rows=queue:new()},
    {reply, {HighSeq, queue:to_list(Rows)}, NewState}.

handle_changes_loop_exit(normal, State) ->
    if State#state.reply_to =/= nil ->
        gen_server:reply(State#state.reply_to, complete),
        {stop, normal, State};
    true ->
        {noreply, State#state{complete=true, changes_loop=nil}}
    end;
handle_changes_loop_exit(Reason, State) ->
    {stop, Reason, State#state{changes_loop=nil}}.

changes_loop(OurServer, SourceChangesServer, Parent) ->
    case couch_rep_changes_feed:next(SourceChangesServer) of
    complete ->
        exit(normal);
    Changes ->
        {ok, Target} = gen_server:call(Parent, get_target_db, infinity),
        MissingRevs = get_missing_revs(Target, Changes),
        gen_server:call(OurServer, {add_missing_revs, MissingRevs}, infinity)
    end,
    changes_loop(OurServer, SourceChangesServer, Parent).

get_missing_revs(#http_db{}=Target, Changes) ->
    Transform = fun({Props}) ->
        C = couch_util:get_value(<<"changes">>, Props),
        Id = couch_util:get_value(<<"id">>, Props),
        {Id, [R || {[{<<"rev">>, R}]} <- C]}
    end,
    IdRevsList = [Transform(Change) || Change <- Changes],
    SeqDict = changes_dictionary(Changes),
    {LastProps} = lists:last(Changes),
    HighSeq = couch_util:get_value(<<"seq">>, LastProps),
    Request = Target#http_db{
        resource = "_missing_revs",
        method = post,
        body = {IdRevsList}
    },
    {Resp} = couch_rep_httpc:request(Request),
    case couch_util:get_value(<<"missing_revs">>, Resp) of
    {MissingRevs} ->
        X = [{Id, dict:fetch(Id, SeqDict), couch_doc:parse_revs(RevStrs)} ||
            {Id,RevStrs} <- MissingRevs],
        {HighSeq, X};
    _ ->
        exit({target_error, couch_util:get_value(<<"error">>, Resp)})
    end;

get_missing_revs(Target, Changes) ->
    Transform = fun({Props}) ->
        C = couch_util:get_value(<<"changes">>, Props),
        Id = couch_util:get_value(<<"id">>, Props),
        {Id, [couch_doc:parse_rev(R) || {[{<<"rev">>, R}]} <- C]}
    end,
    IdRevsList = [Transform(Change) || Change <- Changes],
    SeqDict = changes_dictionary(Changes),
    {LastProps} = lists:last(Changes),
    HighSeq = couch_util:get_value(<<"seq">>, LastProps),
    {ok, Results} = couch_db:get_missing_revs(Target, IdRevsList),
    {HighSeq, [{Id, dict:fetch(Id, SeqDict), Revs} || {Id, Revs, _} <- Results]}.

changes_dictionary(ChangeList) ->
    KVs = [{couch_util:get_value(<<"id">>,C), couch_util:get_value(<<"seq">>,C)}
        || {C} <- ChangeList],
    dict:from_list(KVs).

%% save a checkpoint if no revs are missing on target so we don't
%% rescan metadata unnecessarily
maybe_checkpoint(#state{high_missing_seq=N, high_committed_seq=N} = State) ->
    #state{
        parent = Parent,
        high_source_seq = SourceSeq
    } = State,
    Parent ! {missing_revs_checkpoint, SourceSeq};
maybe_checkpoint(_State) ->
    ok.
