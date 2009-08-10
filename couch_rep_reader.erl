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

-module(couch_rep_reader).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
    code_change/3]).

-export([start_link/4, next/1]).

-import(couch_util, [url_encode/1]).

-define (BUFFER_SIZE, 1000).
-define (MAX_CONCURRENT_REQUESTS, 100).
-define (MAX_CONNECTIONS, 20).
-define (MAX_PIPELINE_SIZE, 50).

-include("couch_db.hrl").
-include("../ibrowse/ibrowse.hrl").

-record (state, {
    parent,
    source,
    missing_revs,
    reader_loop,
    reader_from = nil,
    count = 0,
    docs = queue:new(),
    reply_to = nil,
    complete = false,
    monitor_count = 0,
    monitor_count_by_seq = ets:new(monitor_count_by_seq, [set, private]),
    monitors_by_ref = ets:new(monitors_by_ref, [set, private]),
    pending_doc_request = nil,
    high_missing_seq = 0
}).

start_link(Parent, Source, MissingRevs, PostProps) ->
    gen_server:start_link(?MODULE, [Parent, Source, MissingRevs, PostProps], []).

next(Pid) ->
    gen_server:call(Pid, next_docs, infinity).

init([Parent, Source, MissingRevs, _PostProps]) ->
    process_flag(trap_exit, true),
    if is_record(Source, http_db) ->
        #url{host=Host, port=Port} = ibrowse_lib:parse_url(Source#http_db.url),
        ibrowse:set_max_sessions(Host, Port, ?MAX_CONNECTIONS),
        ibrowse:set_max_pipeline_size(Host, Port, ?MAX_PIPELINE_SIZE);
    true -> ok end,
    Self = self(),
    ReaderLoop = spawn_link(fun() -> reader_loop(Self, Source, MissingRevs) end),
    State = #state{
        parent = Parent,
        source = Source,
        missing_revs = MissingRevs,
        reader_loop = ReaderLoop
    },
    {ok, State}.

handle_call({add_docs, Docs}, From, State) ->
    State#state.parent ! {update_stats, docs_read, length(Docs)},
    handle_add_docs(lists:flatten(Docs), From, State);

handle_call(next_docs, From, State) ->
    handle_next_docs(From, State);

handle_call({open_doc_revs, Id, Revs, HighSeq}, From, State) ->
    handle_open_doc_revs(Id, Revs, HighSeq, From, State);

handle_call({set_monitor_count, Seq, Count}, _From, State) ->
    ets:insert(State#state.monitor_count_by_seq, {Seq,Count}),
    {reply, ok, State};

handle_call({update_high_seq, HighSeq}, _From, State) ->
    {reply, ok, State#state{high_missing_seq=HighSeq}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', Ref, _, _, Reason}, State) ->
    handle_monitor_down(Reason, Ref, State);

handle_info({'EXIT', Loop, complete}, #state{reader_loop=Loop} = State) ->
    handle_reader_loop_complete(State).

terminate(Reason, _State) ->
    % ?LOG_INFO("rep reader terminating with reason ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%internal funs

handle_add_docs(DocsToAdd, From, #state{reply_to=nil} = State) ->
    NewState = State#state{
        docs = queue:join(State#state.docs, queue:from_list(DocsToAdd)),
        count = State#state.count + length(DocsToAdd)
    },
    if NewState#state.count < ?BUFFER_SIZE ->
        {reply, ok, NewState};
    true ->
        {noreply, NewState#state{reader_from=From}}
    end;
handle_add_docs(DocsToAdd, _From, #state{count=0} = State) ->
    HighSeq = State#state.high_missing_seq,
    gen_server:reply(State#state.reply_to, {HighSeq, DocsToAdd}),
    {reply, ok, State#state{reply_to=nil}}.

handle_next_docs(From, #state{count=0} = State) ->
    if State#state.complete ->
        {stop, normal, {complete, State#state.high_missing_seq}, State};
    true ->
        {noreply, State#state{reply_to=From}}
    end;
handle_next_docs(_From, State) ->
    #state{
        reader_from = ReaderFrom,
        docs = Docs,
        high_missing_seq = HighSeq
    } = State,
    if ReaderFrom =/= nil ->
        gen_server:reply(ReaderFrom, ok);
    true -> ok end,
    NewState = State#state{count=0, reader_from=nil, docs=queue:new()},
    {reply, {HighSeq, queue:to_list(Docs)}, NewState}.

handle_open_doc_revs(Id, Revs, Seq, From, #state{monitor_count=N} = State)
        when N > ?MAX_CONCURRENT_REQUESTS ->
    {noreply, State#state{pending_doc_request={From,Id,Revs,Seq}}};
handle_open_doc_revs(Id, Revs, Seq, _From, #state{source=#http_db{}} = State) ->
    #state{
        monitor_count = Count,
        monitors_by_ref = MonitorsByRef,
        source = Source
    } = State,
    {_, Ref} = spawn_document_request(Source, Id, Revs),
    ets:insert(MonitorsByRef, {Ref, Seq}),
    {reply, ok, State#state{monitor_count = Count+1}}.

handle_monitor_down(normal, Ref, #state{pending_doc_request=nil,
        monitor_count=1, complete=waiting_on_monitors} = State) ->
    N = calculate_new_high_seq(State, Ref),
    {noreply, State#state{complete=true, monitor_count=0, high_missing_seq=N}};
handle_monitor_down(normal, Ref, #state{pending_doc_request=nil} = State) ->
    #state{monitor_count = Count} = State,
    HighSeq = calculate_new_high_seq(State, Ref),
    {noreply, State#state{monitor_count = Count-1, high_missing_seq=HighSeq}};
handle_monitor_down(normal, Ref, State) ->
    #state{
        source = Source,
        monitors_by_ref = MonitorsByRef,
        pending_doc_request = {From, Id, Revs, Seq}
    } = State,
    HighSeq = calculate_new_high_seq(State, Ref),
    gen_server:reply(From, ok),
    {_, NewRef} = spawn_document_request(Source, Id, Revs),
    ets:insert(MonitorsByRef, {NewRef, Seq}),
    {noreply, State#state{pending_doc_request=nil, high_missing_seq=HighSeq}};
handle_monitor_down(Reason, _, State) ->
    {stop, Reason, State}.

handle_reader_loop_complete(#state{reply_to=nil, monitor_count=0} = State) ->
    {noreply, State#state{complete = true}};
handle_reader_loop_complete(#state{monitor_count=0} = State) ->
    HighSeq = State#state.high_missing_seq,
    gen_server:reply(State#state.reply_to, {complete, HighSeq}),
    {stop, normal, State};
handle_reader_loop_complete(State) ->
    {noreply, State#state{complete = waiting_on_monitors}}.

split_revlist(Rev, {[CurrentAcc|Rest], BaseLength, Length}) ->
    case Length+size(Rev) > 8192 of
    false ->
        {[[Rev|CurrentAcc] | Rest], BaseLength, Length+size(Rev)};
    true ->
        {[[Rev],CurrentAcc|Rest], BaseLength, BaseLength}
    end.

open_doc_revs(#http_db{} = DbS, DocId, Revs) ->
    %% all this logic just splits up revision lists that are too long for
    %% MochiWeb into multiple requests
    BaseQS = [{revs,true}, {latest,true}],
    BaseReq = DbS#http_db{resource=url_encode(DocId), qs=BaseQS},
    BaseLength = length(couch_rep_httpc:full_url(BaseReq)) + 11, % &open_revs=

    {RevLists, _, _} = lists:foldl(fun split_revlist/2,
        {[[]], BaseLength, BaseLength}, couch_doc:rev_to_strs(Revs)),

    Requests = [BaseReq#http_db{
        qs = [{open_revs, ?JSON_ENCODE(RevList)} | BaseQS]
    } || RevList <- RevLists],
    JsonResults = lists:flatten([couch_rep_httpc:request(R) || R <- Requests]),

    Transform =
    fun({[{<<"missing">>, Rev}]}) ->
        {{not_found, missing}, couch_doc:parse_rev(Rev)};
    ({[{<<"ok">>, Json}]}) ->
        #doc{id=Id, revs=Rev, atts=Atts} = Doc = couch_doc:from_json_obj(Json),
        Doc#doc{atts=[couch_rep_att:convert_stub(A, {DbS,Id,Rev}) || A <- Atts]}
    end,
    [Transform(Result) || Result <- JsonResults].

reader_loop(ReaderServer, Source, MissingRevsServer) ->
    case couch_rep_missing_revs:next(MissingRevsServer) of
    complete ->
        % ?LOG_INFO("reader_loop terminating with complete", []),
        exit(complete);
    {HighSeq, IdsRevs} ->
        % ?LOG_DEBUG("got IdsRevs ~p", [IdsRevs]),
        case Source of
        #http_db{} ->
            N = length(IdsRevs),
            gen_server:call(ReaderServer, {set_monitor_count, HighSeq, N}),
            [gen_server:call(ReaderServer, {open_doc_revs, Id, Revs, HighSeq})
                || {Id,Revs} <- IdsRevs];
        _Local ->
            lists:foreach(fun({Id,Revs}) ->
                {ok, Docs} = couch_db:open_doc_revs(Source, Id, Revs, [latest]),
                JustTheDocs = [Doc || {ok, Doc} <- Docs],
                gen_server:call(ReaderServer, {add_docs, JustTheDocs})
            end, IdsRevs),
            gen_server:call(ReaderServer, {update_high_seq, HighSeq})
        end
    end,
    reader_loop(ReaderServer, Source, MissingRevsServer).

spawn_document_request(Source, Id, Revs) ->
    Server = self(),
    SpawnFun = fun() ->
        Results = open_doc_revs(Source, Id, Revs),
        gen_server:call(Server, {add_docs, Results})
    end,
    spawn_monitor(SpawnFun).

%% check if any more HTTP requests are pending for this update sequence
calculate_new_high_seq(State, Ref) ->
    #state{
        monitors_by_ref = MonitorsByRef,
        monitor_count_by_seq = MonitorCountBySeq,
        high_missing_seq = OldSeq
    } = State,
    Seq = ets:lookup_element(MonitorsByRef, Ref, 2),
    ets:delete(MonitorsByRef, Ref),
    case ets:update_counter(MonitorCountBySeq, Seq, -1) of
    0 ->
        ets:delete(MonitorCountBySeq, Seq),
        case ets:first(MonitorCountBySeq) of
        Key when Key > Seq ->
            Seq;
        '$end_of_table' ->
            Seq;
        _Else ->
            OldSeq
        end;
    _Else ->
        OldSeq
    end.
