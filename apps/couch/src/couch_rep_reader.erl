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

-include("couch_db.hrl").

-record (state, {
    parent,
    source,
    missing_revs,
    reader_loop,
    reader_from = [],
    count = 0,
    docs = queue:new(),
    reply_to = nil,
    complete = false,
    monitor_count = 0,
    pending_doc_request = nil,
    requested_seqs = [],
    opened_seqs = []
}).

start_link(Parent, Source, MissingRevs_or_DocIds, PostProps) ->
    gen_server:start_link(
        ?MODULE, [Parent, Source, MissingRevs_or_DocIds, PostProps], []
    ).

next(Pid) ->
    gen_server:call(Pid, next_docs, infinity).

init([Parent, Source, MissingRevs_or_DocIds, _PostProps]) ->
    process_flag(trap_exit, true),
    Self = self(),
    ReaderLoop = spawn_link(
        fun() -> reader_loop(Self, Parent, Source, MissingRevs_or_DocIds) end
    ),
    MissingRevs = case MissingRevs_or_DocIds of
    Pid when is_pid(Pid) ->
        Pid;
    _ListDocIds ->
        nil
    end,
    State = #state{
        parent = Parent,
        source = Source,
        missing_revs = MissingRevs,
        reader_loop = ReaderLoop
    },
    {ok, State}.

handle_call({add_docs, Seq, Docs}, From, State) ->
    State#state.parent ! {update_stats, docs_read, length(Docs)},
    handle_add_docs(Seq, lists:flatten(Docs), From, State);

handle_call({add_request_seqs, Seqs}, _From, State) ->
    SeqList = State#state.requested_seqs,
    {reply, ok, State#state{requested_seqs = lists:merge(Seqs, SeqList)}};

handle_call(next_docs, From, State) ->
    handle_next_docs(From, State);

handle_call({open_remote_doc, Id, Seq, Revs}, From, State) ->
    handle_open_remote_doc(Id, Seq, Revs, From, State).

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, _, _, Reason}, State) ->
    handle_monitor_down(Reason, State);

handle_info({'EXIT', Loop, complete}, #state{reader_loop=Loop} = State) ->
    handle_reader_loop_complete(State).

terminate(_Reason, _State) ->
    % ?LOG_INFO("rep reader terminating with reason ~p", [_Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%internal funs

handle_add_docs(_Seq, [], _From, State) ->
    {reply, ok, State};
handle_add_docs(Seq, DocsToAdd, From, #state{reply_to=nil} = State) ->
    State1 = update_sequence_lists(Seq, State),
    NewState = State1#state{
        docs = queue:join(State1#state.docs, queue:from_list(DocsToAdd)),
        count = State1#state.count + length(DocsToAdd)
    },
    if NewState#state.count < ?BUFFER_SIZE ->
        {reply, ok, NewState};
    true ->
        {noreply, NewState#state{reader_from=[From|State#state.reader_from]}}
    end;
handle_add_docs(Seq, DocsToAdd, _From, #state{count=0} = State) ->
    NewState = update_sequence_lists(Seq, State),
    HighSeq = calculate_new_high_seq(NewState),
    gen_server:reply(State#state.reply_to, {HighSeq, DocsToAdd}),
    {reply, ok, NewState#state{reply_to=nil}}.

handle_next_docs(From, #state{count=0} = State) ->
    if State#state.complete ->
        {stop, normal, {complete, calculate_new_high_seq(State)}, State};
    true ->
        {noreply, State#state{reply_to=From}}
    end;
handle_next_docs(_From, State) ->
    #state{
        reader_from = ReaderFrom,
        docs = Docs
    } = State,
    [gen_server:reply(F, ok) || F <- ReaderFrom],
    NewState = State#state{count=0, reader_from=[], docs=queue:new()},
    {reply, {calculate_new_high_seq(State), queue:to_list(Docs)}, NewState}.

handle_open_remote_doc(Id, Seq, Revs, From, #state{monitor_count=N} = State)
        when N > ?MAX_CONCURRENT_REQUESTS ->
    {noreply, State#state{pending_doc_request={From,Id,Seq,Revs}}};
handle_open_remote_doc(Id, Seq, Revs, _, #state{source=#http_db{}} = State) ->
    #state{
        monitor_count = Count,
        source = Source
    } = State,
    {_, _Ref} = spawn_document_request(Source, Id, Seq, Revs),
    {reply, ok, State#state{monitor_count = Count+1}}.

handle_monitor_down(normal, #state{pending_doc_request=nil, reply_to=nil,
        monitor_count=1, complete=waiting_on_monitors} = State) ->
    {noreply, State#state{complete=true, monitor_count=0}};
handle_monitor_down(normal, #state{pending_doc_request=nil, reply_to=From,
        monitor_count=1, complete=waiting_on_monitors} = State) ->
    gen_server:reply(From, {complete, calculate_new_high_seq(State)}),
    {stop, normal, State#state{complete=true, monitor_count=0}};
handle_monitor_down(normal, #state{pending_doc_request=nil} = State) ->
    #state{monitor_count = Count} = State,
    {noreply, State#state{monitor_count = Count-1}};
handle_monitor_down(normal, State) ->
    #state{
        source = Source,
        pending_doc_request = {From, Id, Seq, Revs}
    } = State,
    gen_server:reply(From, ok),
    {_, _NewRef} = spawn_document_request(Source, Id, Seq, Revs),
    {noreply, State#state{pending_doc_request=nil}};
handle_monitor_down(Reason, State) ->
    {stop, Reason, State}.

handle_reader_loop_complete(#state{reply_to=nil, monitor_count=0} = State) ->
    {noreply, State#state{complete = true}};
handle_reader_loop_complete(#state{monitor_count=0} = State) ->
    HighSeq = calculate_new_high_seq(State),
    gen_server:reply(State#state.reply_to, {complete, HighSeq}),
    {stop, normal, State};
handle_reader_loop_complete(State) ->
    {noreply, State#state{complete = waiting_on_monitors}}.

calculate_new_high_seq(#state{missing_revs=nil}) ->
    nil;
calculate_new_high_seq(#state{requested_seqs=[], opened_seqs=[Open|_]}) ->
    Open;
calculate_new_high_seq(#state{requested_seqs=[Req|_], opened_seqs=[Open|_]})
        when Req < Open ->
    0;
calculate_new_high_seq(#state{opened_seqs=[]}) ->
    0;
calculate_new_high_seq(State) ->
    hd(State#state.opened_seqs).

split_revlist(Rev, {[CurrentAcc|Rest], BaseLength, Length}) ->
    case Length+size(Rev) > 8192 of
    false ->
        {[[Rev|CurrentAcc] | Rest], BaseLength, Length+size(Rev)};
    true ->
        {[[Rev],CurrentAcc|Rest], BaseLength, BaseLength}
    end.

% We store outstanding requested sequences and a subset of already opened
% sequences in 2 ordered lists.  The subset of opened seqs is a) the largest
% opened seq smaller than the smallest outstanding request seq plus b) all the
% opened seqs greater than the smallest outstanding request.  I believe its the
% minimal set of info needed to correctly calculate which seqs have been
% replicated (because remote docs can be opened out-of-order) -- APK
update_sequence_lists(_Seq, #state{missing_revs=nil} = State) ->
    State;
update_sequence_lists(Seq, State) ->
    Requested = lists:delete(Seq, State#state.requested_seqs),
    AllOpened = lists:merge([Seq], State#state.opened_seqs),
    Opened = case Requested of
    [] ->
        [lists:last(AllOpened)];
    [EarliestReq|_] ->
        case lists:splitwith(fun(X) -> X < EarliestReq end, AllOpened) of
        {[], Greater} ->
            Greater;
        {Less, Greater} ->
            [lists:last(Less) | Greater]
        end
    end,
    State#state{
        requested_seqs = Requested,
        opened_seqs = Opened
    }.

open_doc_revs(#http_db{url = Url} = DbS, DocId, Revs) ->
    %% all this logic just splits up revision lists that are too long for
    %% MochiWeb into multiple requests
    BaseQS = [{revs,true}, {latest,true}, {att_encoding_info,true}],
    BaseReq = DbS#http_db{resource=url_encode(DocId), qs=BaseQS},
    BaseLength = length(couch_rep_httpc:full_url(BaseReq)) + 11, % &open_revs=

    {RevLists, _, _} = lists:foldl(fun split_revlist/2,
        {[[]], BaseLength, BaseLength}, couch_doc:revs_to_strs(Revs)),

    Requests = [BaseReq#http_db{
        qs = [{open_revs, ?JSON_ENCODE(RevList)} | BaseQS]
    } || RevList <- RevLists],
    JsonResults = lists:flatten([couch_rep_httpc:request(R) || R <- Requests]),

    Transform =
    fun({[{<<"ok">>, Json}]}, Acc) ->
        #doc{id=Id, revs=Rev, atts=Atts} = Doc = couch_doc:from_json_obj(Json),
        Doc1 = Doc#doc{
            atts=[couch_rep_att:convert_stub(A, {DbS,Id,Rev}) || A <- Atts]
        },
        [Doc1 | Acc];
    ({ErrorProps}, Acc) ->
        Err = couch_util:get_value(<<"error">>, ErrorProps,
            ?JSON_ENCODE({ErrorProps})),
        ?LOG_ERROR("Replicator: error accessing doc ~s at ~s, reason: ~s",
           [DocId, couch_util:url_strip_password(Url), Err]),
        Acc
    end,
    lists:reverse(lists:foldl(Transform, [], JsonResults)).

open_doc(#http_db{url = Url} = DbS, DocId) ->
    % get latest rev of the doc
    Req = DbS#http_db{
        resource=url_encode(DocId),
        qs=[{att_encoding_info, true}]
    },
    {Props} = Json = couch_rep_httpc:request(Req),
    case couch_util:get_value(<<"_id">>, Props) of
    Id when is_binary(Id) ->
        #doc{id=Id, revs=Rev, atts=Atts} = Doc = couch_doc:from_json_obj(Json),
        [Doc#doc{
            atts=[couch_rep_att:convert_stub(A, {DbS,Id,Rev}) || A <- Atts]
        }];
    undefined ->
        Err = couch_util:get_value(<<"error">>, Props, ?JSON_ENCODE(Json)),
        ?LOG_ERROR("Replicator: error accessing doc ~s at ~s, reason: ~s",
            [DocId, couch_util:url_strip_password(Url), Err]),
        []
    end.

reader_loop(ReaderServer, Parent, Source1, DocIds) when is_list(DocIds) ->
    case Source1 of
    #http_db{} ->
        [gen_server:call(ReaderServer, {open_remote_doc, Id, nil, nil},
            infinity) || Id <- DocIds];
    _LocalDb ->
        {ok, Source} = gen_server:call(Parent, get_source_db, infinity),
        Docs = lists:foldr(fun(Id, Acc) ->
            case couch_db:open_doc(Source, Id) of
            {ok, Doc} ->
                [Doc | Acc];
            _ ->
                Acc
            end
        end, [], DocIds),
        gen_server:call(ReaderServer, {add_docs, nil, Docs}, infinity)
    end,
    exit(complete);
    
reader_loop(ReaderServer, Parent, Source, MissingRevsServer) ->
    case couch_rep_missing_revs:next(MissingRevsServer) of
    complete ->
        exit(complete);
    {HighSeq, IdsRevs} ->
        % to be safe, make sure Results are sorted by source_seq
        SortedIdsRevs = lists:keysort(2, IdsRevs),
        RequestSeqs = [S || {_,S,_} <- SortedIdsRevs],
        gen_server:call(ReaderServer, {add_request_seqs, RequestSeqs}, infinity),
        case Source of
        #http_db{} ->
            [gen_server:call(ReaderServer, {open_remote_doc, Id, Seq, Revs},
                infinity) || {Id,Seq,Revs} <- SortedIdsRevs],
            reader_loop(ReaderServer, Parent, Source, MissingRevsServer);
        _Local ->
            {ok, Source1} = gen_server:call(Parent, get_source_db, infinity),
            Source2 = maybe_reopen_db(Source1, HighSeq),
            lists:foreach(fun({Id,Seq,Revs}) ->
                {ok, Docs} = couch_db:open_doc_revs(Source2, Id, Revs, [latest]),
                JustTheDocs = [Doc || {ok, Doc} <- Docs],
                gen_server:call(ReaderServer, {add_docs, Seq, JustTheDocs},
                    infinity)
            end, SortedIdsRevs),
            couch_db:close(Source2),
            reader_loop(ReaderServer, Parent, Source2, MissingRevsServer)
        end
    end.

maybe_reopen_db(#db{update_seq=OldSeq} = Db, HighSeq) when HighSeq > OldSeq ->
    {ok, NewDb} = couch_db:open(Db#db.name, [{user_ctx, Db#db.user_ctx}]),
    NewDb;
maybe_reopen_db(Db, _HighSeq) ->
    Db.

spawn_document_request(Source, Id, nil, nil) ->
    spawn_document_request(Source, Id);
spawn_document_request(Source, Id, Seq, Revs) ->
    Server = self(),
    SpawnFun = fun() ->
        Results = open_doc_revs(Source, Id, Revs),
        gen_server:call(Server, {add_docs, Seq, Results}, infinity)
    end,
    spawn_monitor(SpawnFun).

spawn_document_request(Source, Id) ->
    Server = self(),
    SpawnFun = fun() ->
        Results = open_doc(Source, Id),
        gen_server:call(Server, {add_docs, nil, Results}, infinity)
    end,
    spawn_monitor(SpawnFun).
