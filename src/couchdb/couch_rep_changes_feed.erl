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

-module(couch_rep_changes_feed).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
    code_change/3]).

-export([start_link/4, next/1, stop/1]).

-define(BUFFER_SIZE, 1000).

-include("couch_db.hrl").
-include("../ibrowse/ibrowse.hrl").

-record (state, {
    changes_from = nil,
    changes_loop = nil,
    last_seq,
    conn = nil,
    reqid = nil,
    complete = false,
    count = 0,
    partial_chunk = nil,
    reply_to = nil,
    rows = queue:new()
}).

start_link(Parent, Source, StartSeq, PostProps) ->
    gen_server:start_link(?MODULE, [Parent, Source, StartSeq, PostProps], []).

next(Server) ->
    gen_server:call(Server, next_changes, infinity).

stop(Server) ->
    gen_server:call(Server, stop).

init([_Parent, #http_db{}=Source, Since, PostProps]) ->
    process_flag(trap_exit, true),
    Feed = case proplists:get_value(<<"continuous">>, PostProps, false) of
    false ->
        normal;
    true ->
        continuous
    end,
    Pid = couch_rep_httpc:spawn_link_worker_process(Source),
    Req = Source#http_db{
        resource = "_changes",
        qs = [{style, all_docs}, {heartbeat, true}, {since, Since},
            {feed, Feed}],
        conn = Pid,
        options = [{stream_to, {self(), once}}, {response_format, binary}],
        headers = Source#http_db.headers -- [{"Accept-Encoding", "gzip"}]
    },    
    {ibrowse_req_id, ReqId} = couch_rep_httpc:request(Req),

    receive
    {ibrowse_async_headers, ReqId, "200", _} ->
        ibrowse:stream_next(ReqId),
        {ok, #state{conn=Pid, last_seq=Since, reqid=ReqId}};
    {ibrowse_async_headers, ReqId, "301", Hdrs} ->
        catch ibrowse:stop_worker_process(Pid),
        Url2 = mochiweb_headers:get_value("Location", mochiweb_headers:make(Hdrs)),
        %% TODO use couch_httpc:request instead of start_http_request
        {Pid2, ReqId2} = start_http_request(Url2),
        receive {ibrowse_async_headers, ReqId2, "200", _} ->
            {ok, #state{conn=Pid2, last_seq=Since, reqid=ReqId2}}
        after 30000 ->
            {stop, changes_timeout}
        end;
    {ibrowse_async_headers, ReqId, "404", _} ->
        catch ibrowse:stop_worker_process(Pid),
        ?LOG_INFO("source doesn't have _changes, trying _all_docs_by_seq", []),
        Self = self(),
        BySeqPid = spawn_link(fun() -> by_seq_loop(Self, Source, Since) end),
        {ok, #state{last_seq=Since, changes_loop=BySeqPid}};
    {ibrowse_async_headers, ReqId, Code, _} ->
        {stop, {changes_error_code, list_to_integer(Code)}}
    after 10000 ->
        {stop, changes_timeout}
    end;

init([_Parent, Source, Since, PostProps]) ->
    process_flag(trap_exit, true),
    Server = self(),
    ChangesPid =
    case proplists:get_value(<<"continuous">>, PostProps, false) of
    false ->
        spawn_link(fun() -> send_local_changes_once(Server, Source, Since) end);
    true ->
        spawn_link(fun() -> send_local_changes_forever(Server, Source, Since) end)
    end,
    {ok, #state{changes_loop=ChangesPid}}.

handle_call({add_change, Row}, From, State) ->
    handle_add_change(Row, From, State);

handle_call(next_changes, From, State) ->
    handle_next_changes(From, State);
    
handle_call(stop, _From, State) ->
    #state{
        changes_loop = ChangesPid,
        conn = Conn
    } = State,
    if is_pid(ChangesPid) -> exit(ChangesPid, stop); true -> ok end,
    if is_pid(Conn) -> catch ibrowse:stop_worker_process(Conn); true -> ok end,
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ibrowse_async_headers, Id, Code, Hdrs}, #state{reqid=Id}=State) ->
    handle_headers(list_to_integer(Code), Hdrs, State);

handle_info({ibrowse_async_response, Id, Msg}, #state{reqid=Id} = State) ->
    handle_response(Msg, State);

handle_info({ibrowse_async_response_end, Id}, #state{reqid=Id} = State) ->
    handle_feed_completion(State);

handle_info({'EXIT', From, normal}, #state{changes_loop=From} = State) ->
    handle_feed_completion(State);

handle_info({'EXIT', From, Reason}, #state{changes_loop=From} = State) ->
    ?LOG_ERROR("changes_loop died with reason ~p", [Reason]),
    {stop, changes_loop_died, State};

handle_info(Msg, State) ->
    ?LOG_DEBUG("unexpected message at changes_feed ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{conn=Pid}) when is_pid(Pid) ->
    catch ibrowse:stop_worker_process(Pid),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%internal funs

handle_add_change(Row, From, #state{reply_to=nil} = State) ->
    #state{
        count = Count,
        rows = Rows
    } = State,
    NewState = State#state{count=Count+1, rows=queue:in(Row,Rows)},
    if Count < ?BUFFER_SIZE ->
        {reply, ok, NewState};
    true ->
        {noreply, NewState#state{changes_from=From}}
    end;
handle_add_change(Row, _From, #state{count=0} = State) ->
    gen_server:reply(State#state.reply_to, [Row]),
    {reply, ok, State#state{reply_to=nil}}.

handle_next_changes(From, #state{count=0}=State) ->
    if State#state.complete ->
        {stop, normal, complete, State};
    true ->
        {noreply, State#state{reply_to=From}}
    end;
handle_next_changes(_From, State) ->
    #state{
        changes_from = ChangesFrom,
        rows = Rows
    } = State,
    NewState = State#state{count=0, changes_from=nil, rows=queue:new()},
    ok = maybe_stream_next(NewState),
    if ChangesFrom =/= nil -> gen_server:reply(ChangesFrom, ok); true -> ok end,
    {reply, queue:to_list(Rows), NewState}.

handle_headers(200, _, State) ->
    ok = maybe_stream_next(State),
    {noreply, State};
handle_headers(301, Hdrs, State) ->
    catch ibrowse:stop_worker_process(State#state.conn),
    Url = mochiweb_headers:get_value("Location", mochiweb_headers:make(Hdrs)),
    %% TODO use couch_httpc:request instead of start_http_request
    {Pid, ReqId} = start_http_request(Url),
    {noreply, State#state{conn=Pid, reqid=ReqId}};
handle_headers(Code, Hdrs, State) ->
    ?LOG_ERROR("replicator changes feed failed with code ~s and Headers ~n~p",
        [Code,Hdrs]),
    {stop, {error, Code}, State}.

handle_response({error, Reason}, State) ->
    {stop, {error, Reason}, State};
handle_response(<<"\n">>, State) ->
    ?LOG_DEBUG("got a heartbeat from the remote server", []),
    ok = maybe_stream_next(State),
    {noreply, State};
handle_response(<<"{\"results\":[\n">>, State) ->
    ok = maybe_stream_next(State),
    {noreply, State};
handle_response(<<"\n],\n\"last_seq\":", LastSeqStr/binary>>, State) ->
    LastSeq = list_to_integer(?b2l(hd(re:split(LastSeqStr, "}")))),
    {noreply, State#state{last_seq = LastSeq}};
handle_response(<<"{\"last_seq\":", LastSeqStr/binary>>, State) ->
    LastSeq = list_to_integer(?b2l(hd(re:split(LastSeqStr, "}")))),
    {noreply, State#state{last_seq = LastSeq}};
handle_response(Chunk, #state{partial_chunk=nil} = State) ->
    #state{
        count = Count,
        rows = Rows
    } = State,
    ok = maybe_stream_next(State),
    try
        Row = decode_row(Chunk),
        case State of
        #state{reply_to=nil} ->
            {noreply, State#state{count=Count+1, rows = queue:in(Row, Rows)}};
        #state{count=0, reply_to=From}->
            gen_server:reply(From, [Row]),
            {noreply, State#state{reply_to=nil}}
        end
    catch
    throw:{invalid_json, Bad} ->
        {noreply, State#state{partial_chunk = Bad}}
    end;
handle_response(Chunk, State) ->
    #state{
        count = Count,
        partial_chunk = Partial,
        rows = Rows
    } = State,
    ok = maybe_stream_next(State),
    try
        Row = decode_row(<<Partial/binary, Chunk/binary>>),
        {noreply, case State of
        #state{reply_to=nil} ->
            State#state{count=Count+1, partial_chunk=nil, rows=queue:in(Row,Rows)};
        #state{count=0, reply_to=From}->
            gen_server:reply(From, [Row]),
            State#state{reply_to=nil, partial_chunk=nil}
        end}
    catch
    throw:{invalid_json, Bad} ->
        {noreply, State#state{partial_chunk = Bad}}
    end.

handle_feed_completion(#state{reply_to=nil} = State)->
    {noreply, State#state{complete=true}};
handle_feed_completion(#state{count=0} = State) ->
    gen_server:reply(State#state.reply_to, complete),
    {stop, normal, State}.

by_seq_loop(Server, Source, StartSeq) ->
    Req = Source#http_db{
        resource = "_all_docs_by_seq",
        qs = [{limit, 1000}, {startkey, StartSeq}]
    },
    {Results} = couch_rep_httpc:request(Req),
    Rows = proplists:get_value(<<"rows">>, Results),
    if Rows =:= [] -> exit(normal); true -> ok end,
    EndSeq = lists:foldl(fun({RowInfoList}, _) ->
        Id = proplists:get_value(<<"id">>, RowInfoList),
        Seq = proplists:get_value(<<"key">>, RowInfoList),
        {RowProps} = proplists:get_value(<<"value">>, RowInfoList),
        RawRevs = [
            proplists:get_value(<<"rev">>, RowProps),
            proplists:get_value(<<"conflicts">>, RowProps, []),
            proplists:get_value(<<"deleted_conflicts">>, RowProps, [])
        ],
        ParsedRevs = couch_doc:parse_revs(lists:flatten(RawRevs)),
        Change = {[
            {<<"seq">>, Seq},
            {<<"id">>, Id},
            {<<"changes">>, [{[{<<"rev">>,R}]} || R <- ParsedRevs]}
        ]},
        gen_server:call(Server, {add_change, Change}),
        Seq
    end, 0, Rows),
    by_seq_loop(Server, Source, EndSeq+1).

decode_row(<<",\n", Rest/binary>>) ->
    decode_row(Rest);
decode_row(Row) ->
    {[Seq, Id, {<<"changes">>,C}]} = ?JSON_DECODE(Row),
    C2 = [{[{<<"rev">>,couch_doc:parse_rev(R)}]} || {[{<<"rev">>,R}]} <- C],
    {[Seq, Id, {<<"changes">>,C2}]}.

flush_updated_messages() ->
    receive updated -> flush_updated_messages()
    after 0 -> ok
    end.

local_update_notification(Self, DbName, {updated, DbName}) ->
    Self ! updated;
local_update_notification(Self, DbName, {deleted, DbName}) ->
    Self ! deleted;
local_update_notification(_, _, _) ->
    ok.

maybe_stream_next(#state{reqid=nil}) ->
    ok;
maybe_stream_next(#state{complete=false, count=N} = S) when N < ?BUFFER_SIZE ->
    ibrowse:stream_next(S#state.reqid);
maybe_stream_next(_) ->
    ok.

send_local_changes_forever(Server, Db, Since) ->
    #db{name = DbName, user_ctx = UserCtx} = Db,
    Self = self(),
    {ok, _} = couch_db_update_notifier:start_link(
        fun(Msg) -> local_update_notification(Self, DbName, Msg) end),
    {ok, NewSeq} = send_local_changes_once(Server, Db, Since),
    couch_db:close(Db),
    ok = wait_db_updated(),
    {ok, NewDb} = couch_db:open(DbName, [{user_ctx, UserCtx}]),
    send_local_changes_forever(Server, NewDb, NewSeq).

send_local_changes_once(Server, Db, Since) ->
    FilterFun =
    fun(#doc_info{revs=[#rev_info{rev=Rev}|_]}) ->
        {[{<<"rev">>, Rev}]}
    end,

    ChangesFun =
    fun([#doc_info{id=Id, high_seq=Seq}|_]=DocInfos, _) ->
        Results0 = [FilterFun(DocInfo) || DocInfo <- DocInfos],
        Results = [Result || Result <- Results0, Result /= null],
        if Results /= [] ->
            Change = {[{<<"seq">>,Seq}, {<<"id">>,Id}, {<<"changes">>,Results}]},
            gen_server:call(Server, {add_change, Change}, infinity);
        true ->
            ok
        end,
        {ok, Seq}
    end,

    couch_db:changes_since(Db, all_docs, Since, ChangesFun, Since).

start_http_request(RawUrl) ->
    Url = ibrowse_lib:parse_url(RawUrl),
    {ok, Pid} = ibrowse:spawn_link_worker_process(Url#url.host, Url#url.port),
    Opts = [
        {stream_to, {self(), once}},
        {inactivity_timeout, 30000},
        {response_format, binary}
    ],
    {ibrowse_req_id, Id} = 
        ibrowse:send_req_direct(Pid, RawUrl, [], get, [], Opts, infinity),
    {Pid, Id}.

wait_db_updated() ->
    receive deleted ->
        exit(deleted)
    after 0 ->
        receive updated ->
            flush_updated_messages()
        end
    end.
