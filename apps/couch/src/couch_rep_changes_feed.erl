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
    init_args,
    last_seq,
    conn = nil,
    reqid = nil,
    complete = false,
    count = 0,
    partial_chunk = <<>>,
    reply_to = nil,
    rows = queue:new()
}).

start_link(Parent, Source, StartSeq, PostProps) ->
    gen_server:start_link(?MODULE, [Parent, Source, StartSeq, PostProps], []).

next(Server) ->
    gen_server:call(Server, next_changes, infinity).

stop(Server) ->
    gen_server:call(Server, stop).

init([_Parent, #http_db{}=Source, Since, PostProps] = Args) ->
    process_flag(trap_exit, true),
    Feed = case couch_util:get_value(<<"continuous">>, PostProps, false) of
    false ->
        normal;
    true ->
        continuous
    end,
    BaseQS = [
        {"style", all_docs},
        {"heartbeat", 10000},
        {"since", Since},
        {"feed", Feed}
    ],
    QS = case couch_util:get_value(<<"filter">>, PostProps) of
    undefined ->
        BaseQS;
    FilterName ->
        {Params} = couch_util:get_value(<<"query_params">>, PostProps, {[]}),
        lists:foldr(
            fun({K, V}, QSAcc) ->
                Ks = couch_util:to_list(K),
                case proplists:is_defined(Ks, QSAcc) of
                true ->
                    QSAcc;
                false ->
                    [{Ks, V} | QSAcc]
                end
            end,
            [{"filter", FilterName} | BaseQS],
            Params
        )
    end,
    Pid = couch_rep_httpc:spawn_link_worker_process(Source),
    Req = Source#http_db{
        resource = "_changes",
        qs = QS,
        conn = Pid,
        options = [{stream_to, {self(), once}}, {response_format, binary}],
        headers = Source#http_db.headers -- [{"Accept-Encoding", "gzip"}]
    },
    {ibrowse_req_id, ReqId} = couch_rep_httpc:request(Req),

    receive
    {ibrowse_async_headers, ReqId, "200", _} ->
        ibrowse:stream_next(ReqId),
        {ok, #state{conn=Pid, last_seq=Since, reqid=ReqId, init_args=Args}};
    {ibrowse_async_headers, ReqId, Code, Hdrs} when Code=="301"; Code=="302" ->
        catch ibrowse:stop_worker_process(Pid),
        Url2 = mochiweb_headers:get_value("Location", mochiweb_headers:make(Hdrs)),
        %% TODO use couch_httpc:request instead of start_http_request
        {Pid2, ReqId2} = start_http_request(Url2),
        receive {ibrowse_async_headers, ReqId2, "200", _} ->
            {ok, #state{conn=Pid2, last_seq=Since, reqid=ReqId2, init_args=Args}}
        after 30000 ->
            {stop, changes_timeout}
        end;
    {ibrowse_async_headers, ReqId, "404", _} ->
        catch ibrowse:stop_worker_process(Pid),
        ?LOG_INFO("source doesn't have _changes, trying _all_docs_by_seq", []),
        Self = self(),
        BySeqPid = spawn_link(fun() -> by_seq_loop(Self, Source, Since) end),
        {ok, #state{last_seq=Since, changes_loop=BySeqPid, init_args=Args}};
    {ibrowse_async_headers, ReqId, Code, _} ->
        {stop, {changes_error_code, list_to_integer(Code)}}
    after 10000 ->
        {stop, changes_timeout}
    end;

init([_Parent, Source, Since, PostProps] = InitArgs) ->
    process_flag(trap_exit, true),
    Server = self(),
    ChangesArgs = #changes_args{
        style = all_docs,
        since = Since,
        filter = ?b2l(couch_util:get_value(<<"filter">>, PostProps, <<>>)),
        feed = case couch_util:get_value(<<"continuous">>, PostProps, false) of
            true ->
                "continuous";
            false ->
                "normal"
        end,
        timeout = infinity
    },
    ChangesPid = spawn_link(fun() ->
        ChangesFeedFun = couch_changes:handle_changes(
            ChangesArgs,
            {json_req, filter_json_req(Source, PostProps)},
            Source
        ),
        ChangesFeedFun(fun({change, Change, _}, _) ->
                gen_server:call(Server, {add_change, Change}, infinity);
            (_, _) ->
                ok
        end)
    end),
    {ok, #state{changes_loop=ChangesPid, init_args=InitArgs}}.

filter_json_req(Db, PostProps) ->
    case couch_util:get_value(<<"filter">>, PostProps) of
    undefined ->
        {[]};
    FilterName ->
        {Query} = couch_util:get_value(<<"query_params">>, PostProps, {[]}),
        {ok, Info} = couch_db:get_db_info(Db),
        % simulate a request to db_name/_changes
        {[
            {<<"info">>, {Info}},
            {<<"id">>, null},
            {<<"method">>, 'GET'},
            {<<"path">>, [couch_db:name(Db), <<"_changes">>]},
            {<<"query">>, {[{<<"filter">>, FilterName} | Query]}},
            {<<"headers">>, []},
            {<<"body">>, []},
            {<<"peer">>, <<"replicator">>},
            {<<"form">>, []},
            {<<"cookie">>, []},
            {<<"userCtx">>, couch_util:json_user_ctx(Db)}
       ]}
    end.

handle_call({add_change, Row}, From, State) ->
    handle_add_change(Row, From, State);

handle_call(next_changes, From, State) ->
    handle_next_changes(From, State);
    
handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ibrowse_async_headers, Id, Code, Hdrs}, #state{reqid=Id}=State) ->
    handle_headers(list_to_integer(Code), Hdrs, State);

handle_info({ibrowse_async_response, Id, {error,connection_closed}},
        #state{reqid=Id}=State) ->
    handle_retry(State);

handle_info({ibrowse_async_response, Id, {error,E}}, #state{reqid=Id}=State) ->
    {stop, {error, E}, State};

handle_info({ibrowse_async_response, Id, Chunk}, #state{reqid=Id}=State) ->
    Messages = [M || M <- re:split(Chunk, ",?\n", [trim]), M =/= <<>>],
    handle_messages(Messages, State);

handle_info({ibrowse_async_response_end, Id}, #state{reqid=Id} = State) ->
    handle_feed_completion(State);

handle_info({'EXIT', From, normal}, #state{changes_loop=From} = State) ->
    handle_feed_completion(State);

handle_info({'EXIT', From, Reason}, #state{changes_loop=From} = State) ->
    ?LOG_ERROR("changes_loop died with reason ~p", [Reason]),
    {stop, changes_loop_died, State};

handle_info({'EXIT', _From, normal}, State) ->
    {noreply, State};

handle_info(Msg, State) ->
    ?LOG_DEBUG("unexpected message at changes_feed ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, State) ->
    #state{
        changes_loop = ChangesPid,
        conn = Conn
    } = State,
    if is_pid(ChangesPid) -> exit(ChangesPid, stop); true -> ok end,
    if is_pid(Conn) -> catch ibrowse:stop_worker_process(Conn); true -> ok end,
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
    maybe_stream_next(NewState),
    if ChangesFrom =/= nil -> gen_server:reply(ChangesFrom, ok); true -> ok end,
    {reply, queue:to_list(Rows), NewState}.

handle_headers(200, _, State) ->
    maybe_stream_next(State),
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

handle_messages([], State) ->
    maybe_stream_next(State),
    {noreply, State};
handle_messages([<<"{\"results\":[">>|Rest], State) ->
    handle_messages(Rest, State);
handle_messages([<<"]">>, <<"\"last_seq\":", _/binary>>], State) ->
    handle_feed_completion(State);
handle_messages([<<"{\"last_seq\":", _/binary>>], State) ->
    handle_feed_completion(State);
handle_messages([Chunk|Rest], State) ->
    #state{
        count = Count,
        partial_chunk = Partial,
        rows = Rows
    } = State,
    NewState = try
        Row = {Props} = decode_row(<<Partial/binary, Chunk/binary>>),
        case State of
        #state{reply_to=nil} ->
            State#state{
                count = Count+1,
                last_seq = couch_util:get_value(<<"seq">>, Props),
                partial_chunk = <<>>,
                rows=queue:in(Row,Rows)
            };
        #state{count=0, reply_to=From}->
            gen_server:reply(From, [Row]),
            State#state{reply_to = nil, partial_chunk = <<>>}
        end
    catch
    throw:{invalid_json, Bad} ->
        State#state{partial_chunk = Bad}
    end,
    handle_messages(Rest, NewState).

handle_feed_completion(#state{reply_to=nil} = State)->
    {noreply, State#state{complete=true}};
handle_feed_completion(#state{count=0} = State) ->
    gen_server:reply(State#state.reply_to, complete),
    {stop, normal, State}.

handle_retry(State) ->
    ?LOG_DEBUG("retrying changes feed because our connection closed", []),
    #state{
        count = Count,
        init_args = [_, Source, _, PostProps],
        last_seq = Since,
        reply_to = ReplyTo,
        rows = Rows
    } = State,
    case init([nil, Source, Since, PostProps]) of
    {ok, State1} ->
        MergedState = State1#state{
            count = Count,
            reply_to = ReplyTo,
            rows = Rows
        },
        {noreply, MergedState};
    _ ->
        {stop, {error, connection_closed}, State}
    end.

by_seq_loop(Server, Source, StartSeq) ->
    Req = Source#http_db{
        resource = "_all_docs_by_seq",
        qs = [{limit, 1000}, {startkey, StartSeq}]
    },
    {Results} = couch_rep_httpc:request(Req),
    Rows = couch_util:get_value(<<"rows">>, Results),
    if Rows =:= [] -> exit(normal); true -> ok end,
    EndSeq = lists:foldl(fun({RowInfoList}, _) ->
        Id = couch_util:get_value(<<"id">>, RowInfoList),
        Seq = couch_util:get_value(<<"key">>, RowInfoList),
        {RowProps} = couch_util:get_value(<<"value">>, RowInfoList),
        RawRevs = [
            couch_util:get_value(<<"rev">>, RowProps),
            couch_util:get_value(<<"conflicts">>, RowProps, []),
            couch_util:get_value(<<"deleted_conflicts">>, RowProps, [])
        ],
        ParsedRevs = couch_doc:parse_revs(lists:flatten(RawRevs)),
        Change = {[
            {<<"seq">>, Seq},
            {<<"id">>, Id},
            {<<"changes">>, [{[{<<"rev">>,R}]} || R <- ParsedRevs]}
        ]},
        gen_server:call(Server, {add_change, Change}, infinity),
        Seq
    end, 0, Rows),
    by_seq_loop(Server, Source, EndSeq).

decode_row(<<",", Rest/binary>>) ->
    decode_row(Rest);
decode_row(Row) ->
    ?JSON_DECODE(Row).

maybe_stream_next(#state{reqid=nil}) ->
    ok;
maybe_stream_next(#state{complete=false, count=N} = S) when N < ?BUFFER_SIZE ->
    timer:cancel(get(timeout)),
    {ok, Timeout} = timer:exit_after(31000, changes_timeout),
    put(timeout, Timeout),
    ibrowse:stream_next(S#state.reqid);
maybe_stream_next(_) ->
    timer:cancel(get(timeout)).

start_http_request(RawUrl) ->
    Url = ibrowse_lib:parse_url(RawUrl),
    {ok, Pid} = ibrowse:spawn_link_worker_process(Url#url.host, Url#url.port),
    Opts = [
        {stream_to, {self(), once}},
        {inactivity_timeout, 31000},
        {response_format, binary}
    ],
    {ibrowse_req_id, Id} =
        ibrowse:send_req_direct(Pid, RawUrl, [], get, [], Opts, infinity),
    {Pid, Id}.
