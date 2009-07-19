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

-export([start/2, start_link/2, next/1, stop/1]).

-define(MIN_BUFFER_SIZE, 100).

-include("couch_db.hrl").
-include("../ibrowse/ibrowse.hrl").

-record (remote, {
    conn = nil,
    reqid = nil,
    complete = false,
    count = 0,
    reply_to = nil,
    rows = queue:new()
}).

-record (local, {
    changes_from = nil,
    changes_pid = nil,
    complete = false,
    count = 0,
    reply_to = nil,
    rows = queue:new()
}).

start(Url, Options) ->
    gen_server:start(?MODULE, [Url, Options], []).

start_link(Url, Options) ->
    gen_server:start_link(?MODULE, [Url, Options], []).

next(Server) ->
    gen_server:call(Server, next_change, infinity).

stop(Server) ->
    gen_server:call(Server, stop).

init([{remote, Url}, Options]) ->
    Since = proplists:get_value(since, Options, 0),
    Continuous = proplists:get_value(continuous, Options, false),
    {Pid, ReqId} = start_http_request(lists:concat([Url, "/_changes",
        "?style=all_docs", "&since=", Since, "&continuous=", Continuous])),
    {ok, #remote{conn=Pid, reqid=ReqId}};

init([{local, DbName}, Options]) when is_list(DbName) ->
    init([{local, ?l2b(DbName)}, Options]);
init([{local, DbName}, Options]) ->
    ?LOG_DEBUG("initializing local changes feed for ~s with ~p", [DbName, Options]),
    process_flag(trap_exit, true),
    Server = self(),
    Since = proplists:get_value(since, Options, 0),
    ChangesPid =
    case proplists:get_value(continuous, Options, false) of
    false ->
        spawn_link(fun() -> send_local_changes_once(Server, DbName, Since) end);
    true ->
        spawn_link(fun() -> send_local_changes_forever(Server, DbName, Since) end)
    end,
    {ok, #local{changes_pid=ChangesPid}}.

handle_call({add, Row}, _From, #local{count=Count, rows=Rows}=State) 
        when Count < ?MIN_BUFFER_SIZE->
    case State of
    #local{reply_to=nil} ->
        {reply, ok, State#local{count=Count+1, rows = queue:in(Row, Rows)}};
    #local{count=0, reply_to=Requestor}->
        gen_server:reply(Requestor, Row),
        {reply, ok, State#local{reply_to=nil}}
    end;
handle_call({add, Row}, From, #local{}=State) ->
    #local{
        count = Count,
        rows = Rows
    } = State,
    {noreply, State#local{count=Count+1, changes_from=From, rows=queue:in(Row,Rows)}};

handle_call(next_change, From, #local{count=0}=State) ->
    if State#local.complete ->
        {stop, normal, complete, State};
    true ->
        {noreply, State#local{reply_to=From}}
    end;
handle_call(next_change, _From, #local{}=State) ->
    #local{
        count = Count,
        changes_from = ChangesFrom,
        rows = Rows
    } = State,
    {{value, Row}, NewRows} = queue:out(Rows),
    if Count =:= ?MIN_BUFFER_SIZE, ChangesFrom =/= nil ->
        gen_server:reply(ChangesFrom, ok),
        {reply, Row, State#local{count=Count-1, changes_from=nil, rows=NewRows}};
    true ->
        {reply, Row, State#local{count=Count-1, rows=NewRows}}
    end;

handle_call(next_change, From, #remote{count=0}=State) ->
    if State#remote.complete ->
        {stop, normal, complete, State};
    true ->
        {noreply, State#remote{reply_to=From}}
    end;
handle_call(next_change, _From, #remote{}=State) ->
    #remote{
        reqid = Id,
        complete = Complete,
        count = Count,
        rows = Rows
    } = State,
    ok = maybe_stream_next(Complete, Count, Id),
    {{value, Row}, NewRows} = queue:out(Rows),
    {reply, Row, State#remote{count=Count-1, rows=NewRows}};

handle_call(stop, _From, #local{changes_pid=ChangesPid} = State) ->
    exit(ChangesPid, stop),
    {stop, normal, ok, State};

handle_call(stop, _From, #remote{conn=Conn} = State) ->
    catch ibrowse:stop_worker_process(Conn),
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ibrowse_async_headers, Id, "200", _}, #remote{reqid=Id}=State) ->
    #remote{
        complete = Complete,
        count = Count
    } = State,
    ?LOG_DEBUG("~p reqid ~p ibrowse_async_headers 200", [?MODULE, Id]),
    ok = maybe_stream_next(Complete, Count, Id),
    {noreply, State};
handle_info({ibrowse_async_headers, Id, "301", Hdrs}, #remote{reqid=Id}=State) ->
    ?LOG_DEBUG("~p reqid ~p ibrowse_async_headers 301", [?MODULE, Id]),
    catch ibrowse:stop_worker_process(State#remote.conn),
    Url = mochiweb_headers:get_value("Location", mochiweb_headers:make(Hdrs)),
    {Pid, ReqId} = start_http_request(Url),
    {noreply, State#remote{conn=Pid, reqid=ReqId}};
handle_info({ibrowse_async_headers, Id, Code, Hdrs}, #remote{reqid=Id}=State) ->
    ?LOG_ERROR("replicator changes feed failed with code ~s and Headers ~n~p",
        [Code,Hdrs]),
    {stop, {error, list_to_integer(Code)}, State};

handle_info({ibrowse_async_response, Id, Msg}, #remote{reqid=Id} = State) ->
    ?LOG_DEBUG("~p reqid ~p ibrowse_async_response ~p", [?MODULE, Id, Msg]),
    #remote{
        complete = Complete,
        count = Count,
        rows = Rows
    } = State,
    try
        Row = decode_row(Msg),
        case State of
        #remote{reply_to=nil} ->
            {noreply, State#remote{count=Count+1, rows = queue:in(Row, Rows)}};
        #remote{count=0, reply_to=From}->
            gen_server:reply(From, Row),
            {noreply, State#remote{reply_to=nil}}
        end
    catch
    throw:{invalid_json, Msg} ->
        ?LOG_DEBUG("got invalid_json ~p", [Msg]),
        ok = maybe_stream_next(Complete, Count, Id),
        {noreply, State}
    end;

handle_info({ibrowse_async_response_end, Id}, #remote{reqid=Id} = State) ->
    ?LOG_DEBUG("got ibrowse_async_response_end ~p", [State#remote.reply_to]),
    case State of
    #remote{reply_to=nil} ->
        {noreply, State#remote{complete=true}};
    #remote{count=0, reply_to=From}->
        gen_server:reply(From, complete),
        {stop, normal, State}
    end;

handle_info({'EXIT', From, normal}, #local{changes_pid=From} = State) ->
    if State#local.reply_to =/= nil ->
        gen_server:reply(State#local.reply_to, complete),
        {stop, normal, State};
    true ->
        {noreply, State#local{complete=true}}
    end;
handle_info({'EXIT', From, Reason}, #local{changes_pid=From} = State) ->
    ?LOG_ERROR("changes_pid died with reason ~p", [Reason]),
    {stop, changes_pid_died, State};

handle_info(Msg, State) ->
    ?LOG_INFO("unexpected message ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, #remote{conn=Pid}) when is_pid(Pid) ->
    catch ibrowse:stop_worker_process(Pid),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%internal funs

decode_row([$,, $\n | Rest]) ->
    decode_row(Rest);
decode_row(Row) ->
    ?JSON_DECODE(Row).

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

maybe_stream_next(false, Count, Id) when Count < ?MIN_BUFFER_SIZE ->
    ?LOG_DEBUG("~p reqid ~p streaming next chunk", [?MODULE, Id]),
    ibrowse:stream_next(Id);
maybe_stream_next(_Complete, _Count, Id) ->
    ?LOG_DEBUG("~p reqid ~p not streaming", [?MODULE, Id]),
    ok.

send_local_changes_forever(Server, DbName, Since) ->
    Self = self(),
    {ok, _} = couch_db_update_notifier:start_link(
        fun(Msg) -> local_update_notification(Self, DbName, Msg) end),
    {ok, NewSeq} = send_local_changes_once(Server, DbName, Since),
    ok = wait_db_updated(),
    send_local_changes_forever(Server, DbName, NewSeq).

send_local_changes_once(Server, DbName, Since) ->
    {ok, Db} = couch_db:open(DbName, []),

    FilterFun =
    fun(#doc_info{revs=[#rev_info{rev=Rev}|_]}) ->
        {[{<<"rev">>, couch_doc:rev_to_str(Rev)}]}
    end,

    ChangesFun =
    fun([#doc_info{id=Id, high_seq=Seq}|_]=DocInfos, _) ->
        Results0 = [FilterFun(DocInfo) || DocInfo <- DocInfos],
        Results = [Result || Result <- Results0, Result /= null],
        if Results /= [] ->
            Change = {[{<<"seq">>,Seq}, {<<"id">>,Id}, {<<"changes">>,Results}]},
            gen_server:call(Server, {add, Change}, infinity);
        true ->
            ?LOG_DEBUG("Results was empty ~p", [Results0]),
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
        {inactivity_timeout, 30000}
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
