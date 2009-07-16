% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
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

-record (state, {
    conn = nil,
    reqid = nil,
    count = 0,
    reply_to = nil,
    rows = queue:new(),
    complete = false
}).

start(Url, Options) ->
    gen_server:start(?MODULE, [Url, Options], []).

start_link(Url, Options) ->
    gen_server:start_link(?MODULE, [Url, Options], []).

next(Server) ->
    ?LOG_DEBUG("~p at ~p received next_change call", [?MODULE, Server]),
    gen_server:call(Server, next_change, infinity).

stop(Server) ->
    gen_server:call(Server, stop).

init([{remote, Url}, Options]) ->
    Since = proplists:get_value(since, Options, 0),
    Continuous = proplists:get_value(continuous, Options, false),
    {Pid, ReqId} = start_http_request(lists:concat([Url, "/_changes",
        "?style=all_docs", "&since=", Since, "&continuous=", Continuous])),
    {ok, #state{conn=Pid, reqid=ReqId}};

init([{local, DbName}, Options]) ->
    init([{remote, "http://" ++ couch_config:get("httpd", "bind_address") ++ ":"
        ++ couch_config:get("httpd", "port") ++ "/" ++ DbName}, Options]).

handle_call(next_change, From, State) ->
    #state{
        reqid = Id,
        complete = Complete,
        count = Count,
        rows = Rows
    } = State,
    
    ok = maybe_stream_next(Complete, Count, Id),
    
    case queue:out(Rows) of
    {{value, Row}, NewRows} ->
        {reply, Row, State#state{count=Count-1, rows=NewRows}};
    {empty, Rows} ->
        if State#state.complete ->
            {stop, normal, complete, State};
        % State#state.waiting_on_headers ->
        %     {noreply, State#state{reply_to=From}};
        true ->
            {noreply, State#state{reply_to=From}}
        end
    end;

handle_call(stop, _From, State) ->
    catch ibrowse:stop_worker_process(State#state.conn),
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ibrowse_async_headers, Id, "200", _}, #state{reqid=Id}=State) ->
    #state{
        complete = Complete,
        count = Count
    } = State,
    ?LOG_DEBUG("~p reqid ~p ibrowse_async_headers 200", [?MODULE, Id]),
    ok = maybe_stream_next(Complete, Count, Id),
    {noreply, State};
handle_info({ibrowse_async_headers, Id, "301", Hdrs}, #state{reqid=Id}=State) ->
    ?LOG_DEBUG("~p reqid ~p ibrowse_async_headers 301", [?MODULE, Id]),
    catch ibrowse:stop_worker_process(State#state.conn),
    Url = mochiweb_headers:get_value("Location", mochiweb_headers:make(Hdrs)),
    {Pid, ReqId} = start_http_request(Url),
    {noreply, State#state{conn=Pid, reqid=ReqId}};
handle_info({ibrowse_async_headers, Id, Code, Hdrs}, #state{reqid=Id}=State) ->
    ?LOG_ERROR("replicator changes feed failed with code ~s and Headers ~n~p",
        [Code,Hdrs]),
    {stop, {error, list_to_integer(Code)}, State};

handle_info({ibrowse_async_response, Id, Msg}, #state{reqid=Id} = State) ->
    ?LOG_DEBUG("~p reqid ~p ibrowse_async_response ~p", [?MODULE, Id, Msg]),
    #state{
        complete = Complete,
        count = Count,
        rows = Rows
    } = State,
    try
        Row = decode_row(Msg),
        case State of
        #state{reply_to=nil} ->
            {noreply, State#state{count=Count+1, rows = queue:in(Row, Rows)}};
        #state{count=0, reply_to=From}->
            gen_server:reply(From, Row),
            {noreply, State#state{reply_to=nil}}
        end
    catch
    throw:{invalid_json, Msg} ->
        ?LOG_DEBUG("got invalid_json ~p", [Msg]),
        ok = maybe_stream_next(Complete, Count, Id),
        {noreply, State}
    end;

handle_info({ibrowse_async_response_end, Id}, #state{reqid=Id} = State) ->
    ?LOG_DEBUG("got ibrowse_async_response_end ~p", [State#state.reply_to]),
    case State of
    #state{reply_to=nil} ->
        {noreply, State#state{complete=true}};
    #state{count=0, reply_to=From}->
        gen_server:reply(From, complete),
        {stop, normal, State}
    end;

handle_info(Msg, State) ->
    ?LOG_INFO("unexpected message ~p", [Msg]),
    {noreply, State}.

terminate(_Reason, #state{conn=Pid}) when is_pid(Pid) ->
    catch ibrowse:stop_worker_process(Pid),
    ok;
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

decode_row([$,, $\n | Rest]) ->
    decode_row(Rest);
decode_row(Row) ->
    ?JSON_DECODE(Row).

maybe_stream_next(false, Count, Id) when Count < ?MIN_BUFFER_SIZE ->
    ?LOG_DEBUG("~p reqid ~p streaming next chunk", [?MODULE, Id]),
    ibrowse:stream_next(Id);
maybe_stream_next(_Complete, _Count, Id) ->
    ?LOG_DEBUG("~p reqid ~p not streaming", [?MODULE, Id]),
    ok.

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
