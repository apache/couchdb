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

-module(couch_ft_query).
-behaviour(gen_server).

-export([start_link/1, execute/2]).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,code_change/3, stop/0]).

-define(ERR_HANDLE, {Port, {exit_status, Status}} -> {stop, {unknown_error, Status}, {unknown_error, Status}, Port}).

start_link(QueryExec) ->
    gen_server:start_link({local, couch_ft_query}, couch_ft_query, QueryExec, []).

stop() ->
    exit(whereis(couch_ft_query), close).

execute(DatabaseName, QueryString) ->
    gen_server:call(couch_ft_query, {ft_query, DatabaseName, QueryString}).

init(QueryExec) ->
    Port = open_port({spawn, QueryExec}, [{line, 1000}, exit_status, hide]),
    {ok, Port}.

terminate(_Reason, _Server) ->
    ok.

handle_call({ft_query, Database, QueryText}, _From, Port) ->
    %% send the database name
    true = port_command(Port, Database ++ "\n"),
    true = port_command(Port, QueryText ++ "\n"),
    case get_line(Port) of
    "ok" ->
        DocIds = read_query_results(Port, []),
        {reply, {ok, DocIds}, Port};
    "error" ->
        ErrorId = get_line(Port),
        ErrorMsg = get_line(Port),
        {reply, {list_to_atom(ErrorId), ErrorMsg}, Port}
    end.

read_query_results(Port, Acc) ->
    case get_line(Port) of
    "" -> % line by itself means all done
        lists:reverse(Acc);
    DocId ->
        Score = get_line(Port),
        read_query_results(Port, [{DocId, Score} | Acc])
    end.


get_line(Port) ->
    receive
    {Port, {data, {eol, Line}}} ->
        Line;
    ?ERR_HANDLE
    end.

handle_cast(_Whatever, State) ->
    {noreply, State}.

handle_info({Port, {exit_status, Status}}, Port) ->
    {stop, {os_process_exited, Status}, Port};
handle_info(_Whatever, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
