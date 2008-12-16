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

-module(couch_external_server).
-behaviour(gen_server).

-export([start_link/2, stop/1, execute/8]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2, code_change/3]). 

-define(TIMEOUT, 5000).

-include("couch_db.hrl").

% External API

start_link(Name, Command) ->
    gen_server:start_link(couch_external_server, [Name, Command], []).

stop(Pid) ->
    gen_server:cast(Pid, stop).

execute(Pid, Db, Verb, Path, Query, Body, Post, Cookie) ->
    gen_server:call(Pid, {execute, Db, Verb, Path, Query, Body, Post, Cookie}).

% Gen Server Handlers

init([Name, Command]) ->
    ?LOG_INFO("Starting process for: ~s", [Name]),
    {ok, Pid} = couch_os_process:start_link(Command),
    {ok, {Name, Command, Pid}}.

terminate(_Reason, {Name, _Command, Pid}) ->
    ?LOG_INFO("External Process Terminating: ~p: ~p", [Name, Pid]),
    couch_os_process:stop(Pid),
    ok.

handle_call({execute, Db, Verb, Path, Query, Body, Post, Cookie}, _From, {Name, Command, Pid}) ->
    ?LOG_DEBUG("Query Params ~p",[Query]),
    {ok, Info} = couch_db:get_db_info(Db),
    Json = {[
        {<<"info">>, {Info}},
        {<<"verb">>, Verb},
        {<<"path">>, Path},
        {<<"query">>, to_json_terms(Query)},
        {<<"body">>, Body},
        {<<"form">>, to_json_terms(Post)},
        {<<"cookie">>, to_json_terms(Cookie)}]},
    {reply, couch_os_process:prompt(Pid, Json), {Name, Command, Pid}}.

handle_info({'EXIT', Pid, Reason}, {Name, Command, Pid}) ->
    ?LOG_INFO("EXTERNAL: Restarting process for ~s (reason: ~w)", [Name, Reason]),
    {ok, Pid} = couch_os_process:start_link(Command),
    {noreply, {Name, Command, Pid}}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Whatever, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Internal API

to_json_terms(Data) ->
    to_json_terms(Data, []).
to_json_terms([], Acc) ->
    {lists:reverse(Acc)};
to_json_terms([{Key, Value} | Rest], Acc) ->
    to_json_terms(Rest, [{list_to_binary(Key), list_to_binary(Value)} | Acc]).

