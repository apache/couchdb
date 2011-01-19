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

-module(couch_external_manager).
-behaviour(gen_server).

-export([start_link/0, execute/2, config_change/2]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("couch_db.hrl").

start_link() ->
    gen_server:start_link({local, couch_external_manager},
        couch_external_manager, [], []).

execute(UrlName, JsonReq) ->
    Pid = gen_server:call(couch_external_manager, {get, UrlName}),
    case Pid of
    {error, Reason} ->
        Reason;
    _ ->
        couch_external_server:execute(Pid, JsonReq)
    end.

config_change("external", UrlName) ->
    gen_server:call(couch_external_manager, {config, UrlName}).

% gen_server API

init([]) ->
    process_flag(trap_exit, true),
    Handlers = ets:new(couch_external_manager_handlers, [set, private]),
    couch_config:register(fun ?MODULE:config_change/2),
    {ok, Handlers}.

terminate(_Reason, Handlers) ->
    ets:foldl(fun({_UrlName, Pid}, nil) ->
        couch_external_server:stop(Pid),
        nil
    end, nil, Handlers),
    ok.

handle_call({get, UrlName}, _From, Handlers) ->
    case ets:lookup(Handlers, UrlName) of
    [] ->
        case couch_config:get("external", UrlName, nil) of
        nil ->
            Msg = lists:flatten(
                io_lib:format("No server configured for ~p.", [UrlName])),
            {reply, {error, {unknown_external_server, ?l2b(Msg)}}, Handlers};
        Command ->
            {ok, NewPid} = couch_external_server:start_link(UrlName, Command),
            true = ets:insert(Handlers, {UrlName, NewPid}),
            {reply, NewPid, Handlers}
        end;
    [{UrlName, Pid}] ->
        {reply, Pid, Handlers}
    end;
handle_call({config, UrlName}, _From, Handlers) ->
    % A newly added handler and a handler that had it's command
    % changed are treated exactly the same.

    % Shutdown the old handler.
    case ets:lookup(Handlers, UrlName) of
    [{UrlName, Pid}] ->
        couch_external_server:stop(Pid);
    [] ->
        ok
    end,
    % Wait for next request to boot the handler.
    {reply, ok, Handlers}.

handle_cast(_Whatever, State) ->
    {noreply, State}.

handle_info({'EXIT', Pid, normal}, Handlers) ->
    ?LOG_INFO("EXTERNAL: Server ~p terminated normally", [Pid]),
    % The process terminated normally without us asking - Remove Pid from the
    % handlers table so we don't attempt to reuse it
    ets:match_delete(Handlers, {'_', Pid}),
    {noreply, Handlers};

handle_info({'EXIT', Pid, Reason}, Handlers) ->
    ?LOG_INFO("EXTERNAL: Server ~p died. (reason: ~p)", [Pid, Reason]),
    % Remove Pid from the handlers table so we don't try closing
    % it a second time in terminate/2.
    ets:match_delete(Handlers, {'_', Pid}),
    {stop, normal, Handlers}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

