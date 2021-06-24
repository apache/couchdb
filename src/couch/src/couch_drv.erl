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

-module(couch_drv).
-behaviour(gen_server).
-vsn(1).
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-export([start_link/0]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("kernel/include/logger.hrl").

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    LibDir = util_driver_dir(),
    case erl_ddll:load(LibDir, "couch_icu_driver") of
        ok ->
            {ok, nil};
        {error, already_loaded} ->
            ?LOG_INFO(#{what => reload_couch_icu_driver}),
            couch_log:info("~p reloading couch_icu_driver", [?MODULE]),
            ok = erl_ddll:reload(LibDir, "couch_icu_driver"),
            {ok, nil};
        {error, Error} ->
            {stop, erl_ddll:format_error(Error)}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% private API
util_driver_dir() ->
    case config:get("couchdb", "util_driver_dir", undefined) of
        undefined ->
            couch_util:priv_dir();
        LibDir0 ->
            LibDir0
    end.
