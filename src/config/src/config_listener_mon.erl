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

-module(config_listener_mon).
-behaviour(gen_server).

-export([
    subscribe/2,
    start_link/2
]).

-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2
]).

-record(st, {
    pid,
    ref
}).

start_link(Module, InitSt) ->
    proc_lib:start_link(?MODULE, init, [{self(), Module, InitSt}]).

subscribe(Module, InitSt) ->
    case proc_lib:start(?MODULE, init, [{self(), Module, InitSt}]) of
        {ok, _} -> ok;
        Else -> Else
    end.

init({Pid, Mod, InitSt}) ->
    Ref = erlang:monitor(process, Pid),
    case config_listener:start(Mod, {Mod, Pid}, {Pid, InitSt}) of
        ok ->
            proc_lib:init_ack({ok, self()}),
            gen_server:enter_loop(?MODULE, [], #st{pid = Pid, ref = Ref});
        Else ->
            proc_lib:init_ack(Else)
    end.

handle_call(_Message, _From, St) ->
    {reply, ignored, St}.

handle_cast(_Message, St) ->
    {noreply, St}.

handle_info({'DOWN', Ref, _, _, _}, #st{ref = Ref} = St) ->
    {stop, normal, St};
handle_info({gen_event_EXIT, {config_listener, Module}, Reason}, St) ->
    Level =
        case Reason of
            normal -> debug;
            shutdown -> debug;
            _ -> error
        end,
    Fmt = "config_listener(~p) for ~p stopped with reason: ~r~n",
    couch_log:Level(Fmt, [Module, St#st.pid, Reason]),
    {stop, shutdown, St};
handle_info(_, St) ->
    {noreply, St}.
