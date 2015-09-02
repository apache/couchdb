% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_epi_module_keeper).

-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([maybe_start_keeper/2]).
-export([register_service/2]).

-export([start_link/2, save/3]).
-export([stop/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {codegen, module}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

register_service(Codegen, Module) ->
    {ok, Server} = maybe_start_keeper(Codegen, Module),
    compile_dummy_module(Server).

maybe_start_keeper(Codegen, Module) ->
    case couch_epi_keeper_sup:start_child(Codegen, Module) of
        {ok, Pid} ->
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {ok, Pid}
    end.

start_link(Codegen, Module) ->
    gen_server:start_link({local, Module}, ?MODULE, [Codegen, Module], []).

stop(Server) ->
    catch gen_server:call(Server, stop).

save(Server, Source, Config) ->
    gen_server:call(Server, {save, Source, Config}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([Codegen, Module]) ->
    {ok, #state{codegen = Codegen, module = Module}}.

handle_call({save, Source, Config}, _From, State) ->
    #state{codegen = Codegen, module = Module} = State,
    Reply = Codegen:save(Module, Source, Config),
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

compile_dummy_module(Server) ->
    save(Server, undefined, []).
