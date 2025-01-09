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

-module(csrt_server).

-behaviour(gen_server).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2
]).

-export([
    create_pid_ref/0,
    create_resource/1,
    destroy_resource/1,
    get_resource/1,
    get_context_type/1,
    new_context/2,
    set_context_dbname/2,
    set_context_username/2,
    set_context_type/2
]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_stats_resource_tracker.hrl").

-record(st, {}).

%%
%% Public API
%%

create_pid_ref() ->
    {self(), make_ref()}.

%%
%%
%% Context lifecycle API
%%

new_context(Type, Nonce) ->
    #rctx{
       nonce = Nonce,
       pid_ref = create_pid_ref(),
       type = Type
    }.

set_context_dbname(_, undefined) ->
    ok;
set_context_dbname(DbName, PidRef) ->
    update_element(PidRef, [{#rctx.dbname, DbName}]).

%%set_context_handler_fun(_, undefined) ->
%%    ok;
%%set_context_handler_fun(Fun, PidRef) when is_function(Fun) ->
%%    FProps = erlang:fun_info(Fun),
%%    Mod = proplists:get_value(module, FProps),
%%    Func = proplists:get_value(name, FProps),
%%    #rctx{type=#coordinator{}=Coordinator} = get_resource(PidRef),
%%    Update = [{#rctx.type, Coordinator#coordinator{mod=Mod, func=Func}}],
%%    update_element(PidRef, Update).

set_context_username(_, undefined) ->
    ok;
set_context_username(UserName, PidRef) ->
    update_element(PidRef, [{#rctx.username, UserName}]).

get_context_type(#rctx{type=Type}) ->
    Type.

set_context_type(Type, PidRef) ->
    update_element(PidRef, [{#rctx.type, Type}]).

create_resource(#rctx{} = Rctx) ->
    catch ets:insert(?MODULE, Rctx).

destroy_resource(undefined) ->
    ok;
destroy_resource(#rctx{pid_ref=PidRef}) ->
    destroy_resource(PidRef);
destroy_resource({_,_}=PidRef) ->
    catch ets:delete(?MODULE, PidRef).

get_resource(undefined) ->
    undefined;
get_resource(PidRef) ->
    catch case ets:lookup(?MODULE, PidRef) of
        [#rctx{}=Rctx] ->
            Rctx;
        [] ->
            undefined
    end.

%%
%% gen_server callbacks
%%

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    ets:new(?MODULE, [
        named_table,
        public,
        {decentralized_counters, true},
        {write_concurrency, true},
        {read_concurrency, true},
        {keypos, #rctx.pid_ref}
    ]),
    {ok, #st{}}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State, 0}.

%%
%% private functions
%%

update_element(undefined, _Update) ->
    ok;
update_element({_Pid,_Ref}=PidRef, Update) ->
    %% TODO: should we take any action when the update fails?
    catch ets:update_element(?MODULE, PidRef, Update).

