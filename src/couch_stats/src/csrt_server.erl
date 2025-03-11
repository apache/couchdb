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
    inc/2,
    inc/3,
    new_context/2,
    set_context_dbname/2,
    set_context_username/2,
    set_context_type/2,
    update_counter/3
]).

-include_lib("stdlib/include/ms_transform.hrl").
-include_lib("couch_stats_resource_tracker.hrl").


-record(st, {}).

%%
%% Public API
%%

-spec create_pid_ref() -> pid_ref().
create_pid_ref() ->
    {self(), make_ref()}.

%%
%%
%% Context lifecycle API
%%

-spec new_context(Type :: rctx_type(), Nonce :: nonce()) -> rctx().
new_context(Type, Nonce) ->
    #rctx{
       nonce = Nonce,
       pid_ref = create_pid_ref(),
       type = Type
    }.

-spec set_context_dbname(DbName, PidRef) -> boolean() when
    DbName :: dbname(), PidRef :: maybe_pid_ref().
set_context_dbname(_, undefined) ->
    false;
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

-spec set_context_username(UserName, PidRef) -> boolean() when
    UserName :: username(), PidRef :: maybe_pid_ref().
set_context_username(_, undefined) ->
    ok;
set_context_username(UserName, PidRef) ->
    update_element(PidRef, [{#rctx.username, UserName}]).

-spec get_context_type(Rctx :: rctx()) -> rctx_type().
get_context_type(#rctx{type=Type}) ->
    Type.

-spec set_context_type(Type, PidRef) -> boolean() when
    Type :: rctx_type(), PidRef :: maybe_pid_ref().
set_context_type(Type, PidRef) ->
    update_element(PidRef, [{#rctx.type, Type}]).

-spec create_resource(Rctx :: rctx()) -> true.
create_resource(#rctx{} = Rctx) ->
    catch ets:insert(?MODULE, Rctx).

-spec destroy_resource(PidRef :: maybe_pid_ref()) -> boolean().
destroy_resource(undefined) ->
    false;
destroy_resource({_,_}=PidRef) ->
    catch ets:delete(?MODULE, PidRef).

-spec get_resource(PidRef :: maybe_pid_ref()) -> maybe_rctx().
get_resource(undefined) ->
    undefined;
get_resource(PidRef) ->
    catch case ets:lookup(?MODULE, PidRef) of
        [#rctx{}=Rctx] ->
            Rctx;
        [] ->
            undefined
    end.

-spec is_rctx_field(Field :: rctx_field() | atom()) -> boolean().
is_rctx_field(Field) ->
    maps:is_key(Field, ?KEYS_TO_FIELDS).

-spec get_rctx_field(Field :: rctx_field()) -> non_neg_integer().
get_rctx_field(Field) ->
    maps:get(Field, ?KEYS_TO_FIELDS).

-spec update_counter(PidRef, Field, Count) -> non_neg_integer() when
        PidRef :: maybe_pid_ref(),
        Field :: rctx_field(),
        Count :: non_neg_integer().
update_counter(undefined, _Field, _Count) ->
    0;
update_counter({_Pid,_Ref}=PidRef, Field, Count) when Count >= 0 ->
    %% TODO: mem3 crashes without catch, why do we lose the stats table?
    case is_rctx_field(Field) of
        true ->
            Update = {get_rctx_field(Field), Count},
            catch ets:update_counter(?MODULE, PidRef, Update, #rctx{pid_ref=PidRef});
        false ->
            0
    end.

-spec inc(PidRef :: maybe_pid_ref(), Field :: rctx_field()) -> non_neg_integer().
inc(PidRef, Field) ->
    inc(PidRef, Field, 1).

-spec inc(PidRef, Field, N) -> non_neg_integer() when
        PidRef :: maybe_pid_ref(),
        Field :: rctx_field(),
        N :: non_neg_integer().
inc(undefined, _Field, _) ->
    0;
inc(_PidRef, _Field, 0) ->
    0;
inc({_Pid,_Ref}=PidRef, Field, N) when is_integer(N) andalso N >= 0 ->
    case is_rctx_field(Field) of
        true ->
            update_counter(PidRef, Field, N);
        false ->
            0
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

-spec update_element(PidRef :: maybe_pid_ref(), Updates :: [tuple()]) -> boolean().
update_element(undefined, _Update) ->
    false;
update_element({_Pid,_Ref}=PidRef, Update) ->
    %% TODO: should we take any action when the update fails?
    catch ets:update_element(?MODULE, PidRef, Update).

