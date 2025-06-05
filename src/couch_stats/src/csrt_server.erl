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
    match_resource/1,
    new_context/2,
    set_context_dbname/2,
    set_context_handler_fun/2,
    set_context_type/2,
    set_context_username/2,
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

-spec set_context_handler_fun({Mod, Func}, PidRef) -> boolean() when
    Mod :: atom(), Func :: atom(), PidRef :: maybe_pid_ref().
set_context_handler_fun(_, undefined) ->
    false;
set_context_handler_fun({Mod, Func}, PidRef) ->
    case get_resource(PidRef) of
        undefined ->
            false;
        #rctx{} = Rctx ->
            %% TODO: #coordinator{} assumption needs to adapt for other types
            case csrt_server:get_context_type(Rctx) of
                #coordinator{} = Coordinator0 ->
                    Coordinator = Coordinator0#coordinator{mod = Mod, func = Func},
                    set_context_type(Coordinator, PidRef);
                _ ->
                    false
            end
    end.

-spec set_context_username(UserName, PidRef) -> boolean() when
    UserName :: username(), PidRef :: maybe_pid_ref().
set_context_username(_, undefined) ->
    false;
set_context_username(UserName, PidRef) ->
    update_element(PidRef, [{#rctx.username, UserName}]).

-spec get_context_type(Rctx :: rctx()) -> rctx_type().
get_context_type(#rctx{type = Type}) ->
    Type.

-spec set_context_type(Type, PidRef) -> boolean() when
    Type :: rctx_type(), PidRef :: maybe_pid_ref().
set_context_type(Type, PidRef) ->
    update_element(PidRef, [{#rctx.type, Type}]).

-spec create_resource(Rctx :: rctx()) -> boolean().
create_resource(#rctx{} = Rctx) ->
    (catch ets:insert(?CSRT_ETS, Rctx)) == true.

-spec destroy_resource(PidRef :: maybe_pid_ref()) -> boolean().
destroy_resource(undefined) ->
    false;
destroy_resource({_, _} = PidRef) ->
    (catch ets:delete(?CSRT_ETS, PidRef)) == true.

-spec get_resource(PidRef :: maybe_pid_ref()) -> maybe_rctx().
get_resource(undefined) ->
    undefined;
get_resource(PidRef) ->
    try ets:lookup(?CSRT_ETS, PidRef) of
        [#rctx{} = Rctx] ->
            Rctx;
        [] ->
            undefined
    catch
        _:_ ->
            undefined
    end.

-spec match_resource(Rctx :: maybe_rctx()) -> [] | [rctx()].
match_resource(undefined) ->
    [];
match_resource(#rctx{} = Rctx) ->
    try
        ets:match_object(?CSRT_ETS, Rctx)
    catch
        _:_ ->
            []
    end.

%% Is this a valid #rctx{} field for inducing ets:update_counter upon?
-spec is_rctx_stat_field(Field :: rctx_field() | atom()) -> boolean().
is_rctx_stat_field(Field) ->
    maps:is_key(Field, ?STAT_KEYS_TO_FIELDS).

%% Get the #rctx{} field record index of the corresponding stat counter field
-spec get_rctx_stat_field(Field :: rctx_field()) ->
    non_neg_integer()
    | throw({badkey, Key :: any()}).
get_rctx_stat_field(Field) ->
    maps:get(Field, ?STAT_KEYS_TO_FIELDS).

-spec update_counter(PidRef, Field, Count) -> non_neg_integer() when
    PidRef :: maybe_pid_ref(),
    Field :: rctx_field(),
    Count :: non_neg_integer().
update_counter(undefined, _Field, _Count) ->
    0;
update_counter({_Pid, _Ref} = PidRef, Field, Count) when Count >= 0 ->
    case is_rctx_stat_field(Field) of
        true ->
            Update = {get_rctx_stat_field(Field), Count},
            try
                ets:update_counter(?CSRT_ETS, PidRef, Update, #rctx{pid_ref = PidRef})
            catch
                _:_ ->
                    0
            end;
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
inc({_Pid, _Ref} = PidRef, Field, N) when is_integer(N) andalso N > 0 ->
    case is_rctx_stat_field(Field) of
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
    ets:new(?CSRT_ETS, [
        named_table,
        public,
        {write_concurrency, auto},
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
update_element({_Pid, _Ref} = PidRef, Update) ->
    (catch ets:update_element(?CSRT_ETS, PidRef, Update)) == true.
