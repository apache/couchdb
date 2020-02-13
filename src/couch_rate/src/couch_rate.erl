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

-module(couch_rate).

-include("couch_rate.hrl").

-export([
    create_if_missing/2,
    create_if_missing/3,
    create_if_missing/4,
    new/2,
    new/3,
    new/4,
    from_map/4,
    budget/1,
    delay/1,
    wait/1,
    in/2,
    success/2,
    failure/1,
    is_congestion/1,
    min_latency/1,
    format/1,
    to_map/1,
    id/1,
    module/1,
    state/1,
    store/1
]).

-define(LIMITER, ?MODULE).

-type id() :: term().
-type state() :: term().
-type store() :: module().

-opaque limiter() :: #?LIMITER{}.

-export_type([
    id/0,
    state/0,
    limiter/0
]).

-spec create_if_missing(id(), string()) ->
        couch_rate:limiter() | {error, Reason :: term()}.

create_if_missing(Id, ConfigId) ->
    ?MODULE:create_if_missing(Id, ConfigId, couch_rate_ets).

-spec create_if_missing(id(), string(), nil | module()) ->
        couch_rate:limiter() | {error, Reason :: term()}.

create_if_missing(Id, ConfigId, StateStore) ->
    {Module, Options} = get_config(ConfigId),
    ?MODULE:create_if_missing(Id, Module, StateStore, Options).

-spec create_if_missing(id(), module(), nil | module(), map()) ->
        couch_rate:limiter() | {error, Reason :: term()}.

create_if_missing(Id, Module, nil, Options) ->
    #?LIMITER{
        id = Id,
        module = Module,
        store = nil,
        state = Module:new(Id, Options)
    };

create_if_missing(Id, Module, Store, Options) ->
    case Store:create_if_missing(Id, Module:new(Id, Options)) of
        {error, _} = Error ->
            Error;
        State ->
            #?LIMITER{
                id = Id,
                module = Module,
                store = Store,
                state = State
            }
    end.


-spec new(id(), string()) ->
        couch_rate:limiter() | {error, Reason :: term()}.

new(Id, ConfigId) ->
    ?MODULE:new(Id, ConfigId, couch_rate_ets).

-spec new(id(), string(), module()) ->
        couch_rate:limiter() | {error, Reason :: term()}.

new(Id, ConfigId, StateStore) ->
    {Module, Options} = get_config(ConfigId),
    ?MODULE:new(Id, Module, StateStore, Options).


-spec new(id(), module(), nil | module(), map()) ->
        couch_rate:limiter() | {error, Reason :: term()}.

new(Id, Module, nil, Options) ->
    #?LIMITER{
        id = Id,
        module = Module,
        store = nil,
        state = Module:new(Id, Options)
    };

new(Id, Module, Store, Options) ->
    case Store:new(Id, Module:new(Id, Options)) of
        {error, _} = Error ->
            Error;
        State ->
            #?LIMITER{
                id = Id,
                module = Module,
                store = Store,
                state = State
            }
    end.


-spec from_map(id(), module(), store(), map()) ->
    couch_rate:limiter()
    | {error, Reason :: term()}.

from_map(Id, Module, nil, Map) ->
    #?LIMITER{
        id = Id,
        module = Module,
        store = nil,
        state = Module:from_map(Map)
    };

from_map(Id, Module, Store, Map) ->
    case Store:new(Id, Module:from_map(Map)) of
        {error, _} = Error ->
            Error;
        State ->
            #?LIMITER{
                id = Id,
                module = Module,
                store = Store,
                state = State
            }
    end.


-spec update(limiter(), (
            fun(
                (id(), state()) ->
                    {Result :: term(), state()}
                    | {error, Reason :: term()}
            )
        )) ->
            Result :: term()
            | {Result :: term(), state()}
            | {error, Reason :: term()}.

update(#?LIMITER{store = nil, id = Id, state = State0} = Limiter, Fun) ->
    case Fun(Id, State0) of
        {error, _Reason} = Error ->
            Error;
        {Result, State1} ->
            {Result, Limiter#?LIMITER{state = State1}}
    end;

update(#?LIMITER{id = Id, store = Store, state = State}, Fun) ->
    Store:update(Id, State, Fun).


-spec budget(limiter()) ->
        Budget :: integer()
        | {Budget :: integer(), limiter()}
        | {error, term()}.

budget(#?LIMITER{module = Module} = Limiter) ->
    update(Limiter, fun(Id, StateIn) ->
        Module:budget(Id, StateIn)
    end).


-spec delay(limiter()) ->
        DelayTime :: integer()
        | {DelayTime :: integer(), limiter()}
        | {error, term()}.

delay(#?LIMITER{module = Module} = Limiter) ->
    update(Limiter, fun(Id, State) ->
        Module:delay(Id, State)
    end).


-spec wait(limiter()) ->
      ok
      | {ok, limiter()}
      | {error, term()}.

wait(#?LIMITER{module = Module} = Limiter) ->
    update(Limiter, fun(Id, State) ->
        Module:wait(Id, State)
    end).


-spec in(limiter(), integer()) ->
        ok
        | {ok, limiter()}
        | {error, term()}.

in(#?LIMITER{module = Module} = Limiter, Reads) ->
    update(Limiter, fun(Id, State) ->
        Module:in(Id, State, Reads)
    end).


-spec success(limiter(), integer()) ->
        ok
        | limiter()
        | {error, term()}.

success(#?LIMITER{module = Module} = Limiter, Writes) ->
    update(Limiter, fun(Id, State) ->
        Module:success(Id, State, Writes)
    end).


-spec failure(limiter()) ->
    ok
    | limiter()
    | {error, term()}.

failure(#?LIMITER{module = Module} = Limiter) ->
    update(Limiter, fun(Id, State) ->
        Module:failure(Id, State)
    end).


-spec is_congestion(limiter()) -> boolean().

is_congestion(#?LIMITER{store = nil, module = Module, id = Id, state = State}) ->
    Module:is_congestion(Id, State);

is_congestion(#?LIMITER{store = Store, module = Module, id = Id, state = State}) ->
    Module:is_congestion(Id, Store:lookup(Id, State)).


-spec format(limiter()) -> [{Key :: atom(), Value :: term()}].

format(#?LIMITER{store = nil, module = Module, id = Id, state = State}) ->
    Module:format(Id, State);

format(#?LIMITER{store = Store, module = Module, id = Id, state = State}) ->
    Module:format(Id, Store:lookup(Id, State)).


-spec to_map(limiter()) -> map().

to_map(#?LIMITER{store = nil, module = Module, id = Id, state = State}) ->
    Module:to_map(Id, State);

to_map(#?LIMITER{store = Store, module = Module, id = Id, state = State}) ->
    Module:to_map(Id, Store:lookup(Id, State)).

-spec min_latency(limiter()) -> pos_integer().

min_latency(#?LIMITER{store = nil, module = Module, id = Id, state = State}) ->
    Module:min_latency(Id, State);

min_latency(#?LIMITER{store = Store, module = Module, id = Id, state = State}) ->
    Module:to_map(Id, Store:lookup(Id, State)).


-spec id(limiter()) -> module().

id(Limiter) ->
    Limiter#?LIMITER.id.


-spec module(limiter()) -> module().

module(Limiter) ->
    Limiter#?LIMITER.module.


-spec state(limiter()) -> state().

state(Limiter) ->
    Limiter#?LIMITER.state.

-spec store(limiter()) -> module() | nil.

store(Limiter) ->
    Limiter#?LIMITER.store.


get_config(ConfigId) ->
    ConfigSection = "couch_rate." ++ ConfigId,
    ModuleStr = config:get(ConfigSection, "limiter", "couch_rate_limiter"),
    Module = list_to_existing_atom(ModuleStr),
    case config:get(ConfigSection, "opts", undefined) of
        undefined ->
            {error, #{missing_key => "opts", in => ConfigSection}};
        OptionsStr ->
            Options = couch_rate_config:from_str(OptionsStr),
            lists:map(fun(Key) ->
                 maps:is_key(Key, Options) orelse error(#{missing_key => Key, in => Options})
            end, [budget, target, window, sensitivity]),
            {Module, Options}
    end.
