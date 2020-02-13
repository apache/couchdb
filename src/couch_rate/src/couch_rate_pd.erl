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

-module(couch_rate_pd).

-include("couch_rate.hrl").


-export([
    new/2,
    create_if_missing/2,
    lookup/2,
    update/3
]).

-type id() :: term().
-type state() :: term().
-type result() :: term().
-type store_state() :: term().

-define(STATE_KEY, couch_rate_state).


-spec create_if_missing(couch_rate:id(), state()) -> store_state().

create_if_missing(Id, State) ->
    case get({?STATE_KEY, Id}) of
        undefined ->
            put({?STATE_KEY, Id}, State),
            ok;
        _ ->
            ok
    end.


-spec new(couch_rate:id(), state()) ->
        store_state()
        | {error, term()}.

new(Id, State) ->
    case get({?STATE_KEY, Id}) of
        undefined ->
            put({?STATE_KEY, Id}, State),
            ok;
        _ ->
            {error, #{reason => already_exists, id => Id}}
    end.


-spec lookup(id(), store_state()) ->
    state()
    | {error, term()}.

lookup(Id, _StoreState) ->
    case get({?STATE_KEY, Id}) of
        undefined ->
            {error, #{reason => cannot_find, id => Id}};
        State ->
            State
    end.


-spec update(id(), store_state(), fun(
            (id(), state()) -> {state(), result()}
        )) ->
            result()
            | {error, term()}.

update(Id, _StoreState, Fun) ->
    case get({?STATE_KEY, Id}) of
        undefined ->
            {error, #{reason => cannot_find, id => Id}};
        State ->
            case Fun(Id, State) of
                 {Result, State} ->
                      put({?STATE_KEY, Id}, State),
                      Result;
                  Error ->
                      Error
            end
    end.
