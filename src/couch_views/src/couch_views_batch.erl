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

-module(couch_views_batch).

-export([
    start/1,
    success/2,
    failure/1
]).

-include_lib("couch_views/include/couch_views.hrl").

-type update_stats() :: #{
    docs_read => non_neg_integer(),
    tx_size => non_neg_integer(),
    total_kvs => non_neg_integer()
}.

-export_type([update_stats/0]).

-callback start(
    Mrst :: #mrst{},
    State :: term()
) -> {NewState :: term(), BatchSize :: pos_integer()}.

-callback success(
    Mrst :: #mrst{},
    UpdateStats :: update_stats(),
    State :: term()
) -> NewState :: term().

-callback failure(Mrst :: #mrst{}, State :: term()) -> NewState :: term().

-define(DEFAULT_MOD, "couch_views_batch_impl").

-spec start(#mrst{}) -> pos_integer().
start(#mrst{} = Mrst) ->
    {Mod, State} =
        case load_state() of
            {M, S} ->
                {M, S};
            undefined ->
                ModStr = config:get("couch_views", "batch_module", ?DEFAULT_MOD),
                ModAtom = list_to_existing_atom(ModStr),
                {ModAtom, undefined}
        end,
    {NewState, BatchSize} = Mod:start(Mrst, State),
    save_state(Mod, NewState),
    BatchSize.

-spec success(#mrst{}, UpdateStats :: update_stats()) -> ok.
success(#mrst{} = Mrst, UpdateStats) ->
    {Mod, State} = load_state(),
    NewState = Mod:success(Mrst, UpdateStats, State),
    save_state(Mod, NewState),
    ok.

-spec failure(#mrst{}) -> ok.
failure(#mrst{} = Mrst) ->
    {Mod, State} = load_state(),
    NewState = Mod:failure(Mrst, State),
    save_state(Mod, NewState),
    ok.

load_state() ->
    get(?MODULE).

save_state(Mod, State) ->
    put(?MODULE, {Mod, State}).
