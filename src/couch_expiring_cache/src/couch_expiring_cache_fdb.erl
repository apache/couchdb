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

-module(couch_expiring_cache_fdb).

-export([
    insert/6,
    lookup/3,
    clear_all/1,
    clear_expired_range/3
]).


-define(EXPIRING_CACHE, 53). % coordinate with fabric2.hrl
-define(PK, 1).
-define(EXP, 2).


-include_lib("couch_expiring_cache/include/couch_expiring_cache.hrl").


% Data model
% see: https://forums.foundationdb.org/t/designing-key-value-expiration-in-fdb/156
%
% (?EXPIRING_CACHE, Name, ?PK, Key) := (Val, StaleTS, ExpiresTS)
% (?EXPIRING_CACHE, Name, ?EXP, ExpiresTS, Key) := ()


-spec insert(JTx :: jtx(), Name :: binary(), Key :: binary(), Value :: binary(),
    StaleTS :: ?TIME_UNIT, ExpiresTS :: ?TIME_UNIT) -> ok.
insert(#{jtx := true} = JTx, Name, Key, Val, StaleTS, ExpiresTS) ->
    #{tx := Tx, layer_prefix := LayerPrefix} = couch_jobs_fdb:get_jtx(JTx),
    PK = primary_key(Name, Key, LayerPrefix),
    PV = erlfdb_tuple:pack({Val, StaleTS, ExpiresTS}),
    XK = expiry_key(ExpiresTS, Name, Key, LayerPrefix),
    XV = erlfdb_tuple:pack({}),
    ok = erlfdb:set(Tx, PK, PV),
    ok = erlfdb:set(Tx, XK, XV).


-spec lookup(JTx :: jtx(), Name :: binary(), Key :: binary()) ->
    not_found | {fresh, Val :: binary()} | {stale, Val :: binary()} | expired.
lookup(#{jtx := true} = JTx, Name, Key) ->
    #{tx := Tx, layer_prefix := LayerPrefix} = couch_jobs_fdb:get_jtx(JTx),
    PK = primary_key(Name, Key, LayerPrefix),
    case erlfdb:wait(erlfdb:get(Tx, PK)) of
        not_found ->
            not_found;
        Bin when is_binary(Bin) ->
            {Val, StaleTS, ExpiresTS} = erlfdb_tuple:unpack(Bin),
            Now = erlang:system_time(?TIME_UNIT),
            if
                Now < StaleTS -> {fresh, Val};
                Now < ExpiresTS -> {stale, Val};
                true -> expired
            end
    end.


-spec clear_all(Name :: binary()) ->
    ok.
clear_all(Name) ->
    fabric2_fdb:transactional(fun(Tx) ->
        LayerPrefix = fabric2_fdb:get_dir(Tx),
        NamePrefix = erlfdb_tuple:pack({?EXPIRING_CACHE, Name}, LayerPrefix),
        erlfdb:clear_range_startswith(Tx, NamePrefix)
    end).


-spec clear_expired_range(Name :: binary(), EndTS :: ?TIME_UNIT,
    Limit :: non_neg_integer()) ->
        OldestTS :: ?TIME_UNIT.
clear_expired_range(Name, EndTS, Limit) when Limit > 0 ->
    fabric2_fdb:transactional(fun(Tx) ->
        LayerPrefix = fabric2_fdb:get_dir(Tx),
        ExpiresPrefix = erlfdb_tuple:pack(
            {?EXPIRING_CACHE, Name, ?EXP}, LayerPrefix),
        fabric2_fdb:fold_range({tx, Tx}, ExpiresPrefix, fun({K, _V}, Acc) ->
            Unpacked = erlfdb_tuple:unpack(K, ExpiresPrefix),
            couch_log:debug("~p clearing ~p", [?MODULE, Unpacked]),
            {ExpiresTS, Key} = Unpacked,
            clear_expired(Tx, ExpiresTS, Name, Key, LayerPrefix),
            oldest_ts(ExpiresTS, Acc)
        end, 0, [{end_key, EndTS}, {limit, Limit}])
    end).


%% Private


clear_expired(Tx, ExpiresTS, Name, Key, Prefix) ->
    PK = primary_key(Name, Key, Prefix),
    XK = expiry_key(ExpiresTS, Name, Key, Prefix),
    ok = erlfdb:clear(Tx, PK),
    ok = erlfdb:clear(Tx, XK).


oldest_ts(TS, 0) -> TS; % handle initial Acc = 0 case
oldest_ts(TS, OldestTS) -> min(TS, OldestTS).


primary_key(Name, Key, Prefix) ->
    erlfdb_tuple:pack({?EXPIRING_CACHE, Name, ?PK, Key}, Prefix).


expiry_key(ExpiresTS, Name, Key, Prefix) ->
    erlfdb_tuple:pack({?EXPIRING_CACHE, Name, ?EXP, ExpiresTS, Key}, Prefix).
