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

-module(fabric2_txids).
-behaviour(gen_server).
-vsn(1).


-export([
    start_link/0,
    create/2,
    remove/1
]).


-export([
    init/1,
    terminate/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3
]).


-include("fabric2.hrl").


-define(ONE_HOUR, 3600000000).
-define(MAX_TX_IDS, 1000).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


create(Tx, undefined) ->
    Root = erlfdb_directory:root(),
    CouchDB = erlfdb_directory:create_or_open(Tx, Root, [<<"couchdb">>]),
    Prefix = erlfdb_directory:get_name(CouchDB),
    create(Tx, Prefix);

create(_Tx, LayerPrefix) ->
    {Mega, Secs, Micro} = os:timestamp(),
    Key = {?TX_IDS, Mega, Secs, Micro, fabric2_util:uuid()},
    erlfdb_tuple:pack(Key, LayerPrefix).


remove(TxId) when is_binary(TxId) ->
    gen_server:cast(?MODULE, {remove, TxId});

remove(undefined) ->
    ok.



init(_) ->
    {ok, #{
        last_sweep => os:timestamp(),
        txids => []
    }}.


terminate(_, #{txids := TxIds}) ->
    if TxIds == [] -> ok; true ->
        fabric2_fdb:transactional(fun(Tx) ->
            lists:foreach(fun(TxId) ->
                erlfdb:clear(Tx, TxId)
            end)
        end)
    end,
    ok.


handle_call(Msg, _From, St) ->
    {stop, {bad_call, Msg}, {bad_call, Msg}, St}.


handle_cast({remove, TxId}, St) ->
    #{
        last_sweep := LastSweep,
        txids := TxIds
    } = St,

    NewTxIds = [TxId | TxIds],
    NewSt = St#{txids := NewTxIds},

    NeedsSweep = timer:now_diff(os:timestamp(), LastSweep) > ?ONE_HOUR,

    case NeedsSweep orelse length(NewTxIds) >= ?MAX_TX_IDS of
        true ->
            {noreply, clean(NewSt, NeedsSweep)};
        false ->
            {noreply, NewSt}
    end.


handle_info(Msg, St) ->
    {stop, {bad_info, Msg}, St}.


code_change(_OldVsn, St, _Extra) ->
    {ok, St}.


clean(St, NeedsSweep) ->
    #{
        last_sweep := LastSweep,
        txids := TxIds
    } = St,
    fabric2_fdb:transactional(fun(Tx) ->
        lists:foreach(fun(TxId) ->
            erlfdb:clear(Tx, TxId)
        end, TxIds),
        case NeedsSweep of
            true ->
                sweep(Tx, LastSweep),
                St#{
                    last_sweep := os:timestamp(),
                    txids := []
                };
            false ->
                St#{txids := []}
        end
    end).


sweep(Tx, {Mega, Secs, Micro}) ->
    Root = erlfdb_directory:root(),
    CouchDB = erlfdb_directory:create_or_open(Tx, Root, [<<"couchdb">>]),
    Prefix = erlfdb_directory:get_name(CouchDB),
    StartKey = erlfdb_tuple:pack({?TX_IDS}, Prefix),
    EndKey = erlfdb_tuple:pack({?TX_IDS, Mega, Secs, Micro}, Prefix),
    erlfdb:set_option(Tx, next_write_no_write_conflict_range),
    erlfdb:clear_range(Tx, StartKey, EndKey).
