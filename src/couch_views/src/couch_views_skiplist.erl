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
% the License.-author("garren").


-module(couch_views_skiplist).


-export([
    rereduce_and_reply/5,
    update_idx/7
]).


-include_lib("couch_mrview/include/couch_mrview.hrl").


% QUERYING SKIPLIST

rereduce_and_reply(_Reducer, [], _GroupLevel, _Callback, Acc) ->
    Acc;

rereduce_and_reply(Reducer, Rows, GroupLevel, Callback, Acc0) ->
    ReReduced = couch_views_reducer:rereduce(Reducer, Rows,
        GroupLevel),
    lists:foldl(fun ({ReducedKey, ReducedVal}, Acc) ->
        {ok, FinalizedVal} = couch_views_reducer:finalize(Reducer, ReducedVal),
        Callback(ReducedKey, FinalizedVal, Acc)
    end, Acc0, ReReduced).


%%% INSERTING INTO SKIPLIST
%%create_indexes(Db, Sig, Views) ->
%%    #{
%%        db_prefix := DbPrefix
%%    } = Db,
%%
%%    fabric2_fdb:transactional(Db, fun(TxDb) ->
%%        lists:foreach(fun (View) ->
%%            #mrview{
%%                id_num = Id,
%%                reduce_funs = ReduceFuns
%%            } = View,
%%
%%            lists:foreach(fun ({_, ReduceFun}) ->
%%                ReduceId = couch_views_util:reduce_id(Id, ReduceFun),
%%                ViewOpts = #{
%%                    db_prefix => DbPrefix,
%%                    sig => Sig,
%%                    view_id => ReduceId,
%%                    reduce_fun => ReduceFun
%%                },
%%                couch_views_reduce_fdb:create_skip_list(TxDb,
%%                    ?MAX_SKIP_LIST_LEVELS, ViewOpts)
%%            end, ReduceFuns)
%%
%%        end, Views)
%%    end).


update_idx(TxDb, Sig, ViewId, Reducer, _DocId, _ExistingKeys, ReduceResult) ->
    #{
        db_prefix := DbPrefix
    } = TxDb,

    ViewOpts = #{
        db_prefix => DbPrefix,
        sig => Sig,
        view_id => ViewId,
        reducer => Reducer
    },
    ReduceIdxPrefix = couch_views_reduce_fdb:idx_prefix(DbPrefix, Sig, ViewId),
    lists:foreach(fun ({Key, Val}) ->
        add_kv_to_skip_list(TxDb, ReduceIdxPrefix, ViewOpts, Key, Val)
    end, ReduceResult).




% The insert algorithm
% Adding a key involves first checking if that key already exists
% and rereduce the two k/v's together and then save.
add_kv_to_skip_list(Db, ReduceIdxPrefix, #{} = ViewOpts, Key, Val) ->
    #{
        reducer := Reducer
    } = ViewOpts,

    fabric2_fdb:transactional(Db, fun(TxDb) ->
        Val1 = case couch_views_reduce_fdb:get_value(TxDb,
            ReduceIdxPrefix, Key) of
                not_found ->
                    Val;
                ExistingVal ->
                    [{_, NewReducedVal}] = couch_views_reducer:rereduce(Reducer,
                        [{Key, ExistingVal}, {Key, Val}], group_true),
                    NewReducedVal
            end,
            couch_views_reduce_fdb:add_kv(TxDb, ReduceIdxPrefix, Key, Val1)
    end).

