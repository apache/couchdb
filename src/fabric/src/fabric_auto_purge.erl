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

-module(fabric_auto_purge).

-export([get/1, set/2]).

-export([set_int/2]).

-include_lib("couch/include/couch_db.hrl").
-define(KEY, <<"auto_purge">>).

get(DbName) when is_binary(DbName) ->
    with_db(fun(Db) -> get_cb(Db, DbName) end).

get_cb(Db, DocId) ->
    case couch_db:open_doc(Db, DocId) of
        {error, Reason} ->
            {error, Reason};
        {not_found, _Reason} ->
            {ok, []};
        {ok, Doc} ->
            {Props} = couch_doc:to_json_obj(Doc, []),
            {AutoPurgeProps} = couch_util:get_value(?KEY, Props, {[]}),
            {ok, AutoPurgeProps}
    end.

set(DbName, AutoPurgeProps) when is_binary(DbName) ->
    Node = hd(mem3_util:live_nodes()),
    case rpc:call(Node, ?MODULE, set_int, [DbName, AutoPurgeProps]) of
        {badrpc, Reason} ->
            {error, Reason};
        Result ->
            Result
    end.

set_int(DbName, AutoPurgeProps) ->
    with_db(fun(Db) -> set_cb(Db, DbName, AutoPurgeProps) end).

set_cb(Db, DocId, AutoPurgeProps) ->
    case couch_db:open_doc(Db, DocId) of
        {error, Reason} ->
            {error, Reason};
        {not_found, Reason} ->
            {not_found, Reason};
        {ok, #doc{} = Doc0} ->
            {Props0} = couch_doc:to_json_obj(Doc0, []),
            Props1 = lists:keystore(?KEY, 1, Props0, {?KEY, {AutoPurgeProps}}),
            Doc1 = couch_doc:from_json_obj({Props1}),
            case couch_db:update_doc(Db, Doc1, []) of
                {ok, _NewRev} ->
                    ok;
                {error, Reason} ->
                    {error, Reason}
            end
    end.

with_db(Fun) ->
    case couch_db:open_int(dbs_db(), [?ADMIN_CTX]) of
        {ok, Db} ->
            try
                Fun(Db)
            after
                catch couch_db:close(Db)
            end;
        Else ->
            Else
    end.

dbs_db() ->
    ?l2b(config:get("mem3", "shards_db", "_dbs")).
