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

-module(mem3_db_doc_updater).

-behaviour(gen_server).

-export([
    get_db_doc/1,
    update_db_doc/1,

    start_link/0,

    init/1,
    handle_call/3,
    handle_cast/2
]).

-include_lib("couch/include/couch_db.hrl").

% Early return shortcut
%
-define(THROW(RES), throw({reply, RES, nil})).

get_db_doc(DocId) when is_binary(DocId) ->
    Timeout = shard_update_timeout_msec(),
    gen_server:call(first_node(), {get_db_doc, DocId}, Timeout).

update_db_doc(#doc{} = Doc) ->
    Timeout = shard_update_timeout_msec(),
    gen_server:call(first_node(), {update_db_doc, Doc}, Timeout).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    {ok, nil}.

handle_call({get_db_doc, DocId}, _From, nil = St) ->
    {reply, get_db_doc_int(DocId), St};
handle_call({update_db_doc, #doc{} = Doc}, _From, nil = St) ->
    {reply, update_db_doc_int(Doc), St};
handle_call(Msg, _From, nil = St) ->
    {stop, {invalid_call, Msg}, invalid_call, St}.

handle_cast(Msg, nil = St) ->
    {stop, {invalid_cast, Msg}, St}.

% Private

update_db_doc_int(#doc{} = Doc) ->
    ok = validate_coordinator(),
    couch_util:with_db(mem3_sync:shards_db(), fun(Db) ->
        try
            Res = couch_db:update_doc(Db, Doc, [?ADMIN_CTX]),
            ok = replicate_to_all_nodes(shard_update_timeout_msec()),
            Res
        catch
            conflict ->
                ?THROW({error, conflict})
        end
    end).

get_db_doc_int(DocId) ->
    ok = validate_coordinator(),
    ok = replicate_from_all_nodes(shard_update_timeout_msec()),
    couch_util:with_db(mem3_sync:shards_db(), fun(Db) ->
        case couch_db:open_doc(Db, DocId, [ejson_body]) of
            {ok, #doc{deleted = true}} -> ?THROW({error, not_found});
            {ok, #doc{} = Doc} -> {ok, Doc};
            {not_found, _} -> ?THROW({error, not_found})
        end
    end).

validate_coordinator() ->
    case hd(mem3_util:live_nodes()) =:= node() of
        true -> ok;
        false -> ?THROW({error, coordinator_changed})
    end.

replicate_from_all_nodes(TimeoutMSec) ->
    case mem3_util:replicate_dbs_from_all_nodes(TimeoutMSec) of
        ok -> ok;
        Error -> ?THROW({error, Error})
    end.

replicate_to_all_nodes(TimeoutMSec) ->
    case mem3_util:replicate_dbs_to_all_nodes(TimeoutMSec) of
        ok -> ok;
        Error -> ?THROW({error, Error})
    end.

shard_update_timeout_msec() ->
    config:get_integer("mem3", "shard_update_timeout_msec", 300000).

first_node() ->
    FirstNode = hd(mem3_util:live_nodes()),
    {?MODULE, FirstNode}.
