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

-module(smoosh).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").

-export([suspend/0, resume/0, enqueue/1, status/0]).
-export([enqueue_all_dbs/0, enqueue_all_dbs/1, enqueue_all_views/0]).

suspend() ->
    smoosh_server:suspend().

resume() ->
    smoosh_server:resume().

enqueue(Object) ->
    smoosh_server:enqueue(Object).

sync_enqueue(Object) ->
    smoosh_server:sync_enqueue(Object).

sync_enqueue(Object, Timeout) ->
    smoosh_server:sync_enqueue(Object, Timeout).

status() ->
    smoosh_server:status().

enqueue_all_dbs() ->
    fold_local_shards(
        fun(#shard{name = Name}, _Acc) ->
            sync_enqueue(Name)
        end,
        ok
    ).

enqueue_all_dbs(Timeout) ->
    fold_local_shards(
        fun(#shard{name = Name}, _Acc) ->
            sync_enqueue(Name, Timeout)
        end,
        ok
    ).

enqueue_all_views() ->
    fold_local_shards(
        fun(#shard{name = Name}, _Acc) ->
            catch enqueue_views(Name)
        end,
        ok
    ).

fold_local_shards(Fun, Acc0) ->
    mem3:fold_shards(
        fun(Shard, Acc1) ->
            case node() == Shard#shard.node of
                true ->
                    Fun(Shard, Acc1);
                false ->
                    Acc1
            end
        end,
        Acc0
    ).

enqueue_views(ShardName) ->
    DbName = mem3:dbname(ShardName),
    {ok, DDocs} = fabric:design_docs(DbName),
    [sync_enqueue({ShardName, id(DDoc)}) || DDoc <- DDocs].

id(#doc{id = Id}) ->
    Id;
id({Props}) ->
    couch_util:get_value(<<"_id">>, Props).
