% Copyright 2010 Cloudant
%
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

-module(mem3_sync_security).

-export([maybe_sync/2, maybe_sync_int/2]).
-export([go/0, go/1]).

-include("mem3.hrl").


maybe_sync(#shard{}=Src, #shard{}=Dst) ->
    case is_local(Src#shard.name) of
        false ->
            erlang:spawn(?MODULE, maybe_sync_int, [Src, Dst]);
        true ->
            ok
    end.

maybe_sync_int(#shard{name=Name}=Src, Dst) ->
    DbName = mem3:dbname(Name),
    case fabric:get_all_security(DbName, [{shards, [Src, Dst]}]) of
        {ok, WorkerObjs} ->
            Objs = [Obj || {_Worker, Obj} <- WorkerObjs],
            case length(lists:usort(Objs)) of
                1 -> ok;
                2 -> go(DbName)
            end;
        Else ->
            Args = [DbName, Else],
            twig:log(err, "Error checking security objects for ~s :: ~p", Args)
    end.

go() ->
    {ok, Dbs} = fabric:all_dbs(),
    lists:foreach(fun handle_db/1, Dbs).

go(DbName) when is_binary(DbName) ->
    handle_db(DbName).

handle_db(DbName) ->
    ShardCount = length(mem3:shards(DbName)),
    case get_all_security(DbName) of
    {ok, SecObjs} ->
        case is_ok(SecObjs, ShardCount) of
        ok ->
            ok;
        {fixable, SecObj} ->
            twig:log(info, "Sync security object for ~p: ~p", [DbName, SecObj]),
            case fabric:set_security(DbName, SecObj) of
                ok -> ok;
                Error ->
                    twig:log(err, "Error setting security object in ~p: ~p",
                        [DbName, Error])
            end;
        broken ->
            twig:log(err, "Bad security object in ~p: ~p", [DbName, SecObjs])
        end;
    Error ->
        twig:log(err, "Error getting security objects for ~p: ~p", [
                DbName, Error])
    end.

get_all_security(DbName) ->
    case fabric:get_all_security(DbName) of
    {ok, SecObjs} ->
        SecObjsDict = lists:foldl(fun({_, SO}, Acc) ->
            dict:update_counter(SO, 1, Acc)
        end, dict:new(), SecObjs),
        {ok, dict:to_list(SecObjsDict)};
    Error ->
        Error
    end.

is_ok([_], _) ->
    % One security object is the happy case
    ok;
is_ok([_, _] = SecObjs0, ShardCount) ->
    % Figure out if we have a simple majority of security objects
    % and if so, use that as the correct value. Otherwise we abort
    % and rely on human intervention.
    {Count, SecObj} =  lists:max([{C, O} || {O, C} <- SecObjs0]),
    case Count >= ((ShardCount div 2) + 1) of
        true -> {fixable, SecObj};
        false -> broken
    end;
is_ok(_, _) ->
    % Anything else requires human intervention
    broken.


is_local(<<"shards/", _/binary>>) ->
    false;
is_local(_) ->
    true.

