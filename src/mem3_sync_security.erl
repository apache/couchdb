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

-export([go/0, go/1]).

go() ->
    {ok, Dbs} = fabric:all_dbs(),
    lists:foreach(fun handle_db/1, Dbs).

go(DbName) when is_binary(DbName) ->
    handle_db(DbName).

handle_db(DbName) ->
    case get_all_security(DbName) of
    {ok, SecObjs} ->
        case is_ok(SecObjs) of
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

is_ok([_]) ->
    ok;
is_ok([_, _] = SecObjs0) ->
    case lists:keytake({[]}, 1, SecObjs0) of
    {value, {_, EmptyCount}, [{SecObj, Count}]} when Count > EmptyCount ->
        {fixable, SecObj};
    _ ->
        broken
    end;
is_ok(_) ->
    broken.

