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

-module(smoosh_utils).
-include_lib("couch/include/couch_db.hrl").

-export([get/2, get/3, group_pid/1, split/1, stringify/1, ignore_db/1]).
-export([
    in_allowed_window/1
]).

group_pid({Shard, GroupId}) ->
    case couch_view_group:open_db_group(Shard, GroupId) of
    {ok, Group} ->
        try
            gen_server:call(couch_view, {get_group_server, Shard, Group})
        catch _:Error ->
            {error, Error}
        end;
    Else ->
        Else
    end.

get(Channel, Key) ->
    ?MODULE:get(Channel, Key, undefined).

get(Channel, Key, Default) ->
    config:get("smoosh." ++ Channel, Key, Default).

split(CSV) ->
    re:split(CSV, "\\s*,\\s*", [{return,list}, trim]).

stringify({DbName, GroupId}) ->
    io_lib:format("~s ~s", [DbName, GroupId]);
stringify({schema, DbName, GroupId}) ->
    io_lib:format("schema: ~s ~s", [DbName, GroupId]);
stringify(DbName) ->
    io_lib:format("~s", [DbName]).

ignore_db({DbName, _GroupName}) ->
    ignore_db(DbName);
ignore_db(DbName) when is_binary(DbName)->
    ignore_db(?b2l(DbName));
ignore_db(DbName) when is_list(DbName) ->
    case config:get("smoosh.ignore", DbName, false) of
    "true" ->
        true;
    _ ->
        false
    end;
ignore_db(Db) ->
    ignore_db(couch_db:name(Db)).

in_allowed_window(Channel) ->
    From = parse_time(get(Channel, "from"), {00, 00}),
    To = parse_time(get(Channel, "to"), {24, 00}),
    in_allowed_window(From, To).

in_allowed_window(From, To) ->
    {HH, MM, _} = erlang:time(),
    case From < To of
    true ->
        ({HH, MM} >= From) andalso ({HH, MM} < To);
    false ->
        ({HH, MM} >= From) orelse ({HH, MM} < To)
    end.


parse_time(undefined, Default) ->
    Default;
parse_time(String, Default) ->
    case string:tokens(String, ":") of
        [HH, MM] ->
            try
                {list_to_integer(HH), list_to_integer(MM)}
            catch error:badarg ->
                couch_log:error("Malformed compaction schedule configuration: ~s", [String]),
                Default
            end;
        _Else ->
            couch_log:error("Malformed compaction schedule configuration: ~s", [String]),
            Default
    end.
