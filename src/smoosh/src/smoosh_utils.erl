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

-export([get/2, get/3, split/1, stringify/1, ignore_db/1]).
-export([in_allowed_window/1, write_to_file/3]).
-export([log_level/2]).

get(Channel, Key) ->
    ?MODULE:get(Channel, Key, undefined).

get(Channel, Key, Default) ->
    config:get("smoosh." ++ Channel, Key, Default).

split(CSV) ->
    re:split(CSV, "\\s*,\\s*", [{return, list}, trim]).

stringify({DbName, GroupId}) ->
    io_lib:format("~s ~s", [DbName, GroupId]);
stringify(DbName) ->
    io_lib:format("~s", [DbName]).

ignore_db({DbName, _GroupName}) ->
    ignore_db(DbName);
ignore_db(DbName) when is_binary(DbName) ->
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
    {_, {HH, MM, _}} = calendar:universal_time(),
    case From < To of
        true ->
            ({HH, MM} >= From) andalso ({HH, MM} < To);
        false ->
            ({HH, MM} >= From) orelse ({HH, MM} < To)
    end.

file_delete(Path) ->
    case file:delete(Path) of
        % When deleting a state file, we do not care if it does not exist.
        Ret when Ret =:= ok; Ret =:= {error, enoent} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

write_to_file(Content, FileName, VSN) ->
    Level = log_level("compaction_log_level", "debug"),
    couch_log:Level("~p Writing state ~s", [?MODULE, FileName]),
    OnDisk = <<VSN, (erlang:term_to_binary(Content, [compressed, {minor_version, 1}]))/binary>>,
    TmpFileName = FileName ++ ".tmp",
    case file_delete(TmpFileName) of
        ok ->
            case file:write_file(TmpFileName, OnDisk, [sync]) of
                ok ->
                    case file_delete(FileName) of
                        ok ->
                            case file:rename(TmpFileName, FileName) of
                                ok ->
                                    ok;
                                {error, Reason} ->
                                    couch_log:error(
                                        "~p (~p) Error renaming temporary state file ~s to ~s", [
                                            ?MODULE, Reason, TmpFileName, FileName
                                        ]
                                    ),
                                    ok
                            end;
                        {error, Reason} ->
                            couch_log:error("~p (~p) Error deleting state file ~s", [
                                ?MODULE, Reason, FileName
                            ]),
                            ok
                    end;
                {error, Reason} ->
                    couch_log:error("~p (~p) Error writing state to temporary file ~s", [
                        ?MODULE, Reason, TmpFileName
                    ]),
                    ok
            end;
        {error, Reason} ->
            % Failing to persist the queue should not crash smoosh. Return ok.
            couch_log:error("~p (~p) Error deleting temporary state file ~s", [
                ?MODULE, Reason, TmpFileName
            ]),
            ok
    end.

parse_time(undefined, Default) ->
    Default;
parse_time(String, Default) ->
    case string:tokens(String, ":") of
        [HH, MM] ->
            try
                {list_to_integer(HH), list_to_integer(MM)}
            catch
                error:badarg ->
                    couch_log:error("Malformed compaction schedule configuration: ~s", [String]),
                    Default
            end;
        _Else ->
            couch_log:error("Malformed compaction schedule configuration: ~s", [String]),
            Default
    end.

log_level(Key, Default) when is_list(Key), is_list(Default) ->
    list_to_existing_atom(config:get("smoosh", Key, Default)).
