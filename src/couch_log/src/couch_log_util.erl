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

-module(couch_log_util).


-export([
    should_log/1,
    iso8601_timestamp/0,
    get_msg_id/0,

    level_to_integer/1,
    level_to_atom/1,
    level_to_string/1,

    string_p/1
]).


-include("couch_log.hrl").


-spec should_log(#log_entry{} | atom()) -> boolean().
should_log(#log_entry{level = Level}) ->
    should_log(Level);

should_log(Level) ->
    level_to_integer(Level) >= couch_log_config:get(level_int).


-spec iso8601_timestamp() -> string().
iso8601_timestamp() ->
    {_,_,Micro} = Now = os:timestamp(),
    {{Year,Month,Date},{Hour,Minute,Second}} = calendar:now_to_datetime(Now),
    Format = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B.~6.10.0BZ",
    io_lib:format(Format, [Year, Month, Date, Hour, Minute, Second, Micro]).


-spec get_msg_id() -> string().
get_msg_id() ->
    case erlang:get(nonce) of
        undefined -> "--------";
        MsgId -> MsgId
    end.


-spec level_to_integer(atom() | string() | integer()) -> integer().
level_to_integer(L) when L >= 0, L =< 9 -> L;
level_to_integer(debug)                 -> 1;
level_to_integer(info)                  -> 2;
level_to_integer(notice)                -> 3;
level_to_integer(warning)               -> 4;
level_to_integer(warn)                  -> 4;
level_to_integer(error)                 -> 5;
level_to_integer(err)                   -> 5;
level_to_integer(critical)              -> 6;
level_to_integer(crit)                  -> 6;
level_to_integer(alert)                 -> 7;
level_to_integer(emergency)             -> 8;
level_to_integer(emerg)                 -> 8;
level_to_integer(none)                  -> 9;
level_to_integer("debug")               -> 1;
level_to_integer("info")                -> 2;
level_to_integer("notice")              -> 3;
level_to_integer("warning")             -> 4;
level_to_integer("warn")                -> 4;
level_to_integer("error")               -> 5;
level_to_integer("err")                 -> 5;
level_to_integer("critical")            -> 6;
level_to_integer("crit")                -> 6;
level_to_integer("alert")               -> 7;
level_to_integer("emergency")           -> 8;
level_to_integer("emerg")               -> 8;
level_to_integer("none")                -> 9;
level_to_integer("1")                   -> 1;
level_to_integer("2")                   -> 2;
level_to_integer("3")                   -> 3;
level_to_integer("4")                   -> 4;
level_to_integer("5")                   -> 5;
level_to_integer("6")                   -> 6;
level_to_integer("7")                   -> 7;
level_to_integer("8")                   -> 8;
level_to_integer("9")                   -> 9.


-spec level_to_atom(atom() | string() | integer()) -> atom().
level_to_atom(L) when is_atom(L)    -> L;
level_to_atom("1")                  -> debug;
level_to_atom("debug")              -> debug;
level_to_atom("2")                  -> info;
level_to_atom("info")               -> info;
level_to_atom("3")                  -> notice;
level_to_atom("notice")             -> notice;
level_to_atom("4")                  -> warning;
level_to_atom("warning")            -> warning;
level_to_atom("warn")               -> warning;
level_to_atom("5")                  -> error;
level_to_atom("error")              -> error;
level_to_atom("err")                -> error;
level_to_atom("6")                  -> critical;
level_to_atom("critical")           -> critical;
level_to_atom("crit")               -> critical;
level_to_atom("7")                  -> alert;
level_to_atom("alert")              -> alert;
level_to_atom("8")                  -> emergency;
level_to_atom("emergency")          -> emergency;
level_to_atom("emerg")              -> emergency;
level_to_atom("9")                  -> none;
level_to_atom("none")               -> none;
level_to_atom(V) when is_integer(V) -> level_to_atom(integer_to_list(V));
level_to_atom(V) when is_list(V)    -> info.


level_to_string(L) when is_atom(L)  -> atom_to_list(L);
level_to_string(L)                  -> atom_to_list(level_to_atom(L)).



% From error_logger_file_h via lager_stdlib.erl
string_p([]) ->
    false;
string_p(Term) ->
    string_p1(Term).

string_p1([H|T]) when is_integer(H), H >= $\s, H < 256 ->
    string_p1(T);
string_p1([$\n|T]) -> string_p1(T);
string_p1([$\r|T]) -> string_p1(T);
string_p1([$\t|T]) -> string_p1(T);
string_p1([$\v|T]) -> string_p1(T);
string_p1([$\b|T]) -> string_p1(T);
string_p1([$\f|T]) -> string_p1(T);
string_p1([$\e|T]) -> string_p1(T);
string_p1([H|T]) when is_list(H) ->
    case string_p1(H) of
        true -> string_p1(T);
        _    -> false
    end;
string_p1([]) -> true;
string_p1(_) ->  false.
