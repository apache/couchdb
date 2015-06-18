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

-module(couch_stats_httpd).
-include_lib("couch/include/couch_db.hrl").

-export([handle_stats_req/1]).

%% exported for use by chttpd_misc
-export([transform_stats/1, nest/1, to_ejson/1, extract_path/2]).

handle_stats_req(#httpd{method='GET', path_parts=[_ | Path]}=Req) ->
    flush(Req),
    Stats0 = couch_stats:fetch(),
    Stats = transform_stats(Stats0),
    Nested = nest(Stats),
    EJSON0 = to_ejson(Nested),
    EJSON1 = extract_path(Path, EJSON0),
    couch_httpd:send_json(Req, EJSON1).


transform_stats(Stats) ->
    transform_stats(Stats, []).

transform_stats([], Acc) ->
    Acc;
transform_stats([{Key, Props} | Rest], Acc) ->
    {_, Type} = proplists:lookup(type, Props),
    transform_stats(Rest, [{Key, transform_stat(Type, Props)} | Acc]).


transform_stat(counter, Props) ->
    Props;
transform_stat(gauge, Props) ->
    Props;
transform_stat(histogram, Props) ->
    lists:map(fun
        ({value, Value}) ->
            {value, lists:map(fun
                ({Key, List}) when Key == percentile; Key == histogram ->
                    {Key, [tuple_to_list(Item) || Item <- List]};
                (Else) ->
                    Else
            end, Value)};
        (Else) ->
            Else
      end, Props).


nest(Proplist) ->
    nest(Proplist, []).

nest([], Acc) ->
    Acc;
nest([{[Key|Keys], Value}|Rest], Acc) ->
    Acc1 = case proplists:lookup(Key, Acc) of
        {Key, Old} ->
            [{Key, nest([{Keys, Value}], Old)}|proplists:delete(Key, Acc)];
        none ->
            Term = lists:foldr(fun(K, A) -> [{K, A}] end, Value, Keys),
            [{Key, Term}|Acc]
    end,
    nest(Rest, Acc1).


to_ejson([{_, _}|_]=Proplist) ->
    EJSONProps = lists:map(
       fun({Key, Value}) -> {maybe_format_key(Key), to_ejson(Value)} end,
       Proplist
    ),
    {EJSONProps};
to_ejson(NotAProplist) ->
    NotAProplist.


extract_path([], EJSON) ->
    EJSON;
extract_path([Key | Rest], {Props}) ->
    case proplists:lookup(Key, Props) of
        {Key, SubEJSON} ->
            extract_path(Rest, SubEJSON);
        none ->
            null
    end;
extract_path([_ | _], _NotAnObject) ->
    null.


maybe_format_key(Key) when is_list(Key) ->
    list_to_binary(Key);
maybe_format_key(Key) when is_atom(Key) ->
    list_to_binary(atom_to_list(Key));
maybe_format_key(Key) when is_integer(Key) ->
    list_to_binary(integer_to_list(Key));
maybe_format_key(Key) when is_binary(Key) ->
    Key.

flush(Req) ->
    case couch_util:get_value("flush", couch_httpd:qs(Req)) of
        "true" ->
            couch_stats_aggregator:flush();
        _Else ->
            ok
    end.
