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

-module(couch_httpd_stats_handlers).
-include("couch_db.hrl").

-export([handle_stats_req/1]).

handle_stats_req(#httpd{method='GET', path_parts=[_]}=Req) ->
    Stats = couch_stats:fetch(),
    Nested = nest(Stats),
    EJSON = to_ejson(Nested),
    couch_httpd:send_json(Req, EJSON).

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

maybe_format_key(Key) when is_integer(Key) ->
    maybe_format_key(integer_to_list(Key));
maybe_format_key(Key) when is_list(Key) ->
    list_to_binary(Key);
maybe_format_key(Key) ->
    Key.
