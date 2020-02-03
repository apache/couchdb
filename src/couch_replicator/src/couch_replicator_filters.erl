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

-module(couch_replicator_filters).

-export([
    parse/1,
    fetch/3,
    view_type/2,
    ejsort/1
]).

-include_lib("couch/include/couch_db.hrl").


% Parse the filter from replication options proplist.
% Return {ok, {FilterType,...}} | {error, ParseError}.
% For `user` filter, i.e. filters specified as user code
% in source database, this code doesn't fetch the filter
% code, but only returns the name of the filter.
-spec parse([_]) ->
    {ok, nil} |
    {ok, {view, binary(), {[_]}}} |
    {ok, {user, {binary(), binary()}, {[_]}}} |
    {ok, {docids, [_]}} |
    {ok, {mango, {[_]}}} |
    {error, binary()}.
parse(Options) ->
    Filter = couch_util:get_value(filter, Options),
    DocIds = couch_util:get_value(doc_ids, Options),
    Selector = couch_util:get_value(selector, Options),
    case {Filter, DocIds, Selector} of
        {undefined, undefined, undefined} ->
            {ok, nil};
        {<<"_", _/binary>>, undefined, undefined} ->
            {ok, {view, Filter, query_params(Options)}};
        {_, undefined, undefined} ->
            case parse_user_filter(Filter) of
                {ok, {Doc, FilterName}} ->
                    {ok, {user, {Doc, FilterName}, query_params(Options)}};
                {error, Error} ->
                    {error, Error}
            end;
        {undefined, _, undefined} ->
            {ok, {docids, DocIds}};
        {undefined, undefined, _} ->
            {ok, {mango, ejsort(mango_selector:normalize(Selector))}};
        _ ->
            Err = "`selector`, `filter` and `doc_ids` are mutually exclusive",
            {error, list_to_binary(Err)}
    end.


% Fetches body of filter function from source database. Guaranteed to either
% return {ok, Body} or an {error, Reason}. Also assume this function might
% block due to network / socket issues for an undeterminted amount of time.
-spec fetch(binary(), binary(), binary()) ->
    {ok, {[_]}} | {error, binary()}.
fetch(DDocName, FilterName, Source) ->
    {Pid, Ref} = spawn_monitor(fun() ->
        try fetch_internal(DDocName, FilterName, Source) of
            Resp ->
                exit({exit_ok, Resp})
        catch
            throw:{fetch_error, Reason} ->
                exit({exit_fetch_error, Reason});
            _OtherTag:Reason ->
                exit({exit_other_error, Reason})
        end
    end),
    receive
        {'DOWN', Ref, process, Pid, {exit_ok, Resp}} ->
            {ok, Resp};
        {'DOWN', Ref, process, Pid, {exit_fetch_error, Reason}} ->
            {error, Reason};
        {'DOWN', Ref, process, Pid, {exit_other_error, Reason}} ->
            {error, couch_util:to_binary(Reason)}
    end.


% Get replication type and view (if any) from replication document props
-spec view_type([_], [_]) ->
    {view, {binary(), binary()}} | {db, nil} | {error, binary()}.
view_type(Props, Options) ->
    case couch_util:get_value(<<"filter">>, Props) of
        <<"_view">> ->
            {QP}  = couch_util:get_value(query_params, Options, {[]}),
            ViewParam = couch_util:get_value(<<"view">>, QP),
            case re:split(ViewParam, <<"/">>) of
                [DName, ViewName] ->
                    {view, {<< "_design/", DName/binary >>, ViewName}};
                _ ->
                    {error, <<"Invalid `view` parameter.">>}
            end;
        _ ->
            {db, nil}
    end.


% Private functions

fetch_internal(DDocName, FilterName, Source) ->
    Db = case (catch couch_replicator_api_wrap:db_open(Source)) of
    {ok, Db0} ->
        Db0;
    DbError ->
        DbErrorMsg = io_lib:format("Could not open source database `~s`: ~s",
           [couch_replicator_api_wrap:db_uri(Source),
               couch_util:to_binary(DbError)]),
        throw({fetch_error, iolist_to_binary(DbErrorMsg)})
    end,
    try
        Body = case (catch couch_replicator_api_wrap:open_doc(
            Db, <<"_design/", DDocName/binary>>, [ejson_body])) of
        {ok, #doc{body = Body0}} ->
            Body0;
        DocError ->
            DocErrorMsg = io_lib:format(
                "Couldn't open document `_design/~s` from source "
                "database `~s`: ~s", [DDocName,
                couch_replicator_api_wrap:db_uri(Source),
                couch_util:to_binary(DocError)]
            ),
            throw({fetch_error, iolist_to_binary(DocErrorMsg)})
        end,
        try
            Code = couch_util:get_nested_json_value(
                     Body, [<<"filters">>, FilterName]),
            re:replace(Code, [$^, "\s*(.*?)\s*", $$], "\\1", [{return, binary}])
         catch
             _Tag:CodeError ->
                 CodeErrorMsg = io_lib:format(
                     "Couldn't parse filter code from document ~s on `~s` "
                     " Error: ~s", [DDocName,
                     couch_replicator_api_wrap:db_uri(Source),
                     couch_util:to_binary(CodeError)]
                 ),
                 throw({fetch_error, CodeErrorMsg})
         end
    after
        couch_replicator_api_wrap:db_close(Db)
    end.


-spec query_params([_]) -> {[_]}.
query_params(Options)->
    couch_util:get_value(query_params, Options, {[]}).


parse_user_filter(Filter) ->
    case re:run(Filter, "(.*?)/(.*)", [{capture, [1, 2], binary}]) of
        {match, [DDocName0, FilterName0]} ->
            {ok, {DDocName0, FilterName0}};
        _ ->
            {error, <<"Invalid filter. Must match `ddocname/filtername`.">>}
    end.


% Sort an EJSON object's properties to attempt
% to generate a unique representation. This is used
% to reduce the chance of getting different
% replication checkpoints for the same Mango selector
ejsort({V})->
    ejsort_props(V, []);
ejsort(V) when is_list(V) ->
    ejsort_array(V, []);
ejsort(V) ->
    V.


ejsort_props([], Acc)->
    {lists:keysort(1, Acc)};
ejsort_props([{K, V}| R], Acc) ->
    ejsort_props(R, [{K, ejsort(V)} | Acc]).


ejsort_array([], Acc)->
    lists:reverse(Acc);
ejsort_array([V | R], Acc) ->
    ejsort_array(R, [ejsort(V) | Acc]).


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

ejsort_basic_values_test() ->
    ?assertEqual(ejsort(0), 0),
    ?assertEqual(ejsort(<<"a">>), <<"a">>),
    ?assertEqual(ejsort(true), true),
    ?assertEqual(ejsort([]), []),
    ?assertEqual(ejsort({[]}), {[]}).


ejsort_compound_values_test() ->
    ?assertEqual(ejsort([2, 1, 3, <<"a">>]), [2, 1, 3, <<"a">>]),
    Ej1 = {[{<<"a">>, 0}, {<<"c">>, 0},  {<<"b">>, 0}]},
    Ej1s =  {[{<<"a">>, 0}, {<<"b">>, 0}, {<<"c">>, 0}]},
    ?assertEqual(ejsort(Ej1), Ej1s),
    Ej2 = {[{<<"x">>, Ej1}, {<<"z">>, Ej1}, {<<"y">>, [Ej1, Ej1]}]},
    ?assertEqual(ejsort(Ej2),
        {[{<<"x">>, Ej1s}, {<<"y">>, [Ej1s, Ej1s]}, {<<"z">>, Ej1s}]}).

-endif.
