%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(nouveau_httpd).

-include_lib("couch/include/couch_db.hrl").

-export([
    handle_analyze_req/1,
    handle_search_req/3,
    handle_info_req/3,
    handle_cleanup_req/2
]).

-import(chttpd, [
    send_method_not_allowed/2,
    send_json/2, send_json/3,
    send_error/2
]).

-define(RETRY_LIMIT, 20).
-define(RETRY_SLEEP, 500).

handle_analyze_req(#httpd{method = 'POST'} = Req) ->
    check_if_enabled(),
    couch_httpd:validate_ctype(Req, "application/json"),
    {Fields} = chttpd:json_body_obj(Req),
    Analyzer = couch_util:get_value(<<"analyzer">>, Fields),
    Text = couch_util:get_value(<<"text">>, Fields),
    case nouveau_api:analyze(Text, Analyzer) of
        {ok, Tokens} ->
            send_json(Req, 200, {[{<<"tokens">>, Tokens}]});
        {error, Reason} ->
            send_error(Req, Reason)
    end;
handle_analyze_req(Req) ->
    send_method_not_allowed(Req, "POST").

handle_search_req(Req, Db, DDoc) ->
    check_if_enabled(),
    couch_stats:increment_counter([nouveau, active_searches]),
    T0 = erlang:monotonic_time(),
    try
        handle_search_req_int(Req, Db, DDoc)
    after
        T1 = erlang:monotonic_time(),
        couch_stats:decrement_counter([nouveau, active_searches]),
        RequestTime = erlang:convert_time_unit(T1 - T0, native, millisecond),
        couch_stats:update_histogram([nouveau, search_latency], RequestTime)
    end.

handle_search_req_int(#httpd{method = 'GET', path_parts = [_, _, _, _, IndexName]} = Req, Db, DDoc) ->
    DbName = couch_db:name(Db),
    QueryArgs = validate_query_args(#{
        query => chttpd:qs_value(Req, "q"),
        locale => chttpd:qs_value(Req, "locale"),
        partition => chttpd:qs_value(Req, "partition"),
        limit => chttpd:qs_value(Req, "limit"),
        sort => chttpd:qs_value(Req, "sort"),
        ranges => chttpd:qs_value(Req, "ranges"),
        counts => chttpd:qs_value(Req, "counts"),
        update => chttpd:qs_value(Req, "update"),
        bookmark => chttpd:qs_value(Req, "bookmark"),
        include_docs => chttpd:qs_value(Req, "include_docs")
    }),
    handle_search_req(Req, DbName, DDoc, IndexName, QueryArgs, ?RETRY_LIMIT);
handle_search_req_int(
    #httpd{method = 'POST', path_parts = [_, _, _, _, IndexName]} = Req, Db, DDoc
) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    DbName = couch_db:name(Db),
    ReqBody = chttpd:json_body(Req, [return_maps]),
    QueryArgs = validate_query_args(#{
        query => maps:get(<<"q">>, ReqBody, undefined),
        locale => maps:get(<<"locale">>, ReqBody, undefined),
        partition => chttpd:qs_value(Req, "partition"),
        limit => maps:get(<<"limit">>, ReqBody, undefined),
        sort => json_or_undefined(<<"sort">>, ReqBody),
        ranges => json_or_undefined(<<"ranges">>, ReqBody),
        counts => json_or_undefined(<<"counts">>, ReqBody),
        update => maps:get(<<"update">>, ReqBody, undefined),
        bookmark => maps:get(<<"bookmark">>, ReqBody, undefined),
        include_docs => maps:get(<<"include_docs">>, ReqBody, undefined)
    }),
    handle_search_req(Req, DbName, DDoc, IndexName, QueryArgs, ?RETRY_LIMIT);
handle_search_req_int(Req, _Db, _DDoc) ->
    send_method_not_allowed(Req, "GET, POST").

handle_search_req(#httpd{} = Req, DbName, DDoc, IndexName, QueryArgs, Retry) ->
    IncludeDocs = maps:get(include_docs, QueryArgs, false),
    case nouveau_fabric_search:go(DbName, DDoc, IndexName, QueryArgs) of
        {ok, SearchResults} ->
            RespBody = #{
                <<"bookmark">> => nouveau_bookmark:pack(maps:get(bookmark, SearchResults)),
                <<"total_hits">> => maps:get(<<"total_hits">>, SearchResults),
                <<"total_hits_relation">> => maps:get(<<"total_hits_relation">>, SearchResults),
                <<"hits">> => include_docs(
                    DbName, maps:get(<<"hits">>, SearchResults), IncludeDocs
                ),
                <<"counts">> => maps:get(<<"counts">>, SearchResults, null),
                <<"ranges">> => maps:get(<<"ranges">>, SearchResults, null)
            },
            HitCount = length(maps:get(<<"hits">>, RespBody)),
            incr_stats(HitCount, IncludeDocs),
            send_json(Req, 200, RespBody);
        {error, {service_unavailable, _}} when Retry > 1 ->
            couch_log:warning("search unavailable, retrying (~p of ~p)", [
                ?RETRY_LIMIT - Retry + 1, ?RETRY_LIMIT
            ]),
            timer:sleep(?RETRY_SLEEP),
            handle_search_req(Req, DbName, DDoc, IndexName, QueryArgs, Retry - 1);
        {error, Reason} ->
            send_error(Req, Reason)
    end.

handle_info_req(
    #httpd{method = 'GET', path_parts = [_, _, _, _, IndexName]} = Req,
    Db,
    #doc{id = Id} = DDoc
) ->
    check_if_enabled(),
    DbName = couch_db:name(Db),
    case nouveau_fabric_info:go(DbName, DDoc, IndexName) of
        {ok, IndexInfo} ->
            send_json(
                Req,
                200,
                {[
                    {name, <<Id/binary, "/", IndexName/binary>>},
                    {search_index, IndexInfo}
                ]}
            );
        {error, Reason} ->
            send_error(Req, Reason)
    end;
handle_info_req(#httpd{path_parts = [_, _, _, _, _]} = Req, _Db, _DDoc) ->
    check_if_enabled(),
    send_method_not_allowed(Req, "GET");
handle_info_req(Req, _Db, _DDoc) ->
    check_if_enabled(),
    send_error(Req, {bad_request, "path not recognized"}).

handle_cleanup_req(#httpd{method = 'POST'} = Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    ok = nouveau_fabric_cleanup:go(couch_db:name(Db)),
    send_json(Req, 202, {[{ok, true}]});
handle_cleanup_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

include_docs(_DbName, Hits, false) ->
    Hits;
include_docs(DbName, Hits, true) ->
    Ids = [maps:get(<<"id">>, Hit) || Hit <- Hits],
    {ok, Docs} = nouveau_fabric:get_json_docs(DbName, Ids),
    lists:zipwith(fun(Hit, Doc) -> Hit#{<<"doc">> => Doc} end, Hits, Docs).

incr_stats(HitCount, false) ->
    chttpd_stats:incr_rows(HitCount);
incr_stats(HitCount, true) ->
    chttpd_stats:incr_reads(HitCount),
    incr_stats(HitCount, false).

validate_query_args(#{} = QueryArgs) ->
    maps:map(fun validate_query_arg/2, QueryArgs).

validate_query_arg(query, undefined) ->
    throw({query_parse_error, <<"q parameter is mandatory">>});
validate_query_arg(query, Val) when is_list(Val); is_binary(Val) ->
    couch_util:to_binary(Val);
validate_query_arg(locale, undefined) ->
    null;
validate_query_arg(locale, Val) when is_list(Val); is_binary(Val) ->
    couch_util:to_binary(Val);
validate_query_arg(partition, undefined) ->
    null;
validate_query_arg(partition, Val) when is_list(Val); is_binary(Val) ->
    couch_util:to_binary(Val);
validate_query_arg(limit, undefined) ->
    25;
validate_query_arg(limit, Limit) when is_integer(Limit), Limit > 0 ->
    Limit;
validate_query_arg(limit, Limit) when is_integer(Limit) ->
    throw({query_parse_error, <<"limit parameter must be greater than zero">>});
validate_query_arg(limit, List) when is_list(List) ->
    try
        list_to_integer(List)
    catch
        error:badarg ->
            throw({query_parse_error, <<"limit parameter must be an integer">>})
    end;
validate_query_arg(sort, undefined) ->
    null;
validate_query_arg(sort, {json, Sort}) when is_binary(Sort) ->
    [Sort];
validate_query_arg(sort, {json, Sort}) ->
    ok = is_list_of_strings(<<"counts">>, Sort),
    Sort;
validate_query_arg(sort, Sort) ->
    validate_query_arg(sort, {json, ?JSON_DECODE(Sort, [return_maps])});
validate_query_arg(ranges, undefined) ->
    null;
validate_query_arg(ranges, {json, Ranges}) when is_map(Ranges) ->
    maps:foreach(fun is_valid_range/2, Ranges),
    Ranges;
validate_query_arg(ranges, Ranges) ->
    validate_query_arg(ranges, {json, ?JSON_DECODE(Ranges, [return_maps])});
validate_query_arg(counts, undefined) ->
    null;
validate_query_arg(counts, {json, Counts}) when is_list(Counts) ->
    ok = is_list_of_strings(<<"counts">>, Counts),
    Counts;
validate_query_arg(counts, Counts) ->
    validate_query_arg(counts, {json, ?JSON_DECODE(Counts, [return_maps])});
validate_query_arg(update, undefined) ->
    true;
validate_query_arg(update, Bool) when is_boolean(Bool) ->
    Bool;
validate_query_arg(update, "false") ->
    false;
validate_query_arg(update, "true") ->
    true;
validate_query_arg(bookmark, undefined) ->
    null;
validate_query_arg(bookmark, Bookmark) ->
    Bookmark;
validate_query_arg(include_docs, Bool) when is_boolean(Bool) ->
    Bool;
validate_query_arg(include_docs, undefined) ->
    false;
validate_query_arg(include_docs, "false") ->
    false;
validate_query_arg(include_docs, "true") ->
    true;
validate_query_arg(Key, Val) ->
    Msg = io_lib:format("Invalid value for ~p: ~p", [Key, Val]),
    throw({query_parse_error, ?l2b(Msg)}).

json_or_undefined(Key, Map) when is_binary(Key), is_map(Map) ->
    case maps:get(Key, Map, undefined) of
        undefined ->
            undefined;
        Val ->
            {json, Val}
    end.

is_list_of_strings(Name, Val) when is_list(Val) ->
    AllBinaries = lists:all(fun is_binary/1, Val),
    if
        AllBinaries ->
            ok;
        true ->
            throw(
                {query_parser_error, <<"all items in ", Name/binary, " parameter must be strings">>}
            )
    end;
is_list_of_strings(Name, _Val) ->
    throw({query_parser_error, <<Name/binary, " parameter must be a list of strings">>}).

is_valid_range(FieldName, _Ranges) when not is_binary(FieldName) ->
    throw({query_parse_error, <<"range keys must be strings">>});
is_valid_range(_FieldName, Ranges) when not is_list(Ranges) ->
    throw({query_parse_error, <<"range values must be lists of objects">>});
is_valid_range(FieldName, Ranges) when is_binary(FieldName), is_list(Ranges) ->
    AllMaps = lists:all(fun is_map/1, Ranges),
    if
        AllMaps -> ok;
        true -> throw({query_parser_error, <<"all values in ranges parameter must be objects">>})
    end.

check_if_enabled() ->
    case nouveau:enabled() of
        true ->
            ok;
        false ->
            throw(not_found)
    end.
