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
    LuceneMajor = couch_util:get_value(<<"lucene_major">>, Fields),
    Analyzer = couch_util:get_value(<<"analyzer">>, Fields),
    Text = couch_util:get_value(<<"text">>, Fields),
    case nouveau_api:analyze(LuceneMajor, Text, Analyzer) of
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
    QueryArgs = #{
        query => ?l2b(chttpd:qs_value(Req, "q")),
        limit => list_to_integer(chttpd:qs_value(Req, "limit", "25")),
        sort => ?JSON_DECODE(chttpd:qs_value(Req, "sort", "null")),
        ranges => ?JSON_DECODE(chttpd:qs_value(Req, "ranges", "null")),
        counts => ?JSON_DECODE(chttpd:qs_value(Req, "counts", "null")),
        update => ?JSON_DECODE(chttpd:qs_value(Req, "update", "true")),
        bookmark => chttpd:qs_value(Req, "bookmark"),
        include_docs => parse_bool_param(
            "include_docs", chttpd:qs_value(Req, "include_docs", "false")
        )
    },
    handle_search_req(Req, DbName, DDoc, IndexName, QueryArgs, ?RETRY_LIMIT);
handle_search_req_int(#httpd{method = 'POST', path_parts = [_, _, _, _, IndexName]} = Req, Db, DDoc) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    DbName = couch_db:name(Db),
    ReqBody = chttpd:json_body(Req, [return_maps]),
    QueryArgs = #{
        query => maps:get(<<"q">>, ReqBody, undefined),
        limit => maps:get(<<"limit">>, ReqBody, 25),
        sort => maps:get(<<"sort">>, ReqBody, null),
        ranges => maps:get(<<"ranges">>, ReqBody, null),
        counts => maps:get(<<"counts">>, ReqBody, null),
        update => maps:get(<<"update">>, ReqBody, true),
        bookmark => maps:get(<<"bookmark">>, ReqBody, undefined),
        include_docs => maps:get(<<"include_docs">>, ReqBody, false)
    },
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
            couch_log:warning("search unavailable, retrying (~p of ~p)", [?RETRY_LIMIT - Retry + 1, ?RETRY_LIMIT]),
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

parse_bool_param(_, Val) when is_boolean(Val) ->
    Val;
parse_bool_param(_, "true") ->
    true;
parse_bool_param(_, "false") ->
    false;
parse_bool_param(Name, Val) ->
    Msg = io_lib:format("Invalid value for ~s: ~p", [Name, Val]),
    throw({query_parse_error, ?l2b(Msg)}).

check_if_enabled() ->
    case nouveau:enabled() of
        true ->
            ok;
        false ->
            throw(not_found)
    end.
