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
-include("nouveau.hrl").

-export([handle_analyze_req/1, handle_search_req/3, handle_info_req/3]).

-import(chttpd, [
    send_method_not_allowed/2,
    send_json/2, send_json/3,
    send_error/2
]).

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

handle_search_req(#httpd{method = 'GET', path_parts = [_, _, _, _, IndexName]} = Req, Db, DDoc) ->
    check_if_enabled(),
    DbName = couch_db:name(Db),
    Query = ?l2b(chttpd:qs_value(Req, "q")),
    Limit = list_to_integer(chttpd:qs_value(Req, "limit", "25")),
    Sort = ?JSON_DECODE(chttpd:qs_value(Req, "sort", "null")),
    Ranges = ?JSON_DECODE(chttpd:qs_value(Req, "ranges", "null")),
    Counts = ?JSON_DECODE(chttpd:qs_value(Req, "counts", "null")),
    Update = chttpd:qs_value(Req, "update", "true"),
    Bookmark = chttpd:qs_value(Req, "bookmark"),
    QueryArgs = #{
        query => Query,
        limit => Limit,
        sort => Sort,
        ranges => Ranges,
        counts => Counts,
        update => Update,
        bookmark => Bookmark
    },
    case nouveau_fabric_search:go(DbName, DDoc, IndexName, QueryArgs) of
        {ok, SearchResults} ->
            RespBody = #{
                <<"bookmark">> => nouveau_bookmark:pack(maps:get(bookmark, SearchResults)),
                <<"total_hits">> => maps:get(<<"total_hits">>, SearchResults),
                <<"total_hits_relation">> => maps:get(<<"total_hits_relation">>, SearchResults),
                <<"hits">> => maps:get(<<"hits">>, SearchResults),
                <<"counts">> => maps:get(<<"counts">>, SearchResults, null),
                <<"ranges">> => maps:get(<<"ranges">>, SearchResults, null)
            },
            send_json(Req, 200, RespBody);
        {error, Reason} ->
            send_error(Req, Reason)
    end.

handle_info_req(_Req, _Db, _DDoc) ->
    check_if_enabled(),
    ok.

check_if_enabled() ->
    case nouveau:enabled() of
        true ->
            ok;
        false ->
            throw(not_found)
    end.
