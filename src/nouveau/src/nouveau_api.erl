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

-module(nouveau_api).

-include("nouveau.hrl").

-export([
    analyze/2,
    index_info/1,
    create_index/2,
    delete_path/1,
    delete_doc/3,
    update_doc/4,
    search/2
]).

-define(JSON_CONTENT_TYPE, {"Content-Type", "application/json"}).

analyze(Text, Analyzer) when
    is_binary(Text), is_binary(Analyzer)
->
    ReqBody = {[{<<"text">>, Text}, {<<"analyzer">>, Analyzer}]},
    Resp = send_if_enabled(
        nouveau_util:nouveau_url() ++ "/analyze", [?JSON_CONTENT_TYPE], post, jiffy:encode(ReqBody)
    ),
    case Resp of
        {ok, "200", _, RespBody} ->
            Json = jiffy:decode(RespBody, [return_maps]),
            {ok, maps:get(<<"tokens">>, Json)};
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end;
analyze(_, _) ->
    {error, {bad_request, <<"'text' and 'analyzer' fields must be non-empty strings">>}}.

index_info(IndexName) when
    is_binary(IndexName)
->
    Resp = send_if_enabled(index_url(IndexName), [], get),
    case Resp of
        {ok, "200", _, RespBody} ->
            {ok, jiffy:decode(RespBody)};
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

create_index(IndexName, IndexDefinition) when
    is_binary(IndexName)
->
    Resp = send_if_enabled(
        index_url(IndexName), [?JSON_CONTENT_TYPE], put, jiffy:encode(IndexDefinition)
    ),
    case Resp of
        {ok, "204", _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

delete_path(Path) when
    is_binary(Path)
->
    Resp = send_if_enabled(index_url(Path), [?JSON_CONTENT_TYPE], delete, []),
    case Resp of
        {ok, "204", _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

delete_doc(IndexName, DocId, UpdateSeq) when
    is_binary(IndexName), is_binary(DocId)
->
    ReqBody = {[{<<"seq">>, UpdateSeq}]},
    Resp = send_if_enabled(
        doc_url(IndexName, DocId), [?JSON_CONTENT_TYPE], delete, jiffy:encode(ReqBody)
    ),
    case Resp of
        {ok, "204", _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

update_doc(IndexName, DocId, UpdateSeq, Fields) when
    is_binary(IndexName), is_binary(DocId), is_integer(UpdateSeq), is_list(Fields)
->
    ReqBody = {[{<<"seq">>, UpdateSeq}, {<<"fields">>, Fields}]},
    Resp = send_if_enabled(
        doc_url(IndexName, DocId), [?JSON_CONTENT_TYPE], put, jiffy:encode(ReqBody)
    ),
    case Resp of
        {ok, "204", _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

search(IndexName, QueryArgs) when
    is_binary(IndexName)
->
    Resp = send_if_enabled(
        search_url(IndexName), [?JSON_CONTENT_TYPE], post, jiffy:encode(QueryArgs)
    ),
    case Resp of
        {ok, "200", _, RespBody} ->
            {ok, jiffy:decode(RespBody, [return_maps])};
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

%% private functions

index_url(IndexName) ->
    lists:flatten(
        io_lib:format(
            "~s/index/~s",
            [nouveau_util:nouveau_url(), couch_util:url_encode(IndexName)]
        )
    ).

doc_url(IndexName, DocId) ->
    lists:flatten(
        io_lib:format(
            "~s/index/~s/doc/~s",
            [
                nouveau_util:nouveau_url(),
                couch_util:url_encode(IndexName),
                couch_util:url_encode(DocId)
            ]
        )
    ).

search_url(IndexName) ->
    index_url(IndexName) ++ "/search".

jaxrs_error("400", Body) ->
    {bad_request, message(Body)};
jaxrs_error("404", Body) ->
    {not_found, message(Body)};
jaxrs_error("405", Body) ->
    {method_not_allowed, message(Body)};
jaxrs_error("422", Body) ->
    {bad_request, lists:join(" and ", errors(Body))};
jaxrs_error("500", Body) ->
    {internal_server_error, message(Body)}.

send_error({conn_failed, _}) ->
    {error, {service_unavailable, <<"Search service unavailable.">>}};
send_error(Reason) ->
    {error, Reason}.

message(Body) ->
    Json = jiffy:decode(Body, [return_maps]),
    maps:get(<<"message">>, Json).

errors(Body) ->
    Json = jiffy:decode(Body, [return_maps]),
    maps:get(<<"errors">>, Json).

send_if_enabled(Url, Header, Method) ->
    send_if_enabled(Url, Header, Method, []).

send_if_enabled(Url, Header, Method, Body) ->
    case nouveau:enabled() of
        true ->
            ibrowse:send_req(Url, Header, Method, Body);
        false ->
            {error, nouveau_not_enabled}
    end.
