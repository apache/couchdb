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
    delete_path/2,
    delete_doc_async/5,
    purge_doc/5,
    update_doc_async/7,
    search/2,
    set_purge_seq/4,
    set_update_seq/4,
    drain_async_responses/2,
    jaxrs_error/2
]).

-define(JSON_CONTENT_TYPE, {"Content-Type", "application/json"}).

analyze(Text, Analyzer) when
    is_binary(Text), is_binary(Analyzer)
->
    ReqBody = {[{<<"text">>, Text}, {<<"analyzer">>, Analyzer}]},
    Resp = send_if_enabled(
        nouveau_util:nouveau_url() ++ "/analyze",
        [?JSON_CONTENT_TYPE],
        post,
        jiffy:encode(ReqBody)
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

index_info(#index{} = Index) ->
    Resp = send_if_enabled(index_url(Index), [], get),
    case Resp of
        {ok, "200", _, RespBody} ->
            {ok, jiffy:decode(RespBody, [return_maps])};
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

create_index(#index{} = Index, IndexDefinition) ->
    Resp = send_if_enabled(
        index_url(Index), [?JSON_CONTENT_TYPE], put, jiffy:encode(IndexDefinition)
    ),
    case Resp of
        {ok, "204", _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

delete_path(Path) ->
    delete_path(Path, []).

delete_path(Path, Exclusions) when
    is_binary(Path), is_list(Exclusions)
->
    Resp = send_if_enabled(
        index_path(Path), [?JSON_CONTENT_TYPE], delete, jiffy:encode(Exclusions)
    ),
    case Resp of
        {ok, "204", _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

delete_doc_async(ConnPid, #index{} = Index, DocId, MatchSeq, UpdateSeq) when
    is_pid(ConnPid),
    is_binary(DocId),
    is_integer(MatchSeq),
    MatchSeq >= 0,
    is_integer(UpdateSeq),
    UpdateSeq > 0
->
    ReqBody = #{match_seq => MatchSeq, seq => UpdateSeq, purge => false},
    send_direct_if_enabled(
        ConnPid,
        doc_url(Index, DocId),
        [?JSON_CONTENT_TYPE],
        delete,
        jiffy:encode(ReqBody),
        [
            {stream_to, self()}
        ]
    ).

purge_doc(ConnPid, #index{} = Index, DocId, MatchSeq, PurgeSeq) when
    is_pid(ConnPid),
    is_binary(DocId),
    is_integer(MatchSeq),
    MatchSeq >= 0,
    is_integer(PurgeSeq),
    PurgeSeq > 0
->
    ReqBody = #{match_seq => MatchSeq, seq => PurgeSeq, purge => true},
    Resp = send_direct_if_enabled(
        ConnPid, doc_url(Index, DocId), [?JSON_CONTENT_TYPE], delete, jiffy:encode(ReqBody), []
    ),
    case Resp of
        {ok, "204", _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

update_doc_async(ConnPid, #index{} = Index, DocId, MatchSeq, UpdateSeq, Partition, Fields) when
    is_pid(ConnPid),
    is_binary(DocId),
    is_integer(MatchSeq),
    MatchSeq >= 0,
    is_integer(UpdateSeq),
    UpdateSeq > 0,
    (is_binary(Partition) orelse Partition == null),
    is_list(Fields)
->
    ReqBody = #{
        match_seq => MatchSeq,
        seq => UpdateSeq,
        partition => Partition,
        fields => Fields
    },
    send_direct_if_enabled(
        ConnPid,
        doc_url(Index, DocId),
        [?JSON_CONTENT_TYPE],
        put,
        jiffy:encode(ReqBody),
        [
            {stream_to, self()}
        ]
    ).

search(#index{} = Index, QueryArgs) ->
    Resp = send_if_enabled(
        search_url(Index), [?JSON_CONTENT_TYPE], post, jiffy:encode(QueryArgs)
    ),
    case Resp of
        {ok, "200", _, RespBody} ->
            {ok, jiffy:decode(RespBody, [return_maps])};
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

set_update_seq(ConnPid, #index{} = Index, MatchSeq, UpdateSeq) ->
    ReqBody = #{
        match_update_seq => MatchSeq,
        update_seq => UpdateSeq
    },
    set_seq(ConnPid, Index, ReqBody).
set_purge_seq(ConnPid, #index{} = Index, MatchSeq, PurgeSeq) ->
    ReqBody = #{
        match_purge_seq => MatchSeq,
        purge_seq => PurgeSeq
    },
    set_seq(ConnPid, Index, ReqBody).

set_seq(ConnPid, #index{} = Index, ReqBody) ->
    Resp = send_direct_if_enabled(
        ConnPid, index_url(Index), [?JSON_CONTENT_TYPE], post, jiffy:encode(ReqBody), []
    ),
    case Resp of
        {ok, "204", _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

%% wait for enough async responses to reduce the Queue to Min length.
drain_async_responses(Queue0, Min) when Min >= 0 ->
    case queue:len(Queue0) > Min of
        true ->
            {{value, ReqId}, Queue1} = queue:out(Queue0),
            wait_for_response(ReqId),
            drain_async_responses(Queue1, Min);
        false ->
            Queue0
    end.

wait_for_response(ReqId) ->
    case drain_async_response(ReqId) of
        {ok, "204", _Headers, _Body} ->
            ok;
        {ok, StatusCode, _Headers, RespBody} ->
            exit({error, jaxrs_error(StatusCode, RespBody)})
    end.

drain_async_response(ReqId) ->
    drain_async_response(ReqId, undefined, undefined, undefined).

drain_async_response(ReqId, Code0, Headers0, Body0) ->
    receive
        {ibrowse_async_headers, ReqId, Code1, Headers1} ->
            drain_async_response(ReqId, Code1, Headers1, Body0);
        {ibrowse_async_response, ReqId, Body1} ->
            drain_async_response(ReqId, Code0, Headers0, Body1);
        {ibrowse_async_response_end, ReqId} ->
            {ok, Code0, Headers0, Body0}
    end.

%% private functions

index_path(Path) ->
    lists:flatten(
        io_lib:format(
            "~s/index/~s",
            [
                nouveau_util:nouveau_url(),
                couch_util:url_encode(Path)
            ]
        )
    ).

index_url(#index{} = Index) ->
    lists:flatten(
        io_lib:format(
            "~s/index/~s",
            [
                nouveau_util:nouveau_url(),
                couch_util:url_encode(nouveau_util:index_name(Index))
            ]
        )
    ).

doc_url(#index{} = Index, DocId) ->
    lists:flatten(
        io_lib:format(
            "~s/index/~s/doc/~s",
            [
                nouveau_util:nouveau_url(),
                couch_util:url_encode(nouveau_util:index_name(Index)),
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
jaxrs_error("409", Body) ->
    {conflict, message(Body)};
jaxrs_error("417", Body) ->
    {expectation_failed, message(Body)};
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
    send_if_enabled(Url, Header, Method, Body, []).

send_if_enabled(Url, Header, Method, Body, Options) ->
    case nouveau:enabled() of
        true ->
            ibrowse:send_req(Url, Header, Method, Body, Options);
        false ->
            {error, nouveau_not_enabled}
    end.

send_direct_if_enabled(ConnPid, Url, Header, Method, Body, Options) ->
    case nouveau:enabled() of
        true ->
            ibrowse:send_req_direct(ConnPid, Url, Header, Method, Body, Options);
        false ->
            {error, nouveau_not_enabled}
    end.
