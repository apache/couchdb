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
    delete_doc/4,
    purge_doc/4,
    update_doc/6,
    search/2,
    set_purge_seq/3,
    set_update_seq/3,
    jaxrs_error/2
]).

-define(JSON_CONTENT_TYPE, {"Content-Type", "application/json"}).

analyze(Text, Analyzer) when
    is_binary(Text), is_binary(Analyzer)
->
    ReqBody = {[{<<"text">>, Text}, {<<"analyzer">>, Analyzer}]},
    Resp = send_if_enabled(
        "/analyze",
        [?JSON_CONTENT_TYPE],
        <<"POST">>,
        jiffy:encode(ReqBody)
    ),
    case Resp of
        {ok, 200, _, RespBody} ->
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
    Resp = send_if_enabled(index_path(Index), [], <<"GET">>),
    case Resp of
        {ok, 200, _, RespBody} ->
            {ok, jiffy:decode(RespBody, [return_maps])};
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

create_index(#index{} = Index, IndexDefinition) ->
    Resp = send_if_enabled(
        index_path(Index), [?JSON_CONTENT_TYPE], <<"PUT">>, jiffy:encode(IndexDefinition)
    ),
    case Resp of
        {ok, 200, _, _} ->
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
        index_path(Path), [?JSON_CONTENT_TYPE], <<"DELETE">>, jiffy:encode(Exclusions)
    ),
    case Resp of
        {ok, 200, _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

delete_doc(#index{} = Index, DocId, MatchSeq, UpdateSeq) when
    is_binary(DocId),
    is_integer(MatchSeq),
    MatchSeq >= 0,
    is_integer(UpdateSeq),
    UpdateSeq > 0
->
    ReqBody = #{match_seq => MatchSeq, seq => UpdateSeq, purge => false},
    Resp = send_if_enabled(
        doc_path(Index, DocId),
        [?JSON_CONTENT_TYPE],
        <<"DELETE">>,
        jiffy:encode(ReqBody)
    ),
    case Resp of
        {ok, 200, _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

purge_doc(#index{} = Index, DocId, MatchSeq, PurgeSeq) when
    is_binary(DocId),
    is_integer(MatchSeq),
    MatchSeq >= 0,
    is_integer(PurgeSeq),
    PurgeSeq > 0
->
    ReqBody = #{match_seq => MatchSeq, seq => PurgeSeq, purge => true},
    Resp = send_if_enabled(
        doc_path(Index, DocId), [?JSON_CONTENT_TYPE], <<"DELETE">>, jiffy:encode(ReqBody)
    ),
    case Resp of
        {ok, 200, _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

update_doc(#index{} = Index, DocId, MatchSeq, UpdateSeq, Partition, Fields) when
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
    Resp = send_if_enabled(
        doc_path(Index, DocId),
        [?JSON_CONTENT_TYPE],
        <<"PUT">>,
        jiffy:encode(ReqBody)
    ),
    case Resp of
        {ok, 200, _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

search(#index{} = Index, QueryArgs) ->
    Resp = send_if_enabled(
        search_path(Index), [?JSON_CONTENT_TYPE], <<"POST">>, jiffy:encode(QueryArgs)
    ),
    case Resp of
        {ok, 200, _, RespBody} ->
            {ok, jiffy:decode(RespBody, [return_maps])};
        {ok, 409, _, _} ->
            %% Index was not current enough.
            {error, stale_index};
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

set_update_seq(#index{} = Index, MatchSeq, UpdateSeq) ->
    ReqBody = #{
        match_update_seq => MatchSeq,
        update_seq => UpdateSeq
    },
    set_seq(Index, ReqBody).

set_purge_seq(#index{} = Index, MatchSeq, PurgeSeq) ->
    ReqBody = #{
        match_purge_seq => MatchSeq,
        purge_seq => PurgeSeq
    },
    set_seq(Index, ReqBody).

set_seq(#index{} = Index, ReqBody) ->
    Resp = send_if_enabled(
        index_path(Index), [?JSON_CONTENT_TYPE], <<"POST">>, jiffy:encode(ReqBody)
    ),
    case Resp of
        {ok, 200, _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

%% private functions

index_path(Path) when is_binary(Path) ->
    [<<"/index/">>, couch_util:url_encode(Path)];
index_path(#index{} = Index) ->
    [<<"/index/">>, couch_util:url_encode(nouveau_util:index_name(Index))].

doc_path(#index{} = Index, DocId) ->
    [
        <<"/index/">>,
        couch_util:url_encode(nouveau_util:index_name(Index)),
        <<"/doc/">>,
        couch_util:url_encode(DocId)
    ].

search_path(#index{} = Index) ->
    [index_path(Index), <<"/search">>].

jaxrs_error(400, Body) ->
    {bad_request, message(Body)};
jaxrs_error(404, Body) ->
    {not_found, message(Body)};
jaxrs_error(405, Body) ->
    {method_not_allowed, message(Body)};
jaxrs_error(409, Body) ->
    {conflict, message(Body)};
jaxrs_error(417, Body) ->
    {expectation_failed, message(Body)};
jaxrs_error(422, Body) ->
    {bad_request, lists:join(" and ", errors(Body))};
jaxrs_error(500, Body) ->
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

send_if_enabled(Path, ReqHeaders, Method) ->
    send_if_enabled(Path, ReqHeaders, Method, <<>>).

send_if_enabled(Path, ReqHeaders, Method, ReqBody) ->
    send_if_enabled(Path, ReqHeaders, Method, ReqBody, 5).

send_if_enabled(Path, ReqHeaders, Method, ReqBody, RemainingTries) ->
    case nouveau:enabled() of
        true ->
            case
                gun_pool:request(
                    Method,
                    Path,
                    [nouveau_gun:host_header() | ReqHeaders],
                    ReqBody
                )
            of
                {async, PoolStreamRef} ->
                    Timeout = config:get_integer("nouveau", "request_timeout", 30000),
                    case gun_pool:await(PoolStreamRef, Timeout) of
                        {response, fin, Status, RespHeaders} ->
                            {ok, Status, RespHeaders, []};
                        {response, nofin, Status, RespHeaders} ->
                            case gun_pool:await_body(PoolStreamRef, Timeout) of
                                {ok, RespBody} ->
                                    {ok, Status, RespHeaders, RespBody};
                                {error, Reason} ->
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            {error, Reason}
                    end;
                {error, no_connection_available, _Reason} when RemainingTries > 0 ->
                    timer:sleep(1000),
                    send_if_enabled(Path, ReqHeaders, Method, ReqBody, RemainingTries - 1);
                {error, _Type, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, nouveau_not_enabled}
    end.
