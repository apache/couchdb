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
    start_update/1,
    end_update/1,
    search/2,
    set_purge_seq/3,
    set_update_seq/3,
    supported_lucene_versions/0,
    jaxrs_error/2
]).

-define(JSON_CONTENT_TYPE, {"Content-Type", "application/json"}).
-define(JSON_SEQ_CONTENT_TYPE, {"Content-Type", "application/json-seq"}).

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

delete_doc({_, _} = PoolStreamRef, DocId, MatchSeq, UpdateSeq) when
    is_binary(DocId),
    is_integer(MatchSeq),
    MatchSeq >= 0,
    is_integer(UpdateSeq),
    UpdateSeq > 0
->
    Row = #{
        <<"@type">> => delete,
        doc_id => DocId,
        match_seq => MatchSeq,
        seq => UpdateSeq,
        purge => false
    },
    ok = gun_pool:data(PoolStreamRef, nofin, encode_json_seq(Row)),
    check_status(PoolStreamRef).

purge_doc({_, _} = PoolStreamRef, DocId, MatchSeq, PurgeSeq) when
    is_binary(DocId),
    is_integer(MatchSeq),
    MatchSeq >= 0,
    is_integer(PurgeSeq),
    PurgeSeq > 0
->
    Row = #{
        <<"@type">> => delete,
        doc_id => DocId,
        match_seq => MatchSeq,
        seq => PurgeSeq,
        purge => true
    },
    ok = gun_pool:data(PoolStreamRef, nofin, encode_json_seq(Row)),
    check_status(PoolStreamRef).

start_update(#index{} = Index) ->
    case nouveau:enabled() of
        true ->
            gun_pool:post(
                update_path(Index),
                [nouveau_gun:host_header(), ?JSON_SEQ_CONTENT_TYPE]
            );
        false ->
            {error, nouveau_not_enabled}
    end.

end_update({_, _} = PoolStreamRef) ->
    ok = gun_pool:data(PoolStreamRef, fin, <<>>),
    case await(PoolStreamRef) of
        {ok, 200, _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.

update_doc({_, _} = PoolStreamRef, DocId, MatchSeq, UpdateSeq, Partition, Fields) when
    is_binary(DocId),
    is_integer(MatchSeq),
    MatchSeq >= 0,
    is_integer(UpdateSeq),
    UpdateSeq > 0,
    (is_binary(Partition) orelse Partition == null),
    is_list(Fields)
->
    Row = #{
        <<"@type">> => update,
        doc_id => DocId,
        match_seq => MatchSeq,
        seq => UpdateSeq,
        partition => Partition,
        fields => Fields
    },
    ok = gun_pool:data(PoolStreamRef, nofin, encode_json_seq(Row)),
    check_status(PoolStreamRef).

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

set_update_seq({_, _} = PoolStreamRef, MatchSeq, UpdateSeq) ->
    Row = #{
        <<"@type">> => index_info,
        match_update_seq => MatchSeq,
        update_seq => UpdateSeq
    },
    ok = gun_pool:data(PoolStreamRef, nofin, encode_json_seq(Row)),
    check_status(PoolStreamRef).

set_purge_seq({_, _} = PoolStreamRef, MatchSeq, PurgeSeq) ->
    Row = #{
        <<"@type">> => index_info,
        match_purge_seq => MatchSeq,
        purge_seq => PurgeSeq
    },
    ok = gun_pool:data(PoolStreamRef, nofin, encode_json_seq(Row)),
    check_status(PoolStreamRef).

supported_lucene_versions() ->
    Resp = send_if_enabled(<<"/">>, [], <<"GET">>),
    case Resp of
        {ok, 200, _, RespBody} ->
            Json = jiffy:decode(RespBody, [return_maps]),
            {ok, maps:get(<<"supported_lucene_versions">>, Json, [])};
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

search_path(#index{} = Index) ->
    [index_path(Index), <<"/search">>].

update_path(#index{} = Index) ->
    [index_path(Index), <<"/update">>].

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
                    await(PoolStreamRef);
                {error, no_connection_available, _Reason} when RemainingTries > 0 ->
                    timer:sleep(1000),
                    send_if_enabled(Path, ReqHeaders, Method, ReqBody, RemainingTries - 1);
                {error, _Type, Reason} ->
                    {error, Reason}
            end;
        false ->
            {error, nouveau_not_enabled}
    end.

await(PoolStreamRef) ->
    Timeout = config:get_integer("nouveau", "request_timeout", 30000),
    await(PoolStreamRef, Timeout).

await({ConnPid, _} = PoolStreamRef, Timeout) ->
    MRef = monitor(process, ConnPid),
    T0 = now_ms(),
    Res =
        case gun_pool:await(PoolStreamRef, Timeout, MRef) of
            {response, fin, Status, RespHeaders} ->
                {ok, Status, RespHeaders, []};
            {response, nofin, Status, RespHeaders} ->
                Elapsed = now_ms() - T0,
                case gun_pool:await_body(PoolStreamRef, max(0, Timeout - Elapsed), MRef) of
                    {ok, RespBody} ->
                        {ok, Status, RespHeaders, RespBody};
                    {'DOWN', MRef, process, ConnPid, Reason} ->
                        {error, Reason};
                    {error, Reason} ->
                        {error, Reason}
                end;
            {'DOWN', MRef, process, ConnPid, Reason} ->
                {error, Reason};
            {error, Reason} ->
                {error, Reason}
        end,
    demonitor(MRef, [flush]),
    Res.

now_ms() ->
    erlang:monotonic_time(millisecond).

encode_json_seq(Data) ->
    [$\x{1e}, jiffy:encode(Data), $\n].

check_status({_, _} = PoolStreamRef) ->
    case await(PoolStreamRef, 0) of
        {error, timeout} ->
            ok;
        {ok, 200, _, _} ->
            ok;
        {ok, StatusCode, _, RespBody} ->
            {error, jaxrs_error(StatusCode, RespBody)};
        {error, Reason} ->
            send_error(Reason)
    end.
