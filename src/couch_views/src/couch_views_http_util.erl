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

% The reason this module and couch_views_http exist is because they have
% functions which are named the same but do slightly different things. The
% general pattern is chttpd code would call into couch_view_http and those
% function will in turn call into this module.

-module(couch_views_http_util).

-export([
    prepend_val/1,
    parse_body_and_query/2,
    parse_body_and_query/3,
    parse_params/2,
    parse_params/3,
    parse_params/4,
    view_cb/2,
    row_to_obj/1,
    row_to_obj/2,
    row_to_json/1,
    row_to_json/2
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("couch_views/include/couch_views.hrl").

%% these clauses start (and possibly end) the response
view_cb({error, Reason}, #vacc{resp = undefined} = Acc) ->
    {ok, Resp} = chttpd:send_error(Acc#vacc.req, Reason),
    {ok, Acc#vacc{resp = Resp}};
view_cb(complete, #vacc{resp = undefined} = Acc) ->
    % Nothing in view
    {ok, Resp} = chttpd:send_json(Acc#vacc.req, 200, {[{rows, []}]}),
    {ok, Acc#vacc{resp = Resp}};
view_cb(Msg, #vacc{resp = undefined} = Acc) ->
    %% Start response
    Headers = [],
    {ok, Resp} = chttpd:start_delayed_json_response(Acc#vacc.req, 200, Headers),
    view_cb(Msg, Acc#vacc{resp = Resp, should_close = true});
%% ---------------------------------------------------

%% From here on down, the response has been started.

view_cb({error, Reason}, #vacc{resp = Resp} = Acc) ->
    {ok, Resp1} = chttpd:send_delayed_error(Resp, Reason),
    {ok, Acc#vacc{resp = Resp1}};
view_cb(complete, #vacc{resp = Resp, buffer = Buf, threshold = Max} = Acc) ->
    % Finish view output and possibly end the response
    {ok, Resp1} = chttpd:close_delayed_json_object(Resp, Buf, "\r\n]}", Max),
    case Acc#vacc.should_close of
        true ->
            {ok, Resp2} = chttpd:end_delayed_json_response(Resp1),
            {ok, Acc#vacc{resp = Resp2}};
        _ ->
            {ok, Acc#vacc{
                resp = Resp1,
                meta_sent = false,
                row_sent = false,
                prepend = ",\r\n",
                buffer = [],
                bufsize = 0
            }}
    end;
view_cb({meta, Meta}, #vacc{meta_sent = false, row_sent = false} = Acc) ->
    % Sending metadata as we've not sent it or any row yet
    Parts =
        case couch_util:get_value(total, Meta) of
            undefined -> [];
            Total -> [io_lib:format("\"total_rows\":~p", [Total])]
        end ++
            case couch_util:get_value(offset, Meta) of
                undefined -> [];
                Offset -> [io_lib:format("\"offset\":~p", [Offset])]
            end ++
            case couch_util:get_value(update_seq, Meta) of
                undefined ->
                    [];
                null ->
                    ["\"update_seq\":null"];
                UpdateSeq when is_integer(UpdateSeq) ->
                    [io_lib:format("\"update_seq\":~B", [UpdateSeq])];
                UpdateSeq when is_binary(UpdateSeq) ->
                    [io_lib:format("\"update_seq\":\"~s\"", [UpdateSeq])]
            end ++ ["\"rows\":["],
    Chunk = [prepend_val(Acc), "{", string:join(Parts, ","), "\r\n"],
    {ok, AccOut} = maybe_flush_response(Acc, Chunk, iolist_size(Chunk)),
    {ok, AccOut#vacc{prepend = "", meta_sent = true}};
view_cb({meta, _Meta}, #vacc{} = Acc) ->
    %% ignore metadata
    {ok, Acc};
view_cb({row, Row}, #vacc{meta_sent = false} = Acc) ->
    %% sorted=false and row arrived before meta
    % Adding another row
    Chunk = [prepend_val(Acc), "{\"rows\":[\r\n", row_to_json(Row)],
    maybe_flush_response(Acc#vacc{meta_sent = true, row_sent = true}, Chunk, iolist_size(Chunk));
view_cb({row, Row}, #vacc{meta_sent = true} = Acc) ->
    % Adding another row
    Chunk = [prepend_val(Acc), row_to_json(Row)],
    maybe_flush_response(Acc#vacc{row_sent = true}, Chunk, iolist_size(Chunk)).

maybe_flush_response(#vacc{bufsize = Size, threshold = Max} = Acc, Data, Len) when
    Size > 0 andalso (Size + Len) > Max
->
    #vacc{buffer = Buffer, resp = Resp} = Acc,
    {ok, R1} = chttpd:send_delayed_chunk(Resp, Buffer),
    {ok, Acc#vacc{prepend = ",\r\n", buffer = Data, bufsize = Len, resp = R1}};
maybe_flush_response(Acc0, Data, Len) ->
    #vacc{buffer = Buf, bufsize = Size} = Acc0,
    Acc = Acc0#vacc{
        prepend = ",\r\n",
        buffer = [Buf | Data],
        bufsize = Size + Len
    },
    {ok, Acc}.

prepend_val(#vacc{prepend = Prepend}) ->
    case Prepend of
        undefined ->
            "";
        _ ->
            Prepend
    end.

row_to_json(Row) ->
    ?JSON_ENCODE(row_to_obj(Row)).

row_to_json(Kind, Row) ->
    ?JSON_ENCODE(row_to_obj(Kind, Row)).

row_to_obj(Row) ->
    Id = couch_util:get_value(id, Row),
    row_to_obj(Id, Row).

row_to_obj(error, Row) ->
    % Special case for _all_docs request with KEYS to
    % match prior behavior.
    Key = couch_util:get_value(key, Row),
    Val = couch_util:get_value(value, Row),
    Reason = couch_util:get_value(reason, Row),
    ReasonProp =
        if
            Reason == undefined -> [];
            true -> [{reason, Reason}]
        end,
    {[{key, Key}, {error, Val}] ++ ReasonProp};
row_to_obj(Id0, Row) ->
    Id =
        case Id0 of
            undefined -> [];
            Id0 -> [{id, Id0}]
        end,
    Key = couch_util:get_value(key, Row, null),
    Val = couch_util:get_value(value, Row),
    Doc =
        case couch_util:get_value(doc, Row) of
            undefined -> [];
            Doc0 -> [{doc, Doc0}]
        end,
    {Id ++ [{key, Key}, {value, Val}] ++ Doc}.

parse_params(#httpd{} = Req, Keys) ->
    parse_params(chttpd:qs(Req), Keys);
parse_params(Props, Keys) ->
    Args = #mrargs{},
    parse_params(Props, Keys, Args).

parse_params(Props, Keys, Args) ->
    parse_params(Props, Keys, Args, []).

parse_params(Props, Keys, #mrargs{} = Args0, Options) ->
    IsDecoded = lists:member(decoded, Options),
    Args1 =
        case lists:member(keep_group_level, Options) of
            true ->
                Args0;
            _ ->
                % group_level set to undefined to detect if explicitly set by user
                Args0#mrargs{keys = Keys, group = undefined, group_level = undefined}
        end,
    lists:foldl(
        fun({K, V}, Acc) ->
            parse_param(K, V, Acc, IsDecoded)
        end,
        Args1,
        Props
    ).

parse_body_and_query(#httpd{method = 'POST'} = Req, Keys) ->
    Props = chttpd:json_body_obj(Req),
    parse_body_and_query(Req, Props, Keys);
parse_body_and_query(Req, Keys) ->
    parse_params(
        chttpd:qs(Req),
        Keys,
        #mrargs{
            keys = Keys,
            group = undefined,
            group_level = undefined
        },
        [keep_group_level]
    ).

parse_body_and_query(Req, {Props}, Keys) ->
    Args = #mrargs{keys = Keys, group = undefined, group_level = undefined},
    BodyArgs = parse_params(Props, Keys, Args, [decoded]),
    parse_params(chttpd:qs(Req), Keys, BodyArgs, [keep_group_level]).

parse_param(Key, Val, Args, IsDecoded) when is_binary(Key) ->
    parse_param(binary_to_list(Key), Val, Args, IsDecoded);
parse_param(Key, Val, Args, IsDecoded) ->
    case Key of
        "" ->
            Args;
        "reduce" ->
            Args#mrargs{reduce = parse_boolean(Val)};
        "key" when IsDecoded ->
            Args#mrargs{start_key = Val, end_key = Val};
        "key" ->
            JsonKey = ?JSON_DECODE(Val),
            Args#mrargs{start_key = JsonKey, end_key = JsonKey};
        "keys" when IsDecoded ->
            Args#mrargs{keys = Val};
        "keys" ->
            Args#mrargs{keys = ?JSON_DECODE(Val)};
        "startkey" when IsDecoded ->
            Args#mrargs{start_key = Val};
        "start_key" when IsDecoded ->
            Args#mrargs{start_key = Val};
        "startkey" ->
            Args#mrargs{start_key = ?JSON_DECODE(Val)};
        "start_key" ->
            Args#mrargs{start_key = ?JSON_DECODE(Val)};
        "startkey_docid" ->
            Args#mrargs{start_key_docid = couch_util:to_binary(Val)};
        "start_key_doc_id" ->
            Args#mrargs{start_key_docid = couch_util:to_binary(Val)};
        "endkey" when IsDecoded ->
            Args#mrargs{end_key = Val};
        "end_key" when IsDecoded ->
            Args#mrargs{end_key = Val};
        "endkey" ->
            Args#mrargs{end_key = ?JSON_DECODE(Val)};
        "end_key" ->
            Args#mrargs{end_key = ?JSON_DECODE(Val)};
        "endkey_docid" ->
            Args#mrargs{end_key_docid = couch_util:to_binary(Val)};
        "end_key_doc_id" ->
            Args#mrargs{end_key_docid = couch_util:to_binary(Val)};
        "limit" ->
            Args#mrargs{limit = parse_pos_int(Val)};
        "page_size" ->
            Args#mrargs{page_size = parse_pos_int(Val)};
        "stale" when Val == "ok" orelse Val == <<"ok">> ->
            Args#mrargs{stable = true, update = false};
        "stale" when Val == "update_after" orelse Val == <<"update_after">> ->
            Args#mrargs{stable = true, update = lazy};
        "stale" ->
            throw({query_parse_error, <<"Invalid value for `stale`.">>});
        "stable" when Val == "true" orelse Val == <<"true">> orelse Val == true ->
            Args#mrargs{stable = true};
        "stable" when Val == "false" orelse Val == <<"false">> orelse Val == false ->
            Args#mrargs{stable = false};
        "stable" ->
            throw({query_parse_error, <<"Invalid value for `stable`.">>});
        "update" when Val == "true" orelse Val == <<"true">> orelse Val == true ->
            Args#mrargs{update = true};
        "update" when Val == "false" orelse Val == <<"false">> orelse Val == false ->
            Args#mrargs{update = false};
        "update" when Val == "lazy" orelse Val == <<"lazy">> ->
            Args#mrargs{update = lazy};
        "update" ->
            throw({query_parse_error, <<"Invalid value for `update`.">>});
        "descending" ->
            case parse_boolean(Val) of
                true -> Args#mrargs{direction = rev};
                _ -> Args#mrargs{direction = fwd}
            end;
        "skip" ->
            Args#mrargs{skip = parse_pos_int(Val)};
        "group" ->
            Args#mrargs{group = parse_boolean(Val)};
        "group_level" ->
            Args#mrargs{group_level = parse_pos_int(Val)};
        "inclusive_end" ->
            Args#mrargs{inclusive_end = parse_boolean(Val)};
        "include_docs" ->
            Args#mrargs{include_docs = parse_boolean(Val)};
        "attachments" ->
            case parse_boolean(Val) of
                true ->
                    Opts = Args#mrargs.doc_options,
                    Args#mrargs{doc_options = [attachments | Opts]};
                false ->
                    Args
            end;
        "att_encoding_info" ->
            case parse_boolean(Val) of
                true ->
                    Opts = Args#mrargs.doc_options,
                    Args#mrargs{doc_options = [att_encoding_info | Opts]};
                false ->
                    Args
            end;
        "update_seq" ->
            Args#mrargs{update_seq = parse_boolean(Val)};
        "conflicts" ->
            Args#mrargs{conflicts = parse_boolean(Val)};
        "callback" ->
            Args#mrargs{callback = couch_util:to_binary(Val)};
        "sorted" ->
            Args#mrargs{sorted = parse_boolean(Val)};
        "partition" ->
            Partition = couch_util:to_binary(Val),
            couch_partition:validate_partition(Partition),
            couch_views_util:set_extra(Args, partition, Partition);
        _ ->
            BKey = couch_util:to_binary(Key),
            BVal = couch_util:to_binary(Val),
            Args#mrargs{extra = [{BKey, BVal} | Args#mrargs.extra]}
    end.

parse_boolean(Val) ->
    case couch_lib_parse:parse_boolean(Val) of
        {error, Reason} ->
            throw({query_parse_error, Reason});
        Boolean ->
            Boolean
    end.

parse_pos_int(Val) ->
    case couch_lib_parse:parse_non_neg_integer(Val) of
        {error, Reason} ->
            throw({query_parse_error, Reason});
        Int ->
            Int
    end.
