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

-module(chttpd_view).
-include_lib("couch/include/couch_db.hrl").

-export([handle_view_req/3, handle_temp_view_req/2, get_reduce_type/1,
    parse_view_params/3, view_group_etag/2, view_group_etag/3,
    parse_bool_param/1, extract_view_type/3]).

-import(chttpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,send_chunk/2,
    start_json_response/2, start_json_response/3, end_json_response/1,
    send_chunked_error/2]).

design_doc_view(Req, Db, DDoc, ViewName, Keys) ->
    Group = couch_view_group:design_doc_to_view_group(DDoc),
    IsReduce = get_reduce_type(Req),
    ViewType = extract_view_type(ViewName, Group#group.views, IsReduce),
    QueryArgs = parse_view_params(Req, Keys, ViewType),
    % TODO proper calculation of etag
    % Etag = view_group_etag(ViewGroup, Db, Keys),
    Etag = couch_uuids:new(),
    couch_stats_collector:increment({httpd, view_reads}),
    chttpd:etag_respond(Req, Etag, fun() ->
        {ok, Resp} = chttpd:start_json_response(Req, 200, [{"Etag",Etag}]),
        CB = fun view_callback/2,
        fabric:query_view(Db, DDoc, ViewName, CB, {nil, Resp}, QueryArgs)
    end).

view_callback({total_and_offset, Total, Offset}, {nil, Resp}) ->
    Chunk = "{\"total_rows\":~p,\"offset\":~p,\"rows\":[\r\n",
    send_chunk(Resp, io_lib:format(Chunk, [Total, Offset])),
    {ok, {"", Resp}};
view_callback({total_and_offset, _, _}, Acc) ->
    % a sorted=false view where the message came in late.  Ignore.
    {ok, Acc};
view_callback({row, Row}, {nil, Resp}) ->
    % first row of a reduce view, or a sorted=false view
    send_chunk(Resp, ["{\"rows\":[\r\n", ?JSON_ENCODE(Row)]),
    {ok, {",\r\n", Resp}};
view_callback({row, Row}, {Prepend, Resp}) ->
    send_chunk(Resp, [Prepend, ?JSON_ENCODE(Row)]),
    {ok, {",\r\n", Resp}};
view_callback(complete, {nil, Resp}) ->
    send_chunk(Resp, "{\"rows\":[]}"),
    end_json_response(Resp),
    {ok, Resp};
view_callback(complete, {_, Resp}) ->
    send_chunk(Resp, "\r\n]}"),
    end_json_response(Resp),
    {ok, Resp};
view_callback({error, Reason}, {_, Resp}) ->
    chttpd:send_chunked_error(Resp, {error, Reason}).

extract_view_type(_ViewName, [], _IsReduce) ->
    throw({not_found, missing_named_view});
extract_view_type(ViewName, [View|Rest], IsReduce) ->
    case lists:member(ViewName, [Name || {Name, _} <- View#view.reduce_funs]) of
    true ->
        if IsReduce -> reduce; true -> red_map end;
    false ->
        case lists:member(ViewName, View#view.map_names) of
        true -> map;
        false -> extract_view_type(ViewName, Rest, IsReduce)
        end
    end.

handle_view_req(#httpd{method='GET',
        path_parts=[_, _, _, _, ViewName]}=Req, Db, DDoc) ->
    design_doc_view(Req, Db, DDoc, ViewName, nil);

handle_view_req(#httpd{method='POST',
        path_parts=[_, _, _, _, ViewName]}=Req, Db, DDoc) ->
    {Fields} = chttpd:json_body_obj(Req),
    case couch_util:get_value(<<"keys">>, Fields) of
    Keys when is_list(Keys) ->
        design_doc_view(Req, Db, DDoc, ViewName, Keys);
    _ ->
        throw({bad_request, "`keys` body member must be an array."})
    end;

handle_view_req(Req, _Db, _DDoc) ->
    send_method_not_allowed(Req, "GET,POST,HEAD").

handle_temp_view_req(Req, _Db) ->
    Msg = <<"Temporary views are not supported in BigCouch">>,
    chttpd:send_error(Req, 403, forbidden, Msg).

reverse_key_default(?MIN_STR) -> ?MAX_STR;
reverse_key_default(?MAX_STR) -> ?MIN_STR;
reverse_key_default(Key) -> Key.

get_reduce_type(Req) ->
    list_to_atom(chttpd:qs_value(Req, "reduce", "true")).

parse_view_params(Req, Keys, ViewType) ->
    QueryList = chttpd:qs(Req),
    QueryParams =
    lists:foldl(fun({K, V}, Acc) ->
            parse_view_param(K, V) ++ Acc
        end, [], QueryList),
    IsMultiGet = case Keys of
        nil -> false;
        _ -> true
    end,
    Args = #view_query_args{
        view_type=ViewType,
        multi_get=IsMultiGet,
        keys=Keys
    },
    QueryArgs = lists:foldl(fun({K, V}, Args2) ->
        validate_view_query(K, V, Args2)
    end, Args, lists:reverse(QueryParams)), % Reverse to match QS order.

    GroupLevel = QueryArgs#view_query_args.group_level,
    case {ViewType, GroupLevel, IsMultiGet} of
        {reduce, exact, true} ->
            QueryArgs;
        {reduce, _, false} ->
            QueryArgs;
        {reduce, _, _} ->
            Msg = <<"Multi-key fetchs for reduce "
                    "view must include `group=true`">>,
            throw({query_parse_error, Msg});
        _ ->
            QueryArgs
    end,
    QueryArgs.

parse_view_param("", _) ->
    [];
parse_view_param("key", Value) ->
    JsonKey = ?JSON_DECODE(Value),
    [{start_key, JsonKey}, {end_key, JsonKey}];
parse_view_param("startkey_docid", Value) ->
    [{start_docid, ?l2b(Value)}];
parse_view_param("endkey_docid", Value) ->
    [{end_docid, ?l2b(Value)}];
parse_view_param("startkey", Value) ->
    [{start_key, ?JSON_DECODE(Value)}];
parse_view_param("endkey", Value) ->
    [{end_key, ?JSON_DECODE(Value)}];
parse_view_param("limit", Value) ->
    [{limit, parse_positive_int_param(Value)}];
parse_view_param("count", _Value) ->
    throw({query_parse_error, <<"Query parameter 'count' is now 'limit'.">>});
parse_view_param("stale", "ok") ->
    [{stale, ok}];
parse_view_param("stale", _Value) ->
    throw({query_parse_error, <<"stale only available as stale=ok">>});
parse_view_param("update", _Value) ->
    throw({query_parse_error, <<"update=false is now stale=ok">>});
parse_view_param("descending", Value) ->
    [{descending, parse_bool_param(Value)}];
parse_view_param("skip", Value) ->
    [{skip, parse_int_param(Value)}];
parse_view_param("group", Value) ->
    case parse_bool_param(Value) of
        true -> [{group_level, exact}];
        false -> [{group_level, 0}]
    end;
parse_view_param("group_level", Value) ->
    [{group_level, parse_positive_int_param(Value)}];
parse_view_param("inclusive_end", Value) ->
    [{inclusive_end, parse_bool_param(Value)}];
parse_view_param("reduce", Value) ->
    [{reduce, parse_bool_param(Value)}];
parse_view_param("include_docs", Value) ->
    [{include_docs, parse_bool_param(Value)}];
parse_view_param("list", Value) ->
    [{list, ?l2b(Value)}];
parse_view_param("callback", _) ->
    []; % Verified in the JSON response functions
parse_view_param("show_total_rows", Value) ->
    [{show_total_rows, parse_bool_param(Value)}];
parse_view_param("sorted", Value) ->
    [{sorted, parse_bool_param(Value)}];
parse_view_param(Key, Value) ->
    [{extra, {Key, Value}}].

validate_view_query(start_key, Value, Args) ->
    case Args#view_query_args.multi_get of
        true ->
            Msg = <<"Query parameter `start_key` is "
                    "not compatiible with multi-get">>,
            throw({query_parse_error, Msg});
        _ ->
            Args#view_query_args{start_key=Value}
    end;
validate_view_query(start_docid, Value, Args) ->
    Args#view_query_args{start_docid=Value};
validate_view_query(end_key, Value, Args) ->
    case Args#view_query_args.multi_get of
        true->
            Msg = <<"Query paramter `end_key` is "
                    "not compatibile with multi-get">>,
            throw({query_parse_error, Msg});
        _ ->
            Args#view_query_args{end_key=Value}
    end;
validate_view_query(end_docid, Value, Args) ->
    Args#view_query_args{end_docid=Value};
validate_view_query(limit, Value, Args) ->
    Args#view_query_args{limit=Value};
validate_view_query(list, Value, Args) ->
    Args#view_query_args{list=Value};
validate_view_query(stale, Value, Args) ->
    Args#view_query_args{stale=Value};
validate_view_query(descending, true, Args) ->
    case Args#view_query_args.direction of
        rev -> Args; % Already reversed
        fwd ->
            Args#view_query_args{
                direction = rev,
                start_docid =
                    reverse_key_default(Args#view_query_args.start_docid),
                end_docid =
                    reverse_key_default(Args#view_query_args.end_docid)
            }
    end;
validate_view_query(descending, false, Args) ->
    Args; % Ignore default condition
validate_view_query(skip, Value, Args) ->
    Args#view_query_args{skip=Value};
validate_view_query(group_level, Value, Args) ->
    case Args#view_query_args.view_type of
        reduce ->
            Args#view_query_args{group_level=Value};
        _ ->
            Msg = <<"Invalid URL parameter 'group' or "
                    " 'group_level' for non-reduce view.">>,
            throw({query_parse_error, Msg})
    end;
validate_view_query(inclusive_end, Value, Args) ->
    Args#view_query_args{inclusive_end=Value};
validate_view_query(reduce, _, Args) ->
    case Args#view_query_args.view_type of
        map ->
            Msg = <<"Invalid URL parameter `reduce` for map view.">>,
            throw({query_parse_error, Msg});
        _ ->
            Args
    end;
validate_view_query(include_docs, true, Args) ->
    case Args#view_query_args.view_type of
        reduce ->
            Msg = <<"Query paramter `include_docs` "
                    "is invalid for reduce views.">>,
            throw({query_parse_error, Msg});
        _ ->
            Args#view_query_args{include_docs=true}
    end;
validate_view_query(include_docs, _Value, Args) ->
    Args;
validate_view_query(sorted, false, Args) ->
    Args#view_query_args{sorted=false};
validate_view_query(sorted, _Value, Args) ->
    Args;
validate_view_query(extra, _Value, Args) ->
    Args.

view_group_etag(Group, Db) ->
    view_group_etag(Group, Db, nil).

view_group_etag(#group{sig=Sig,current_seq=CurrentSeq}, _Db, Extra) ->
    % ?LOG_ERROR("Group ~p",[Group]),
    % This is not as granular as it could be.
    % If there are updates to the db that do not effect the view index,
    % they will change the Etag. For more granular Etags we'd need to keep
    % track of the last Db seq that caused an index change.
    chttpd:make_etag({Sig, CurrentSeq, Extra}).

parse_bool_param("true") -> true;
parse_bool_param("false") -> false;
parse_bool_param(Val) ->
    Msg = io_lib:format("Invalid value for boolean paramter: ~p", [Val]),
    throw({query_parse_error, ?l2b(Msg)}).

parse_int_param(Val) ->
    case (catch list_to_integer(Val)) of
    IntVal when is_integer(IntVal) ->
        IntVal;
    _ ->
        Msg = io_lib:format("Invalid value for integer parameter: ~p", [Val]),
        throw({query_parse_error, ?l2b(Msg)})
    end.

parse_positive_int_param(Val) ->
    case parse_int_param(Val) of
    IntVal when IntVal >= 0 ->
        IntVal;
    _ ->
        Fmt = "Invalid value for positive integer parameter: ~p",
        Msg = io_lib:format(Fmt, [Val]),
        throw({query_parse_error, ?l2b(Msg)})
    end.
