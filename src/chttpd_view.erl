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

-export([handle_view_req/3,handle_temp_view_req/2]).

-export([get_stale_type/1, get_reduce_type/1, parse_view_params/3]).
-export([make_view_fold_fun/6, finish_view_fold/3, view_row_obj/3]).
-export([view_group_etag/2, view_group_etag/3, make_reduce_fold_funs/5]).
-export([design_doc_view/5, parse_bool_param/1, extract_view_type/3]).

-import(chttpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,send_chunk/2,
    start_json_response/2, start_json_response/3, end_json_response/1,
    send_chunked_error/2]).

design_doc_view(Req, Db, DDoc, ViewName, Keys) ->
    Group = couch_view_group:design_doc_to_view_group(Db, DDoc),
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

handle_temp_view_req(#httpd{method='POST'}=Req, Db) ->
    throw(not_implemented),
    couch_stats_collector:increment({httpd, temporary_view_reads}),
    {Props} = chttpd:json_body_obj(Req),
    Language = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    {DesignOptions} = couch_util:get_value(<<"options">>, Props, {[]}),
    MapSrc = couch_util:get_value(<<"map">>, Props),
    Keys = couch_util:get_value(<<"keys">>, Props, nil),
    case couch_util:get_value(<<"reduce">>, Props, null) of
    null ->
        QueryArgs = parse_view_params(Req, Keys, map),
        {ok, View, Group} = ?COUCH:get_temp_map_view(Db, Language,
            DesignOptions, MapSrc),
        output_map_view(Req, View, Group, Db, QueryArgs, Keys);
    RedSrc ->
        QueryArgs = parse_view_params(Req, Keys, reduce),
        {ok, View, Group} = ?COUCH:get_temp_reduce_view(Db, Language,
            DesignOptions, MapSrc, RedSrc),
        output_reduce_view(Req, Db, View, Group, QueryArgs, Keys)
    end;

handle_temp_view_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

output_map_view(Req, View, Group, Db, QueryArgs, nil) ->
    #view_query_args{
        limit = Limit,
        direction = Dir,
        skip = SkipCount,
        start_key = StartKey,
        start_docid = StartDocId
    } = QueryArgs,
    CurrentEtag = view_group_etag(Group, Db),
    chttpd:etag_respond(Req, CurrentEtag, fun() ->
        {ok, RowCount} = ?COUCH:get_row_count(View),
        Start = {StartKey, StartDocId},
        FoldlFun = make_view_fold_fun(Req, QueryArgs, CurrentEtag, Db, RowCount, #view_fold_helper_funs{reduce_count=fun ?COUCH:reduce_to_count/1}),
        FoldAccInit = {Limit, SkipCount, undefined, [], nil},
        FoldResult = ?COUCH:view_fold(View, Start, Dir, FoldlFun, FoldAccInit),
        finish_view_fold(Req, RowCount, FoldResult)
    end);

output_map_view(Req, View, Group, Db, QueryArgs, Keys) ->
    #view_query_args{
        limit = Limit,
        direction = Dir,
        skip = SkipCount,
        start_docid = StartDocId
    } = QueryArgs,
    CurrentEtag = view_group_etag(Group, Db, Keys),
    chttpd:etag_respond(Req, CurrentEtag, fun() ->
        {ok, RowCount} = ?COUCH:get_row_count(View),
        FoldAccInit = {Limit, SkipCount, undefined, [], nil},
        FoldResult = lists:foldl(
            fun(Key, {ok, FoldAcc}) ->
                Start = {Key, StartDocId},
                FoldlFun = make_view_fold_fun(Req,
                    QueryArgs#view_query_args{
                        start_key = Key,
                        end_key = Key
                    }, CurrentEtag, Db, RowCount,
                    #view_fold_helper_funs{
                        reduce_count = fun ?COUCH:reduce_to_count/1
                    }),
                ?COUCH:view_fold(View, Start, Dir, FoldlFun, FoldAcc)
            end, {ok, FoldAccInit}, Keys),
        finish_view_fold(Req, RowCount, FoldResult)
    end).

output_reduce_view(Req, Db, View, Group, QueryArgs, nil) ->
    #view_query_args{
        start_key = StartKey,
        end_key = EndKey,
        limit = Limit,
        skip = Skip,
        direction = Dir,
        start_docid = StartDocId,
        end_docid = EndDocId,
        group_level = GroupLevel
    } = QueryArgs,
    CurrentEtag = view_group_etag(Group, Db),
    chttpd:etag_respond(Req, CurrentEtag, fun() ->
        {ok, GroupRowsFun, RespFun} = make_reduce_fold_funs(Req, GroupLevel, QueryArgs, CurrentEtag, #reduce_fold_helper_funs{}),
        FoldAccInit = {Limit, Skip, undefined, []},
        {ok, {_, _, Resp, _}} = ?COUCH:view_fold_reduce(View, Dir, {StartKey, StartDocId},
            {EndKey, EndDocId}, GroupRowsFun, RespFun, FoldAccInit),
        finish_reduce_fold(Req, Resp)
    end);

output_reduce_view(Req, Db, View, Group, QueryArgs, Keys) ->
    #view_query_args{
        limit = Limit,
        skip = Skip,
        direction = Dir,
        start_docid = StartDocId,
        end_docid = EndDocId,
        group_level = GroupLevel
    } = QueryArgs,
    CurrentEtag = view_group_etag(Group, Db),
    chttpd:etag_respond(Req, CurrentEtag, fun() ->
        {ok, GroupRowsFun, RespFun} = make_reduce_fold_funs(Req, GroupLevel, QueryArgs, CurrentEtag, #reduce_fold_helper_funs{}),
        {Resp, _RedAcc3} = lists:foldl(
            fun(Key, {Resp, RedAcc}) ->
                % run the reduce once for each key in keys, with limit etc reapplied for each key
                FoldAccInit = {Limit, Skip, Resp, RedAcc},
                {_, {_, _, Resp2, RedAcc2}} = ?COUCH:view_fold_reduce(View, Dir, {Key, StartDocId},
                    {Key, EndDocId}, GroupRowsFun, RespFun, FoldAccInit),
                % Switch to comma
                {Resp2, RedAcc2}
            end,
        {undefined, []}, Keys), % Start with no comma
        finish_reduce_fold(Req, Resp)
    end).

reverse_key_default(?MIN_STR) -> ?MAX_STR;
reverse_key_default(?MAX_STR) -> ?MIN_STR;
reverse_key_default(Key) -> Key.

get_stale_type(Req) ->
    list_to_atom(chttpd:qs_value(Req, "stale", "nil")).

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
validate_view_query(show_total_rows, false, Args) ->
    Args#view_query_args{show_total_rows=false};
validate_view_query(show_total_rows, _Value, Args) ->
    Args;
validate_view_query(sorted, false, Args) ->
    Args#view_query_args{sorted=false};
validate_view_query(sorted, _Value, Args) ->
    Args;
validate_view_query(extra, _Value, Args) ->
    Args.

make_view_fold_fun(Req, QueryArgs, Etag, Db, TotalViewCount, HelperFuns) ->
    #view_query_args{
        end_key = EndKey,
        end_docid = EndDocId,
        inclusive_end = InclusiveEnd,
        direction = Dir
    } = QueryArgs,

    #view_fold_helper_funs{
        passed_end = PassedEndFun,
        start_response = StartRespFun,
        send_row = SendRowFun,
        reduce_count = ReduceCountFun
    } = apply_default_helper_funs(HelperFuns,
        {Dir, EndKey, EndDocId, InclusiveEnd}),

    #view_query_args{
        include_docs = IncludeDocs
    } = QueryArgs,

    fun({{Key, DocId}, Value}, OffsetReds, {AccLimit, AccSkip, Resp, RowFunAcc,
                                            OffsetAcc}) ->
        PassedEnd = PassedEndFun(Key, DocId),
        case {PassedEnd, AccLimit, AccSkip, Resp} of
        {true, _, _, _} ->
            % The stop key has been passed, stop looping.
            % We may need offset so calcluate it here.
            % Checking Resp is an optimization that tells
            % us its already been calculated (and sent).
            NewOffset = case Resp of
                undefined -> ReduceCountFun(OffsetReds);
                _ -> nil
            end,
            {stop, {AccLimit, AccSkip, Resp, RowFunAcc, NewOffset}};
        {_, 0, _, _} ->
            % we've done "limit" rows, stop foldling
            {stop, {0, 0, Resp, RowFunAcc, OffsetAcc}};
        {_, _, AccSkip, _} when AccSkip > 0 ->
            % just keep skipping
            {ok, {AccLimit, AccSkip - 1, Resp, RowFunAcc, OffsetAcc}};
        {_, _, _, undefined} ->
            % rendering the first row, first we start the response
            Offset = ReduceCountFun(OffsetReds),
            {ok, Resp2, RowFunAcc0} = StartRespFun(Req, Etag,
                TotalViewCount, Offset, RowFunAcc),
            {Go, RowFunAcc2} = SendRowFun(Resp2, Db, {{Key, DocId}, Value},
                IncludeDocs, RowFunAcc0),
            {Go, {AccLimit - 1, 0, Resp2, RowFunAcc2, Offset}};
        {_, AccLimit, _, Resp} when (AccLimit > 0) ->
            % rendering all other rows
            {Go, RowFunAcc2} = SendRowFun(Resp, Db, {{Key, DocId}, Value},
                IncludeDocs, RowFunAcc),
            {Go, {AccLimit - 1, 0, Resp, RowFunAcc2, OffsetAcc}}
        end
    end.

make_reduce_fold_funs(Req, GroupLevel, _QueryArgs, Etag, HelperFuns) ->
    #reduce_fold_helper_funs{
        start_response = StartRespFun,
        send_row = SendRowFun
    } = apply_default_helper_funs(HelperFuns),

    GroupRowsFun =
        fun({_Key1,_}, {_Key2,_}) when GroupLevel == 0 ->
            true;
        ({Key1,_}, {Key2,_})
                when is_integer(GroupLevel) and is_list(Key1) and is_list(Key2) ->
            lists:sublist(Key1, GroupLevel) == lists:sublist(Key2, GroupLevel);
        ({Key1,_}, {Key2,_}) ->
            Key1 == Key2
        end,

    RespFun = fun
    (_Key, _Red, {AccLimit, AccSkip, Resp, RowAcc}) when AccSkip > 0 ->
        % keep skipping
        {ok, {AccLimit, AccSkip - 1, Resp, RowAcc}};
    (_Key, _Red, {0, _AccSkip, Resp, RowAcc}) ->
        % we've exhausted limit rows, stop
        {stop, {0, _AccSkip, Resp, RowAcc}};

    (_Key, Red, {AccLimit, 0, undefined, RowAcc0}) when GroupLevel == 0 ->
        % we haven't started responding yet and group=false
        {ok, Resp2, RowAcc} = StartRespFun(Req, Etag, RowAcc0),
        {Go, RowAcc2} = SendRowFun(Resp2, {null, Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp2, RowAcc2}};
    (_Key, Red, {AccLimit, 0, Resp, RowAcc}) when GroupLevel == 0 ->
        % group=false but we've already started the response
        {Go, RowAcc2} = SendRowFun(Resp, {null, Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp, RowAcc2}};

    (Key, Red, {AccLimit, 0, undefined, RowAcc0})
            when is_integer(GroupLevel), is_list(Key) ->
        % group_level and we haven't responded yet
        {ok, Resp2, RowAcc} = StartRespFun(Req, Etag, RowAcc0),
        {Go, RowAcc2} = SendRowFun(Resp2, {lists:sublist(Key, GroupLevel), Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp2, RowAcc2}};
    (Key, Red, {AccLimit, 0, Resp, RowAcc})
            when is_integer(GroupLevel), is_list(Key) ->
        % group_level and we've already started the response
        {Go, RowAcc2} = SendRowFun(Resp, {lists:sublist(Key, GroupLevel), Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp, RowAcc2}};

    (Key, Red, {AccLimit, 0, undefined, RowAcc0}) ->
        % group=true and we haven't responded yet
        {ok, Resp2, RowAcc} = StartRespFun(Req, Etag, RowAcc0),
        {Go, RowAcc2} = SendRowFun(Resp2, {Key, Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp2, RowAcc2}};
    (Key, Red, {AccLimit, 0, Resp, RowAcc}) ->
        % group=true and we've already started the response
        {Go, RowAcc2} = SendRowFun(Resp, {Key, Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp, RowAcc2}}
    end,
    {ok, GroupRowsFun, RespFun}.

apply_default_helper_funs(#view_fold_helper_funs{
    passed_end = PassedEnd,
    start_response = StartResp,
    send_row = SendRow
}=Helpers, {Dir, EndKey, EndDocId, InclusiveEnd}) ->
    PassedEnd2 = case PassedEnd of
    undefined -> make_passed_end_fun(Dir, EndKey, EndDocId, InclusiveEnd);
    _ -> PassedEnd
    end,

    StartResp2 = case StartResp of
    undefined -> fun json_view_start_resp/5;
    _ -> StartResp
    end,

    SendRow2 = case SendRow of
    undefined -> fun send_json_view_row/5;
    _ -> SendRow
    end,

    Helpers#view_fold_helper_funs{
        passed_end = PassedEnd2,
        start_response = StartResp2,
        send_row = SendRow2
    }.

apply_default_helper_funs(#reduce_fold_helper_funs{
    start_response = StartResp,
    send_row = SendRow
}=Helpers) ->
    StartResp2 = case StartResp of
    undefined -> fun json_reduce_start_resp/3;
    _ -> StartResp
    end,

    SendRow2 = case SendRow of
    undefined -> fun send_json_reduce_row/3;
    _ -> SendRow
    end,

    Helpers#reduce_fold_helper_funs{
        start_response = StartResp2,
        send_row = SendRow2
    }.

make_passed_end_fun(fwd, EndKey, EndDocId, InclusiveEnd) ->
    case InclusiveEnd of
    true ->
        fun(ViewKey, ViewId) ->
            couch_view:less_json([EndKey, EndDocId], [ViewKey, ViewId])
        end;
    false ->
        fun
            (ViewKey, _ViewId) when ViewKey == EndKey ->
                true;
            (ViewKey, ViewId) ->
                couch_view:less_json([EndKey, EndDocId], [ViewKey, ViewId])
        end
    end;

make_passed_end_fun(rev, EndKey, EndDocId, InclusiveEnd) ->
    case InclusiveEnd of
    true ->
        fun(ViewKey, ViewId) ->
            couch_view:less_json([ViewKey, ViewId], [EndKey, EndDocId])
        end;
    false->
        fun
            (ViewKey, _ViewId) when ViewKey == EndKey ->
                true;
            (ViewKey, ViewId) ->
                couch_view:less_json([ViewKey, ViewId], [EndKey, EndDocId])
        end
    end.

json_view_start_resp(Req, Etag, TotalViewCount, Offset, _Acc) ->
    {ok, Resp} = start_json_response(Req, 200, [{"Etag", Etag}]),
    BeginBody = io_lib:format("{\"total_rows\":~w,\"offset\":~w,\"rows\":[\r\n",
            [TotalViewCount, Offset]),
    {ok, Resp, BeginBody}.

send_json_view_row(Resp, Db, {{Key, DocId}, Value}, IncludeDocs, RowFront) ->
    JsonObj = view_row_obj(Db, {{Key, DocId}, Value}, IncludeDocs),
    send_chunk(Resp, RowFront ++  ?JSON_ENCODE(JsonObj)),
    {ok, ",\r\n"}.

json_reduce_start_resp(Req, Etag, _Acc0) ->
    {ok, Resp} = start_json_response(Req, 200, [{"Etag", Etag}]),
    {ok, Resp, "{\"rows\":[\r\n"}.

send_json_reduce_row(Resp, {Key, Value}, RowFront) ->
    send_chunk(Resp, RowFront ++ ?JSON_ENCODE({[{key, Key}, {value, Value}]})),
    {ok, ",\r\n"}.

view_group_etag(Group, Db) ->
    view_group_etag(Group, Db, nil).

view_group_etag(#group{sig=Sig,current_seq=CurrentSeq}, _Db, Extra) ->
    % ?LOG_ERROR("Group ~p",[Group]),
    % This is not as granular as it could be.
    % If there are updates to the db that do not effect the view index,
    % they will change the Etag. For more granular Etags we'd need to keep
    % track of the last Db seq that caused an index change.
    chttpd:make_etag({Sig, CurrentSeq, Extra}).

% the view row has an error
view_row_obj(_Db, {{Key, error}, Value}, _IncludeDocs) ->
    {[{key, Key}, {error, Value}]};
% include docs in the view output
view_row_obj(Db, {{Key, DocId}, {Props}}, true) ->
    Rev = case couch_util:get_value(<<"_rev">>, Props) of
    undefined ->
        nil;
    Rev0 ->
        couch_doc:parse_rev(Rev0)
    end,
    view_row_with_doc(Db, {{Key, DocId}, {Props}}, Rev);
view_row_obj(Db, {{Key, DocId}, Value}, true) ->
    view_row_with_doc(Db, {{Key, DocId}, Value}, nil);
% the normal case for rendering a view row
view_row_obj(_Db, {{Key, DocId}, Value}, _IncludeDocs) ->
    {[{id, DocId}, {key, Key}, {value, Value}]}.

view_row_with_doc(Db, {{Key, DocId}, Value}, Rev) ->
    ?LOG_DEBUG("Include Doc: ~p ~p", [DocId, Rev]),
    case (catch chttpd_db:couch_doc_open(Db, DocId, Rev, [])) of
      {{not_found, missing}, _RevId} ->
          {[{id, DocId}, {key, Key}, {value, Value}, {error, missing}]};
      {not_found, missing} ->
          {[{id, DocId}, {key, Key}, {value, Value}, {error, missing}]};
      {not_found, deleted} ->
          {[{id, DocId}, {key, Key}, {value, Value}]};
      Doc ->
        JsonDoc = couch_doc:to_json_obj(Doc, []),
        {[{id, DocId}, {key, Key}, {value, Value}, {doc, JsonDoc}]}
    end.

finish_view_fold(Req, TotalRows, FoldResult) ->
    case FoldResult of
    {ok, {_, _, undefined, _, Offset}} ->
        % nothing found in the view, nothing has been returned
        % send empty view
        NewOffset = case Offset of
            nil -> TotalRows;
            _ -> Offset
        end,
        send_json(Req, 200, {[
            {total_rows, TotalRows},
            {offset, NewOffset},
            {rows, []}
        ]});
    {ok, {_, _, Resp, _, _}} ->
        % end the view
        send_chunk(Resp, "\r\n]}"),
        end_json_response(Resp);
    Error ->
        throw(Error)
    end.

finish_reduce_fold(Req, Resp) ->
    case Resp of
    undefined ->
        send_json(Req, 200, {[
            {rows, []}
        ]});
    Resp ->
        send_chunk(Resp, "\r\n]}"),
        end_json_response(Resp)
    end.

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
