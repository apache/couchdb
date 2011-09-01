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

-module(couch_httpd_view).
-include("couch_db.hrl").

-export([handle_view_req/3,handle_temp_view_req/2]).

-export([parse_view_params/4]).
-export([make_view_fold_fun/7, finish_view_fold/4, finish_view_fold/5, view_row_obj/4]).
-export([view_etag/5, make_reduce_fold_funs/6]).
-export([design_doc_view/5, parse_bool_param/1, doc_member/3]).
-export([make_key_options/1, load_view/4]).

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,send_chunk/2,
    start_json_response/2, start_json_response/3, end_json_response/1,
    send_chunked_error/2]).

-import(couch_db,[get_update_seq/1]).

design_doc_view(Req, Db, DName, ViewName, Keys) ->
    DesignId = <<"_design/", DName/binary>>,
    Stale = get_stale_type(Req),
    Reduce = get_reduce_type(Req),
    Result = case couch_view:get_map_view(Db, DesignId, ViewName, Stale) of
    {ok, View, Group} ->
        QueryArgs = parse_view_params(Req, Keys, map, view_collator(View)),
        output_map_view(Req, View, Group, Db, QueryArgs, Keys);
    {not_found, Reason} ->
        case couch_view:get_reduce_view(Db, DesignId, ViewName, Stale) of
        {ok, ReduceView, Group} ->
            Collator = view_collator(ReduceView),
            case Reduce of
            false ->
                QueryArgs = parse_view_params(Req, Keys, red_map, Collator),
                MapView = couch_view:extract_map_view(ReduceView),
                output_map_view(Req, MapView, Group, Db, QueryArgs, Keys);
            _ ->
                QueryArgs = parse_view_params(Req, Keys, reduce, Collator),
                output_reduce_view(Req, Db, ReduceView, Group, QueryArgs, Keys)
            end;
        _ ->
            throw({not_found, Reason})
        end
    end,
    couch_stats_collector:increment({httpd, view_reads}),
    Result.

handle_view_req(#httpd{method='GET',
        path_parts=[_, _, DName, _, ViewName]}=Req, Db, _DDoc) ->
    Keys = couch_httpd:qs_json_value(Req, "keys", nil),
    design_doc_view(Req, Db, DName, ViewName, Keys);

handle_view_req(#httpd{method='POST',
        path_parts=[_, _, DName, _, ViewName]}=Req, Db, _DDoc) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {Fields} = couch_httpd:json_body_obj(Req),
    case couch_util:get_value(<<"keys">>, Fields, nil) of
    nil ->
        Fmt = "POST to view ~p/~p in database ~p with no keys member.",
        ?LOG_DEBUG(Fmt, [DName, ViewName, Db]),
        design_doc_view(Req, Db, DName, ViewName, nil);
    Keys when is_list(Keys) ->
        design_doc_view(Req, Db, DName, ViewName, Keys);
    _ ->
        throw({bad_request, "`keys` member must be a array."})
    end;

handle_view_req(Req, _Db, _DDoc) ->
    send_method_not_allowed(Req, "GET,POST,HEAD").

handle_temp_view_req(#httpd{method='POST'}=Req, Db) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    ok = couch_db:check_is_admin(Db),
    couch_stats_collector:increment({httpd, temporary_view_reads}),
    {Props} = couch_httpd:json_body_obj(Req),
    Language = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    {DesignOptions} = couch_util:get_value(<<"options">>, Props, {[]}),
    MapSrc = couch_util:get_value(<<"map">>, Props),
    Keys = couch_util:get_value(<<"keys">>, Props, nil),
    Reduce = get_reduce_type(Req),
    case couch_util:get_value(<<"reduce">>, Props, null) of
    null ->
        {ok, View, Group} = couch_view:get_temp_map_view(Db, Language,
            DesignOptions, MapSrc),
        QueryArgs = parse_view_params(Req, Keys, map, view_collator(View)),
        output_map_view(Req, View, Group, Db, QueryArgs, Keys);
    _ when Reduce =:= false ->
        {ok, View, Group} = couch_view:get_temp_map_view(Db, Language,
            DesignOptions, MapSrc),
        QueryArgs = parse_view_params(Req, Keys, red_map, view_collator(View)),
        output_map_view(Req, View, Group, Db, QueryArgs, Keys);
    RedSrc ->
        {ok, View, Group} = couch_view:get_temp_reduce_view(Db, Language,
            DesignOptions, MapSrc, RedSrc),
        QueryArgs = parse_view_params(Req, Keys, reduce, view_collator(View)),
        output_reduce_view(Req, Db, View, Group, QueryArgs, Keys)
    end;

handle_temp_view_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

output_map_view(Req, View, Group, Db, QueryArgs, nil) ->
    #view_query_args{
        limit = Limit,
        skip = SkipCount
    } = QueryArgs,
    CurrentEtag = view_etag(Db, Group, View, QueryArgs),
    couch_httpd:etag_respond(Req, CurrentEtag, fun() ->
        {ok, RowCount} = couch_view:get_row_count(View),
        FoldlFun = make_view_fold_fun(Req, QueryArgs, CurrentEtag, Db, Group#group.current_seq, RowCount, #view_fold_helper_funs{reduce_count=fun couch_view:reduce_to_count/1}),
        FoldAccInit = {Limit, SkipCount, undefined, []},
        {ok, LastReduce, FoldResult} = couch_view:fold(View,
                FoldlFun, FoldAccInit, make_key_options(QueryArgs)),
        finish_view_fold(Req, RowCount,
                couch_view:reduce_to_count(LastReduce), FoldResult)
    end);

output_map_view(Req, View, Group, Db, QueryArgs, Keys) ->
    #view_query_args{
        limit = Limit,
        skip = SkipCount
    } = QueryArgs,
    CurrentEtag = view_etag(Db, Group, View, QueryArgs, Keys),
    couch_httpd:etag_respond(Req, CurrentEtag, fun() ->
        {ok, RowCount} = couch_view:get_row_count(View),
        FoldAccInit = {Limit, SkipCount, undefined, []},
        {LastReduce, FoldResult} = lists:foldl(fun(Key, {_, FoldAcc}) ->
            FoldlFun = make_view_fold_fun(Req, QueryArgs#view_query_args{},
                    CurrentEtag, Db, Group#group.current_seq, RowCount,
                    #view_fold_helper_funs{
                        reduce_count = fun couch_view:reduce_to_count/1
                    }),
            {ok, LastReduce, FoldResult} = couch_view:fold(View, FoldlFun,
                    FoldAcc, make_key_options(
                         QueryArgs#view_query_args{start_key=Key, end_key=Key})),
            {LastReduce, FoldResult}
        end, {{[],[]}, FoldAccInit}, Keys),
        finish_view_fold(Req, RowCount, couch_view:reduce_to_count(LastReduce),
                FoldResult, [{update_seq,Group#group.current_seq}])
    end).

output_reduce_view(Req, Db, View, Group, QueryArgs, nil) ->
    #view_query_args{
        limit = Limit,
        skip = Skip,
        group_level = GroupLevel
    } = QueryArgs,
    CurrentEtag = view_etag(Db, Group, View, QueryArgs),
    couch_httpd:etag_respond(Req, CurrentEtag, fun() ->
        {ok, GroupRowsFun, RespFun} = make_reduce_fold_funs(Req, GroupLevel,
                QueryArgs, CurrentEtag, Group#group.current_seq,
                #reduce_fold_helper_funs{}),
        FoldAccInit = {Limit, Skip, undefined, []},
        {ok, {_, _, Resp, _}} = couch_view:fold_reduce(View,
                RespFun, FoldAccInit, [{key_group_fun, GroupRowsFun} |
                make_key_options(QueryArgs)]),
        finish_reduce_fold(Req, Resp)
    end);

output_reduce_view(Req, Db, View, Group, QueryArgs, Keys) ->
    #view_query_args{
        limit = Limit,
        skip = Skip,
        group_level = GroupLevel
    } = QueryArgs,
    CurrentEtag = view_etag(Db, Group, View, QueryArgs, Keys),
    couch_httpd:etag_respond(Req, CurrentEtag, fun() ->
        {ok, GroupRowsFun, RespFun} = make_reduce_fold_funs(Req, GroupLevel,
                QueryArgs, CurrentEtag, Group#group.current_seq,
                #reduce_fold_helper_funs{}),
        {Resp, _RedAcc3} = lists:foldl(
            fun(Key, {Resp, RedAcc}) ->
                % run the reduce once for each key in keys, with limit etc
                % reapplied for each key
                FoldAccInit = {Limit, Skip, Resp, RedAcc},
                {_, {_, _, Resp2, RedAcc2}} = couch_view:fold_reduce(View,
                        RespFun, FoldAccInit, [{key_group_fun, GroupRowsFun} |
                        make_key_options(QueryArgs#view_query_args{
                            start_key=Key, end_key=Key})]),
                % Switch to comma
                {Resp2, RedAcc2}
            end,
        {undefined, []}, Keys), % Start with no comma
        finish_reduce_fold(Req, Resp, [{update_seq,Group#group.current_seq}])
    end).

reverse_key_default(?MIN_STR) -> ?MAX_STR;
reverse_key_default(?MAX_STR) -> ?MIN_STR;
reverse_key_default(Key) -> Key.

get_stale_type(Req) ->
    list_to_existing_atom(couch_httpd:qs_value(Req, "stale", "nil")).

get_reduce_type(Req) ->
    list_to_existing_atom(couch_httpd:qs_value(Req, "reduce", "true")).

load_view(Req, Db, {ViewDesignId, ViewName}, Keys) ->
    Stale = get_stale_type(Req),
    Reduce = get_reduce_type(Req),
    case couch_view:get_map_view(Db, ViewDesignId, ViewName, Stale) of
    {ok, View, Group} ->
        QueryArgs = parse_view_params(Req, Keys, map, view_collator(View)),
        {map, View, Group, QueryArgs};
    {not_found, _Reason} ->
        case couch_view:get_reduce_view(Db, ViewDesignId, ViewName, Stale) of
        {ok, ReduceView, Group} ->
            Collator = view_collator(ReduceView),
            case Reduce of
            false ->
                QueryArgs = parse_view_params(Req, Keys, map_red, Collator),
                MapView = couch_view:extract_map_view(ReduceView),
                {map, MapView, Group, QueryArgs};
            _ ->
                QueryArgs = parse_view_params(Req, Keys, reduce, Collator),
                {reduce, ReduceView, Group, QueryArgs}
            end;
        {not_found, Reason} ->
            throw({not_found, Reason})
        end
    end.

view_collator({reduce, _N, _Lang, View}) ->
    view_collator(View);

view_collator({temp_reduce, View}) ->
    view_collator(View);

view_collator(#view{btree=Btree}) ->
    % Return an "is-less-than" predicate by calling into the btree's
    % collator. For raw collation, couch_btree compares arbitrary
    % Erlang terms, but for normal (ICU) collation, it expects
    % {Json, Id} tuples.
    fun
        ({_JsonA, _IdA}=A, {_JsonB, _IdB}=B) ->
            couch_btree:less(Btree, A, B);
        (JsonA, JsonB) ->
            couch_btree:less(Btree, {JsonA, null}, {JsonB, null})
    end.

% query_parse_error could be removed
% we wouldn't need to pass the view type, it'd just parse params.
% I'm not sure what to do about the error handling, but
% it might simplify things to have a parse_view_params function
% that doesn't throw().
parse_view_params(Req, Keys, ViewType, LessThan) ->
    QueryList = couch_httpd:qs(Req),
    QueryParams =
    lists:foldl(fun({K, V}, Acc) ->
        parse_view_param(K, V) ++ Acc
    end, [], QueryList),
    IsMultiGet = (Keys =/= nil),
    Args = #view_query_args{
        view_type=ViewType,
        multi_get=IsMultiGet
    },
    QueryArgs = lists:foldl(fun({K, V}, Args2) ->
        validate_view_query(K, V, Args2)
    end, Args, lists:reverse(QueryParams)), % Reverse to match QS order.
    warn_on_empty_key_range(QueryArgs, LessThan),
    GroupLevel = QueryArgs#view_query_args.group_level,
    case {ViewType, GroupLevel, IsMultiGet} of
    {reduce, exact, true} ->
        QueryArgs;
    {reduce, _, false} ->
        QueryArgs;
    {reduce, _, _} ->
        % we can simplify code if we just drop this error message.
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
% TODO: maybe deprecate startkey_docid
parse_view_param("startkey_docid", Value) ->
    [{start_docid, ?l2b(Value)}];
parse_view_param("start_key_doc_id", Value) ->
    [{start_docid, ?l2b(Value)}];
% TODO: maybe deprecate endkey_docid
parse_view_param("endkey_docid", Value) ->
    [{end_docid, ?l2b(Value)}];
parse_view_param("end_key_doc_id", Value) ->
    [{end_docid, ?l2b(Value)}];
% TODO: maybe deprecate startkey
parse_view_param("startkey", Value) ->
    [{start_key, ?JSON_DECODE(Value)}];
parse_view_param("start_key", Value) ->
    [{start_key, ?JSON_DECODE(Value)}];
% TODO: maybe deprecate endkey
parse_view_param("endkey", Value) ->
    [{end_key, ?JSON_DECODE(Value)}];
parse_view_param("end_key", Value) ->
    [{end_key, ?JSON_DECODE(Value)}];
parse_view_param("limit", Value) ->
    [{limit, parse_positive_int_param(Value)}];
parse_view_param("count", _Value) ->
    throw({query_parse_error, <<"Query parameter 'count' is now 'limit'.">>});
parse_view_param("stale", "ok") ->
    [{stale, ok}];
parse_view_param("stale", "update_after") ->
    [{stale, update_after}];
parse_view_param("stale", _Value) ->
    throw({query_parse_error,
            <<"stale only available as stale=ok or as stale=update_after">>});
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
parse_view_param("conflicts", Value) ->
    [{conflicts, parse_bool_param(Value)}];
parse_view_param("list", Value) ->
    [{list, ?l2b(Value)}];
parse_view_param("callback", _) ->
    []; % Verified in the JSON response functions
parse_view_param(Key, Value) ->
    [{extra, {Key, Value}}].

warn_on_empty_key_range(#view_query_args{start_key=undefined}, _Lt) ->
    ok;
warn_on_empty_key_range(#view_query_args{end_key=undefined}, _Lt) ->
    ok;
warn_on_empty_key_range(#view_query_args{start_key=A, end_key=A}, _Lt) ->
    ok;
warn_on_empty_key_range(#view_query_args{
    start_key=StartKey, end_key=EndKey, direction=Dir}, LessThan) ->
    case {Dir, LessThan(StartKey, EndKey)} of
        {fwd, false} ->
            throw({query_parse_error,
            <<"No rows can match your key range, reverse your ",
                "start_key and end_key or set descending=true">>});
        {rev, true} ->
            throw({query_parse_error,
            <<"No rows can match your key range, reverse your ",
                "start_key and end_key or set descending=false">>});
        _ -> ok
    end.

validate_view_query(start_key, Value, Args) ->
    case Args#view_query_args.multi_get of
    true ->
        Msg = <<"Query parameter `start_key` is "
                "not compatible with multi-get">>,
        throw({query_parse_error, Msg});
    _ ->
        Args#view_query_args{start_key=Value}
    end;
validate_view_query(start_docid, Value, Args) ->
    Args#view_query_args{start_docid=Value};
validate_view_query(end_key, Value, Args) ->
    case Args#view_query_args.multi_get of
    true->
        Msg = <<"Query parameter `end_key` is "
                "not compatible with multi-get">>,
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
validate_view_query(stale, ok, Args) ->
    Args#view_query_args{stale=ok};
validate_view_query(stale, update_after, Args) ->
    Args#view_query_args{stale=update_after};
validate_view_query(stale, _, Args) ->
    Args;
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
validate_view_query(reduce, false, Args) ->
    Args;
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
        Msg = <<"Query parameter `include_docs` "
                "is invalid for reduce views.">>,
        throw({query_parse_error, Msg});
    _ ->
        Args#view_query_args{include_docs=true}
    end;
% Use the view_query_args record's default value
validate_view_query(include_docs, _Value, Args) ->
    Args;
validate_view_query(conflicts, true, Args) ->
    case Args#view_query_args.view_type of
    reduce ->
        Msg = <<"Query parameter `conflicts` "
                "is invalid for reduce views.">>,
        throw({query_parse_error, Msg});
    _ ->
        Args#view_query_args{conflicts = true}
    end;
validate_view_query(extra, _Value, Args) ->
    Args.

make_view_fold_fun(Req, QueryArgs, Etag, Db, UpdateSeq, TotalViewCount, HelperFuns) ->
    #view_fold_helper_funs{
        start_response = StartRespFun,
        send_row = SendRowFun,
        reduce_count = ReduceCountFun
    } = apply_default_helper_funs(HelperFuns),

    #view_query_args{
        include_docs = IncludeDocs,
        conflicts = Conflicts
    } = QueryArgs,
    
    fun({{Key, DocId}, Value}, OffsetReds,
            {AccLimit, AccSkip, Resp, RowFunAcc}) ->
        case {AccLimit, AccSkip, Resp} of
        {0, _, _} ->
            % we've done "limit" rows, stop foldling
            {stop, {0, 0, Resp, RowFunAcc}};
        {_, AccSkip, _} when AccSkip > 0 ->
            % just keep skipping
            {ok, {AccLimit, AccSkip - 1, Resp, RowFunAcc}};
        {_, _, undefined} ->
            % rendering the first row, first we start the response
            Offset = ReduceCountFun(OffsetReds),
            {ok, Resp2, RowFunAcc0} = StartRespFun(Req, Etag,
                TotalViewCount, Offset, RowFunAcc, UpdateSeq),
            {Go, RowFunAcc2} = SendRowFun(Resp2, Db, {{Key, DocId}, Value},
                IncludeDocs, Conflicts, RowFunAcc0),
            {Go, {AccLimit - 1, 0, Resp2, RowFunAcc2}};
        {AccLimit, _, Resp} when (AccLimit > 0) ->
            % rendering all other rows
            {Go, RowFunAcc2} = SendRowFun(Resp, Db, {{Key, DocId}, Value},
                IncludeDocs, Conflicts, RowFunAcc),
            {Go, {AccLimit - 1, 0, Resp, RowFunAcc2}}
        end
    end.

make_reduce_fold_funs(Req, GroupLevel, _QueryArgs, Etag, UpdateSeq, HelperFuns) ->
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
        {ok, Resp2, RowAcc} = StartRespFun(Req, Etag, RowAcc0, UpdateSeq),
        {Go, RowAcc2} = SendRowFun(Resp2, {null, Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp2, RowAcc2}};
    (_Key, Red, {AccLimit, 0, Resp, RowAcc}) when GroupLevel == 0 ->
        % group=false but we've already started the response
        {Go, RowAcc2} = SendRowFun(Resp, {null, Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp, RowAcc2}};

    (Key, Red, {AccLimit, 0, undefined, RowAcc0})
            when is_integer(GroupLevel), is_list(Key) ->
        % group_level and we haven't responded yet
        {ok, Resp2, RowAcc} = StartRespFun(Req, Etag, RowAcc0, UpdateSeq),
        {Go, RowAcc2} = SendRowFun(Resp2,
                {lists:sublist(Key, GroupLevel), Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp2, RowAcc2}};
    (Key, Red, {AccLimit, 0, Resp, RowAcc})
            when is_integer(GroupLevel), is_list(Key) ->
        % group_level and we've already started the response
        {Go, RowAcc2} = SendRowFun(Resp,
                {lists:sublist(Key, GroupLevel), Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp, RowAcc2}};

    (Key, Red, {AccLimit, 0, undefined, RowAcc0}) ->
        % group=true and we haven't responded yet
        {ok, Resp2, RowAcc} = StartRespFun(Req, Etag, RowAcc0, UpdateSeq),
        {Go, RowAcc2} = SendRowFun(Resp2, {Key, Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp2, RowAcc2}};
    (Key, Red, {AccLimit, 0, Resp, RowAcc}) ->
        % group=true and we've already started the response
        {Go, RowAcc2} = SendRowFun(Resp, {Key, Red}, RowAcc),
        {Go, {AccLimit - 1, 0, Resp, RowAcc2}}
    end,
    {ok, GroupRowsFun, RespFun}.

apply_default_helper_funs(
        #view_fold_helper_funs{
            start_response = StartResp,
            send_row = SendRow
        }=Helpers) ->
    StartResp2 = case StartResp of
    undefined -> fun json_view_start_resp/6;
    _ -> StartResp
    end,

    SendRow2 = case SendRow of
    undefined -> fun send_json_view_row/6;
    _ -> SendRow
    end,

    Helpers#view_fold_helper_funs{
        start_response = StartResp2,
        send_row = SendRow2
    };


apply_default_helper_funs(
        #reduce_fold_helper_funs{
            start_response = StartResp,
            send_row = SendRow
        }=Helpers) ->
    StartResp2 = case StartResp of
    undefined -> fun json_reduce_start_resp/4;
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

make_key_options(#view_query_args{direction = Dir}=QueryArgs) ->
     [{dir,Dir} | make_start_key_option(QueryArgs) ++
            make_end_key_option(QueryArgs)].

make_start_key_option(
        #view_query_args{
            start_key = StartKey,
            start_docid = StartDocId}) ->
    if StartKey == undefined ->
        [];
    true ->
        [{start_key, {StartKey, StartDocId}}]
    end.

make_end_key_option(#view_query_args{end_key = undefined}) ->
    [];
make_end_key_option(
        #view_query_args{end_key = EndKey,
            end_docid = EndDocId,
            inclusive_end = true}) ->
    [{end_key, {EndKey, EndDocId}}];
make_end_key_option(
        #view_query_args{
            end_key = EndKey,
            end_docid = EndDocId,
            inclusive_end = false}) ->
    [{end_key_gt, {EndKey,reverse_key_default(EndDocId)}}].

json_view_start_resp(Req, Etag, TotalViewCount, Offset, _Acc, UpdateSeq) ->
    {ok, Resp} = start_json_response(Req, 200, [{"ETag", Etag}]),
    BeginBody = case couch_httpd:qs_value(Req, "update_seq") of
    "true" ->
        io_lib:format(
                "{\"total_rows\":~w,\"update_seq\":~w,"
                "\"offset\":~w,\"rows\":[\r\n",
                [TotalViewCount, UpdateSeq, Offset]);
    _Else ->
        io_lib:format(
                "{\"total_rows\":~w,\"offset\":~w,\"rows\":[\r\n",
                [TotalViewCount, Offset])
    end,
    {ok, Resp, BeginBody}.

send_json_view_row(Resp, Db, Kv, IncludeDocs, Conflicts, RowFront) ->
    JsonObj = view_row_obj(Db, Kv, IncludeDocs, Conflicts),
    send_chunk(Resp, RowFront ++  ?JSON_ENCODE(JsonObj)),
    {ok, ",\r\n"}.

json_reduce_start_resp(Req, Etag, _Acc0, UpdateSeq) ->
    {ok, Resp} = start_json_response(Req, 200, [{"ETag", Etag}]),
    case couch_httpd:qs_value(Req, "update_seq") of
    "true" ->
        {ok, Resp, io_lib:format("{\"update_seq\":~w,\"rows\":[\r\n",[UpdateSeq])};
    _Else ->
        {ok, Resp, "{\"rows\":[\r\n"}
    end.

send_json_reduce_row(Resp, {Key, Value}, RowFront) ->
    send_chunk(Resp, RowFront ++ ?JSON_ENCODE({[{key, Key}, {value, Value}]})),
    {ok, ",\r\n"}.

view_etag(Db, Group, View, QueryArgs) ->
    view_etag(Db, Group, View, QueryArgs, nil).

view_etag(Db, Group, {reduce, _, _, View}, QueryArgs, Extra) ->
    view_etag(Db, Group, View, QueryArgs, Extra);
view_etag(Db, Group, {temp_reduce, View}, QueryArgs, Extra) ->
    view_etag(Db, Group, View, QueryArgs, Extra);
view_etag(_Db, #group{sig=Sig, current_seq=CurrentSeq}, _View, #view_query_args{include_docs=true}, Extra) ->
    couch_httpd:make_etag({Sig, CurrentSeq, Extra});
view_etag(_Db, #group{sig=Sig}, #view{update_seq=UpdateSeq, purge_seq=PurgeSeq}, _QueryArgs, Extra) ->
    couch_httpd:make_etag({Sig, UpdateSeq, PurgeSeq, Extra}).

% the view row has an error
view_row_obj(_Db, {{Key, error}, Value}, _IncludeDocs, _Conflicts) ->
    {[{key, Key}, {error, Value}]};
% include docs in the view output
view_row_obj(Db, {{Key, DocId}, {Props}}, true, Conflicts) ->
    Rev = case couch_util:get_value(<<"_rev">>, Props) of
    undefined ->
        nil;
    Rev0 ->
        couch_doc:parse_rev(Rev0)
    end,
    IncludeId = couch_util:get_value(<<"_id">>, Props, DocId),
    view_row_with_doc(Db, {{Key, DocId}, {Props}}, {IncludeId, Rev}, Conflicts);
view_row_obj(Db, {{Key, DocId}, Value}, true, Conflicts) ->
    view_row_with_doc(Db, {{Key, DocId}, Value}, {DocId, nil}, Conflicts);
% the normal case for rendering a view row
view_row_obj(_Db, {{Key, DocId}, Value}, _IncludeDocs, _Conflicts) ->
    {[{id, DocId}, {key, Key}, {value, Value}]}.

view_row_with_doc(Db, {{Key, DocId}, Value}, IdRev, Conflicts) ->
    {[{id, DocId}, {key, Key}, {value, Value}] ++
        doc_member(Db, IdRev, if Conflicts -> [conflicts]; true -> [] end)}.

doc_member(Db, #doc_info{id = Id, revs = [#rev_info{rev = Rev} | _]} = Info,
        Options) ->
    ?LOG_DEBUG("Include Doc: ~p ~p", [Id, Rev]),
    case couch_db:open_doc(Db, Info, [deleted | Options]) of
    {ok, Doc} ->
        [{doc, couch_doc:to_json_obj(Doc, [])}];
    _ ->
        [{doc, null}]
    end;
doc_member(Db, {DocId, Rev}, Options) ->
    ?LOG_DEBUG("Include Doc: ~p ~p", [DocId, Rev]),
    case (catch couch_httpd_db:couch_doc_open(Db, DocId, Rev, Options)) of
    #doc{} = Doc ->
        JsonDoc = couch_doc:to_json_obj(Doc, []),
        [{doc, JsonDoc}];
    _Else ->
        [{doc, null}]
    end.

finish_view_fold(Req, TotalRows, Offset, FoldResult) ->
    finish_view_fold(Req, TotalRows, Offset, FoldResult, []).

finish_view_fold(Req, TotalRows, Offset, FoldResult, Fields) ->
    case FoldResult of
    {_, _, undefined, _} ->
        % nothing found in the view or keys, nothing has been returned
        % send empty view
        send_json(Req, 200, {[
            {total_rows, TotalRows},
            {offset, Offset},
            {rows, []}
        ] ++ Fields});
    {_, _, Resp, _} ->
        % end the view
        send_chunk(Resp, "\r\n]}"),
        end_json_response(Resp)
    end.

finish_reduce_fold(Req, Resp) ->
    finish_reduce_fold(Req, Resp, []).

finish_reduce_fold(Req, Resp, Fields) ->
    case Resp of
    undefined ->
        send_json(Req, 200, {[
            {rows, []}
        ] ++ Fields});
    Resp ->
        send_chunk(Resp, "\r\n]}"),
        end_json_response(Resp)
    end.

parse_bool_param(Val) ->
    case string:to_lower(Val) of
    "true" -> true;
    "false" -> false;
    _ ->
        Msg = io_lib:format("Invalid boolean parameter: ~p", [Val]),
        throw({query_parse_error, ?l2b(Msg)})
    end.

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

