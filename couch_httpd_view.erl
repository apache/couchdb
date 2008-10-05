% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_httpd_view).
-include("couch_db.hrl").

-export([handle_view_req/2,handle_temp_view_req/2]).

-export([parse_view_query/1,make_view_fold_fun/4,finish_view_fold/3]).

-import(couch_httpd,
    [send_json/2,send_json/3,send_json/4,send_method_not_allowed/2,
    start_json_response/2,send_chunk/2,end_json_response/1]).


handle_view_req(#httpd{method='GET',path_parts=[_,_, Id, ViewName]}=Req, Db) ->
    #view_query_args{
        reduce = Reduce
    } = QueryArgs = parse_view_query(Req),
    
    case couch_view:get_map_view({couch_db:name(Db), 
            <<"_design/", Id/binary>>, ViewName}) of
    {ok, View} ->    
        output_map_view(Req, View, QueryArgs);
    {not_found, Reason} ->
        case couch_view:get_reduce_view({couch_db:name(Db),
                <<"_design/", Id/binary>>, ViewName}) of
        {ok, View} ->
            case Reduce of
            false ->
                {reduce, _N, _Lang, MapView} = View,
                output_map_view(Req, MapView, QueryArgs);
            _ ->
                output_reduce_view(Req, View)
            end;
        _ ->
            throw({not_found, Reason})
        end
    end;

handle_view_req(Req, _Db) ->
    send_method_not_allowed(Req, "GET,HEAD").

handle_temp_view_req(#httpd{method='POST'}=Req, Db) ->
    QueryArgs = parse_view_query(Req),

    case couch_httpd:primary_header_value(Req, "content-type") of
        undefined -> ok;
        "application/json" -> ok;
        Else -> throw({incorrect_mime_type, Else})
    end,
    {Props} = couch_httpd:json_body(Req),
    Language = proplists:get_value(<<"language">>, Props, <<"javascript">>),
    MapSrc = proplists:get_value(<<"map">>, Props),
    case proplists:get_value(<<"reduce">>, Props, null) of
    null ->
        {ok, View} = couch_view:get_map_view({temp, couch_db:name(Db), Language, MapSrc}),
        output_map_view(Req, View, QueryArgs);
    RedSrc ->
        {ok, View} = couch_view:get_reduce_view(
                {temp,  couch_db:name(Db), Language, MapSrc, RedSrc}),
        output_reduce_view(Req, View)
    end;

handle_temp_view_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

output_map_view(Req, View, QueryArgs) ->
    #view_query_args{
        count = Count,
        direction = Dir,
        skip = SkipCount,
        start_key = StartKey,
        start_docid = StartDocId
    } = QueryArgs,
    {ok, RowCount} = couch_view:get_row_count(View),
    Start = {StartKey, StartDocId},
    FoldlFun = make_view_fold_fun(Req, QueryArgs, RowCount,
            fun couch_view:reduce_to_count/1),
    FoldAccInit = {Count, SkipCount, undefined, []},
    FoldResult = couch_view:fold(View, Start, Dir, FoldlFun, FoldAccInit),
    finish_view_fold(Req, RowCount, FoldResult).

output_reduce_view(Req, View) ->
    #view_query_args{
        start_key = StartKey,
        end_key = EndKey,
        count = Count,
        skip = Skip,
        direction = Dir,
        start_docid = StartDocId,
        end_docid = EndDocId,
        group_level = GroupLevel
    } = parse_view_query(Req),
    GroupRowsFun =
        fun({_Key1,_}, {_Key2,_}) when GroupLevel == 0 ->
            true;
        ({Key1,_}, {Key2,_})
                when is_integer(GroupLevel) and is_list(Key1) and is_list(Key2) ->
            lists:sublist(Key1, GroupLevel) == lists:sublist(Key2, GroupLevel);
        ({Key1,_}, {Key2,_}) ->
            Key1 == Key2
        end,
    {ok, Resp} = start_json_response(Req, 200),
    send_chunk(Resp, "{\"rows\":["),
    {ok, _} = couch_view:fold_reduce(View, Dir, {StartKey, StartDocId}, {EndKey, EndDocId},
        GroupRowsFun,
        fun(_Key, _Red, {AccSeparator,AccSkip,AccCount}) when AccSkip > 0 ->
            {ok, {AccSeparator,AccSkip-1,AccCount}};
        (_Key, _Red, {AccSeparator,0,AccCount}) when AccCount == 0 ->
            {stop, {AccSeparator,0,AccCount}};
        (_Key, Red, {AccSeparator,0,AccCount}) when GroupLevel == 0 ->
            Json = ?JSON_ENCODE({[{key, null}, {value, Red}]}),
            send_chunk(Resp, AccSeparator ++ Json),
            {ok, {",",0,AccCount-1}};
        (Key, Red, {AccSeparator,0,AccCount})
                when is_integer(GroupLevel) 
                andalso is_list(Key) ->
            Json = ?JSON_ENCODE(
                {[{key, lists:sublist(Key, GroupLevel)},{value, Red}]}),
            send_chunk(Resp, AccSeparator ++ Json),
            {ok, {",",0,AccCount-1}};
        (Key, Red, {AccSeparator,0,AccCount}) ->
            Json = ?JSON_ENCODE({[{key, Key}, {value, Red}]}),
            send_chunk(Resp, AccSeparator ++ Json),
            {ok, {",",0,AccCount-1}}
        end, {"", Skip, Count}),
    send_chunk(Resp, "]}"),
    end_json_response(Resp).
    

reverse_key_default(nil) -> {};
reverse_key_default({}) -> nil;
reverse_key_default(Key) -> Key.

parse_view_query(Req) ->
    QueryList = couch_httpd:qs(Req),
    lists:foldl(fun({Key,Value}, Args) ->
        case {Key, Value} of
        {"", _} ->
            Args;
        {"key", Value} ->
            JsonKey = ?JSON_DECODE(Value),
            Args#view_query_args{start_key=JsonKey,end_key=JsonKey};
        {"startkey_docid", DocId} ->
            Args#view_query_args{start_docid=list_to_binary(DocId)};
        {"endkey_docid", DocId} ->
            Args#view_query_args{end_docid=list_to_binary(DocId)};
        {"startkey", Value} ->
            Args#view_query_args{start_key=?JSON_DECODE(Value)};
        {"endkey", Value} ->
            Args#view_query_args{end_key=?JSON_DECODE(Value)};
        {"count", Value} ->
            case (catch list_to_integer(Value)) of
            Count when is_integer(Count) ->
                if Count < 0 ->
                    Msg = io_lib:format("Count must be a positive integer: count=~s", [Value]),
                    throw({query_parse_error, Msg});
                true ->
                    Args#view_query_args{count=Count}
                end;
            _Error ->
                Msg = io_lib:format("Bad URL query value, number expected: count=~s", [Value]),
                throw({query_parse_error, Msg})
            end;
        {"update", "false"} ->
            Args#view_query_args{update=false};
        {"descending", "true"} ->
            case Args#view_query_args.direction of
            fwd ->
                Args#view_query_args {
                    direction = rev,
                    start_key = reverse_key_default(Args#view_query_args.start_key),
                    start_docid = reverse_key_default(Args#view_query_args.start_docid),
                    end_key = reverse_key_default(Args#view_query_args.end_key),
                    end_docid =  reverse_key_default(Args#view_query_args.end_docid)};
            _ ->
                Args %already reversed
            end;
        {"descending", "false"} ->
          % The descending=false behaviour is the default behaviour, so we
          % simpply ignore it. This is only for convenience when playing with
          % the HTTP API, so that a user doesn't get served an error when
          % flipping true to false in the descending option.
          Args;
        {"skip", Value} ->
            case (catch list_to_integer(Value)) of
            Count when is_integer(Count) ->
                Args#view_query_args{skip=Count};
            _Error ->
                Msg = lists:flatten(io_lib:format(
                "Bad URL query value, number expected: skip=~s", [Value])),
                throw({query_parse_error, Msg})
            end;
        {"group", "true"} ->
            Args#view_query_args{group_level=exact};
        {"group_level", LevelStr} ->
            Args#view_query_args{group_level=list_to_integer(LevelStr)};
        {"reduce", "true"} ->
            Args#view_query_args{reduce=true};
        {"reduce", "false"} ->
            Args#view_query_args{reduce=false};
        _ -> % unknown key
            Msg = lists:flatten(io_lib:format(
                "Bad URL query key:~s", [Key])),
            throw({query_parse_error, Msg})
        end
    end, #view_query_args{}, QueryList).


make_view_fold_fun(Req, QueryArgs, TotalViewCount, ReduceCountFun) ->
    #view_query_args{
        end_key = EndKey,
        end_docid = EndDocId,
        direction = Dir
    } = QueryArgs,

    PassedEndFun =
    case Dir of
    fwd ->
        fun(ViewKey, ViewId) ->
            couch_view:less_json([EndKey, EndDocId], [ViewKey, ViewId])
        end;
    rev->
        fun(ViewKey, ViewId) ->
            couch_view:less_json([ViewKey, ViewId], [EndKey, EndDocId])
        end
    end,

    fun({{Key, DocId}, Value}, OffsetReds,
                      {AccCount, AccSkip, Resp, AccRevRows}) ->
        Offset = ReduceCountFun(OffsetReds), % I think we only need this call once per view
        PassedEnd = PassedEndFun(Key, DocId),
        case {PassedEnd, AccCount, AccSkip, Resp} of
        {true, _, _, _} ->
            % The stop key has been passed, stop looping.
            {stop, {AccCount, AccSkip, Resp, AccRevRows}};
        {_, 0, _, _} ->
            % we've done "count" rows, stop foldling
            {stop, {0, 0, Resp, AccRevRows}};
        {_, _, AccSkip, _} when AccSkip > 0 ->
            {ok, {AccCount, AccSkip - 1, Resp, AccRevRows}};
        {_, _, _, undefined} ->
            {ok, Resp2} = start_json_response(Req, 200),
            JsonBegin = io_lib:format("{\"total_rows\":~w,\"offset\":~w,\"rows\":[\r\n",
                    [TotalViewCount, Offset]),
            JsonObj = {[{id, DocId}, {key, Key}, {value, Value}]},
            
            send_chunk(Resp2, JsonBegin ++ ?JSON_ENCODE(JsonObj)),
            {ok, {AccCount - 1, 0, Resp2, AccRevRows}};
        {_, AccCount, _, Resp} when (AccCount > 0) ->
            JsonObj = {[{id, DocId}, {key, Key}, {value, Value}]},
            send_chunk(Resp, ",\r\n" ++  ?JSON_ENCODE(JsonObj)),
            {ok, {AccCount - 1, 0, Resp, AccRevRows}}
        end
    end.

finish_view_fold(Req, TotalRows, FoldResult) ->
    case FoldResult of
    {ok, {_, _, undefined, _}} ->
        % nothing found in the view, nothing has been returned
        % send empty view
        send_json(Req, 200, {[
            {total_rows, TotalRows},
            {rows, []}
        ]});
    {ok, {_, _, Resp, AccRevRows}} ->
        % end the view
        send_chunk(Resp, AccRevRows ++ "\r\n]}"),
        end_json_response(Resp);
    Error ->
        throw(Error)
    end.