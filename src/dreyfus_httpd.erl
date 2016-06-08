% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.


%% -*- erlang-indent-level: 4;indent-tabs-mode: nil -*-

-module(dreyfus_httpd).

-export([handle_search_req/3, handle_info_req/3,
         handle_cleanup_req/2, handle_analyze_req/1]).
-include("dreyfus.hrl").
-include_lib("couch/include/couch_db.hrl").
-import(chttpd, [send_method_not_allowed/2, send_json/2, send_json/3,
                 send_error/2]).

handle_search_req(Req, Db, DDoc) ->
    handle_search_req(Req, Db, DDoc, 0, 500).

handle_search_req(#httpd{method=Method, path_parts=[_, _, _, _, IndexName]}=Req
                  ,#db{name=DbName}=Db, DDoc, RetryCount, RetryPause)
  when Method == 'GET'; Method == 'POST' ->
    Start = os:timestamp(),
    QueryArgs = #index_query_args{
        q = Query,
        include_docs = IncludeDocs,
        grouping = Grouping
    } = parse_index_params(Req),
    case Query of
        undefined ->
            Msg = <<"Query must include a 'q' or 'query' argument">>,
            throw({query_parse_error, Msg});
        _ ->
            ok
    end,
    Response = case Grouping#grouping.by of
        nil ->
            case dreyfus_fabric_search:go(DbName, DDoc, IndexName, QueryArgs) of
                {ok, Bookmark0, TotalHits, Hits0} -> % legacy clause
                    Hits = hits_to_json(DbName, IncludeDocs, Hits0),
                    Bookmark = dreyfus_bookmark:pack(Bookmark0),
                    send_json(Req, 200, {[
                        {total_rows, TotalHits},
                        {bookmark, Bookmark},
                        {rows, Hits}
                    ]});
                {ok, Bookmark0, TotalHits, Hits0, Counts0, Ranges0} ->
                    Hits = hits_to_json(DbName, IncludeDocs, Hits0),
                    Bookmark = dreyfus_bookmark:pack(Bookmark0),
                    Counts = case Counts0 of
                        undefined ->
                            [];
                        _ ->
                            [{counts, facets_to_json(Counts0)}]
                    end,
                    Ranges = case Ranges0 of
                        undefined ->
                            [];
                        _ ->
                            [{ranges, facets_to_json(Ranges0)}]
                    end,
                    send_json(Req, 200, {[
                        {total_rows, TotalHits},
                        {bookmark, Bookmark},
                        {rows, Hits}
                    ] ++ Counts ++ Ranges
                    });
                {error, Reason} ->
                    handle_error(Req, Db, DDoc, RetryCount, RetryPause, Reason)
            end;
        _ ->
            % ensure limit in group query >0
            LimitValue = parse_positive_int_param("limit", QueryArgs#index_query_args.limit,
                                                  "max_limit", "200"),
            UseNewApi = Grouping#grouping.new_api,
            case dreyfus_fabric_group1:go(DbName, DDoc, IndexName, QueryArgs) of
                {ok, []} ->
                    send_grouped_response(Req, {0, 0, []}, UseNewApi);
                {ok, TopGroups} ->
                    QueryArgs1 = QueryArgs#index_query_args{grouping=Grouping#grouping{groups=TopGroups}},
                    case dreyfus_fabric_group2:go(DbName, DDoc,
                                                  IndexName, QueryArgs1) of
                        {ok, {TotalHits, TotalGroupedHits, Groups0}} ->
                            Groups = [group_to_json(DbName, IncludeDocs, Group, UseNewApi) || Group <- Groups0],
                            send_grouped_response(Req, {TotalHits, TotalGroupedHits, Groups}, UseNewApi);
                        {error, Reason} ->
                            handle_error(Req, Db, DDoc, RetryCount, RetryPause, Reason)
                    end;
                {error, Reason} ->
                    handle_error(Req, Db, DDoc, RetryCount, RetryPause, Reason)
            end
    end,
    RequestTime = timer:now_diff(os:timestamp(), Start) div 1000,
    couch_stats:update_histogram([dreyfus, httpd, search], RequestTime),
    Response;
handle_search_req(#httpd{path_parts=[_, _, _, _, _]}=Req, _Db, _DDoc, _RetryCount, _RetryPause) ->
    send_method_not_allowed(Req, "GET,POST");
handle_search_req(Req, _Db, _DDoc, _RetryCount, _RetryPause) ->
    send_error(Req, {bad_request, "path not recognized"}).

handle_info_req(#httpd{method='GET', path_parts=[_, _, _, _, IndexName]}=Req
                  ,#db{name=DbName}, #doc{id=Id}=DDoc) ->
    case dreyfus_fabric_info:go(DbName, DDoc, IndexName) of
        {ok, IndexInfoList} ->
            send_json(Req, 200, {[
                {name,  <<Id/binary,"/",IndexName/binary>>},
                {search_index, {IndexInfoList}}
            ]});
        {error, Reason} ->
            send_error(Req, Reason)
    end;
handle_info_req(#httpd{path_parts=[_, _, _, _, _]}=Req, _Db, _DDoc) ->
    send_method_not_allowed(Req, "GET");
handle_info_req(Req, _Db, _DDoc) ->
    send_error(Req, {bad_request, "path not recognized"}).

handle_cleanup_req(#httpd{method='POST'}=Req, #db{name=DbName}) ->
    ok = dreyfus_fabric_cleanup:go(DbName),
    send_json(Req, 202, {[{ok, true}]});
handle_cleanup_req(Req, _Db) ->
    send_method_not_allowed(Req, "POST").

handle_analyze_req(#httpd{method='GET'}=Req) ->
    Analyzer = couch_httpd:qs_value(Req, "analyzer"),
    Text = couch_httpd:qs_value(Req, "text"),
    analyze(Req, Analyzer, Text);
handle_analyze_req(#httpd{method='POST'}=Req) ->
    couch_httpd:validate_ctype(Req, "application/json"),
    {Fields} = chttpd:json_body_obj(Req),
    Analyzer = couch_util:get_value(<<"analyzer">>, Fields),
    Text = couch_util:get_value(<<"text">>, Fields),
    analyze(Req, Analyzer, Text);
handle_analyze_req(Req) ->
    send_method_not_allowed(Req, "GET,POST").

analyze(Req, Analyzer, Text) ->
    case Analyzer of
        undefined ->
            throw({bad_request, "analyzer parameter is mandatory"});
        _ when is_list(Analyzer) ->
            ok;
        _ when is_binary(Analyzer) ->
            ok;
        {[_|_]} ->
            ok;
        _ ->
            throw({bad_request, "analyzer parameter must be a string or an object"})
    end,
    case Text of
        undefined ->
            throw({bad_request, "text parameter is mandatory"});
        _ when is_list(Text) ->
            ok;
        _ when is_binary(Text) ->
            ok;
        _ ->
            throw({bad_request, "text parameter must be a string"})
    end,
    case clouseau_rpc:analyze(couch_util:to_binary(Analyzer),
                              couch_util:to_binary(Text)) of
        {ok, Tokens} ->
            send_json(Req, 200, {[{tokens, Tokens}]});
        {error, Reason} ->
            send_error(Req, Reason)
    end.

parse_index_params(#httpd{method='GET'}=Req) ->
    IndexParams = lists:flatmap(fun({K, V}) -> parse_index_param(K, V) end,
        chttpd:qs(Req)),
    parse_index_params(IndexParams);
parse_index_params(#httpd{method='POST'}=Req) ->
    IndexParams = lists:flatmap(fun({K, V}) -> parse_json_index_param(K, V) end,
        element(1, chttpd:json_body_obj(Req))),
    parse_index_params(IndexParams);
parse_index_params(IndexParams) ->
    Args = #index_query_args{},
    lists:foldl(fun({K, V}, Args2) ->
        validate_index_query(K, V, Args2)
    end, Args, IndexParams).

validate_index_query(q, Value, Args) ->
    Args#index_query_args{q=Value};
validate_index_query(stale, Value, Args) ->
    Args#index_query_args{stale=Value};
validate_index_query(limit, Value, Args) ->
    Args#index_query_args{limit=Value};
validate_index_query(include_docs, Value, Args) ->
    Args#index_query_args{include_docs=Value};
validate_index_query(include_fields, Value, Args) ->
    Args#index_query_args{include_fields=Value};
validate_index_query(bookmark, Value, Args) ->
    Args#index_query_args{bookmark=Value};
validate_index_query(sort, Value, Args) ->
    Args#index_query_args{sort=Value};
validate_index_query(group_by, Value, #index_query_args{grouping=Grouping}=Args) ->
    Args#index_query_args{grouping=Grouping#grouping{by=Value, new_api=false}};
validate_index_query(group_field, Value, #index_query_args{grouping=Grouping}=Args) ->
    Args#index_query_args{grouping=Grouping#grouping{by=Value, new_api=true}};
validate_index_query(group_sort, Value, #index_query_args{grouping=Grouping}=Args) ->
    Args#index_query_args{grouping=Grouping#grouping{sort=Value}};
validate_index_query(group_limit, Value, #index_query_args{grouping=Grouping}=Args) ->
    Args#index_query_args{grouping=Grouping#grouping{limit=Value}};
validate_index_query(stable, Value, Args) ->
    Args#index_query_args{stable=Value};
validate_index_query(counts, Value, Args) ->
    Args#index_query_args{counts=Value};
validate_index_query(ranges, Value, Args) ->
    Args#index_query_args{ranges=Value};
validate_index_query(drilldown, Value, Args) ->
    DrillDown = Args#index_query_args.drilldown,
    Args#index_query_args{drilldown=[Value|DrillDown]};
validate_index_query(highlight_fields, Value, Args) ->
    Args#index_query_args{highlight_fields=Value};
validate_index_query(highlight_pre_tag, Value, Args) ->
    Args#index_query_args{highlight_pre_tag=Value};
validate_index_query(highlight_post_tag, Value, Args) ->
    Args#index_query_args{highlight_post_tag=Value};
validate_index_query(highlight_number, Value, Args) ->
    Args#index_query_args{highlight_number=Value};
validate_index_query(highlight_size, Value, Args) ->
    Args#index_query_args{highlight_size=Value};
validate_index_query(extra, _Value, Args) ->
    Args.

parse_index_param("", _) ->
    [];
parse_index_param("q", Value) ->
    [{q, ?l2b(Value)}];
parse_index_param("query", Value) ->
    [{q, ?l2b(Value)}];
parse_index_param("bookmark", Value) ->
    [{bookmark, ?l2b(Value)}];
parse_index_param("sort", Value) ->
    [{sort, ?JSON_DECODE(Value)}];
parse_index_param("limit", Value) ->
    [{limit, parse_non_negative_int_param("limit", Value, "max_limit", "200")}];
parse_index_param("stale", "ok") ->
    [{stale, ok}];
parse_index_param("stale", _Value) ->
    throw({query_parse_error, <<"stale only available as stale=ok">>});
parse_index_param("include_docs", Value) ->
    [{include_docs, parse_bool_param("include_docs", Value)}];
parse_index_param("group_by", Value) ->
    [{group_by, ?l2b(Value)}];
parse_index_param("group_field", Value) ->
    [{group_field, ?l2b(Value)}];
parse_index_param("group_sort", Value) ->
    [{group_sort, ?JSON_DECODE(Value)}];
parse_index_param("group_limit", Value) ->
    [{group_limit, parse_positive_int_param("group_limit", Value, "max_group_limit", "200")}];
parse_index_param("stable", Value) ->
    [{stable, parse_bool_param("stable", Value)}];
parse_index_param("include_fields", Value) ->
    [{include_fields, ?JSON_DECODE(Value)}];
parse_index_param("counts", Value) ->
    [{counts, ?JSON_DECODE(Value)}];
parse_index_param("ranges", Value) ->
    [{ranges, ?JSON_DECODE(Value)}];
parse_index_param("drilldown", Value) ->
    [{drilldown, ?JSON_DECODE(Value)}];
parse_index_param("highlight_fields", Value) ->
    [{highlight_fields, ?JSON_DECODE(Value)}];
parse_index_param("highlight_pre_tag", Value) ->
    [{highlight_pre_tag, ?JSON_DECODE(Value)}];
parse_index_param("highlight_post_tag", Value) ->
    [{highlight_post_tag, ?JSON_DECODE(Value)}];
parse_index_param("highlight_number", Value) ->
    [{highlight_number, parse_positive_int_param2("highlight_number", Value)}];
parse_index_param("highlight_size", Value) ->
    [{highlight_size, parse_positive_int_param2("highlight_size", Value)}];
parse_index_param(Key, Value) ->
    [{extra, {Key, Value}}].

parse_json_index_param(<<"q">>, Value) ->
    [{q, Value}];
parse_json_index_param(<<"query">>, Value) ->
    [{q, Value}];
parse_json_index_param(<<"bookmark">>, Value) ->
    [{bookmark, Value}];
parse_json_index_param(<<"sort">>, Value) ->
    [{sort, Value}];
parse_json_index_param(<<"limit">>, Value) ->
    [{limit, parse_non_negative_int_param("limit", Value, "max_limit", "200")}];
parse_json_index_param(<<"stale">>, <<"ok">>) ->
    [{stale, ok}];
parse_json_index_param(<<"include_docs">>, Value) when is_boolean(Value) ->
    [{include_docs, Value}];
parse_json_index_param(<<"group_by">>, Value) ->
    [{group_by, Value}];
parse_json_index_param(<<"group_field">>, Value) ->
    [{group_field, Value}];
parse_json_index_param(<<"group_sort">>, Value) ->
    [{group_sort, Value}];
parse_json_index_param(<<"group_limit">>, Value) ->
    [{group_limit, parse_positive_int_param("group_limit", Value, "max_group_limit", "200")}];
parse_json_index_param(<<"stable">>, Value) ->
    [{stable, parse_bool_param("stable", Value)}];
parse_json_index_param(<<"include_fields">>, Value) ->
    [{include_fields, Value}];
parse_json_index_param(<<"counts">>, Value) ->
    [{counts, Value}];
parse_json_index_param(<<"ranges">>, Value) ->
    [{ranges, Value}];
parse_json_index_param(<<"drilldown">>, Value) ->
    [{drilldown, Value}];
parse_json_index_param(<<"highlight_fields">>, Value) ->
    [{highlight_fields, Value}];
parse_json_index_param(<<"highlight_pre_tag">>, Value) ->
    [{highlight_pre_tag, Value}];
parse_json_index_param(<<"highlight_pos_tag">>, Value) ->
    [{highlight_post_tag, Value}];
parse_json_index_param(<<"highlight_number">>, Value) ->
    [{highlight_number, parse_positive_int_param2("highlight_number", Value)}];
parse_json_index_param(<<"highlight_size">>, Value) ->
    [{highlight_size, parse_positive_int_param2("highlight_size", Value)}];
parse_json_index_param(Key, Value) ->
    [{extra, {Key, Value}}].

%% VV copied from chttpd_view.erl

parse_bool_param(_, Val) when is_boolean(Val) ->
    Val;
parse_bool_param(_, "true") -> true;
parse_bool_param(_, "false") -> false;
parse_bool_param(Name, Val) ->
    Msg = io_lib:format("Invalid value for ~s: ~p", [Name, Val]),
    throw({query_parse_error, ?l2b(Msg)}).

parse_int_param(_, Val) when is_integer(Val) ->
    Val;
parse_int_param(Name, Val) ->
    case (catch list_to_integer(Val)) of
    IntVal when is_integer(IntVal) ->
        IntVal;
    _ ->
        Msg = io_lib:format("Invalid value for ~s: ~p", [Name, Val]),
        throw({query_parse_error, ?l2b(Msg)})
    end.

parse_positive_int_param(Name, Val, Prop, Default) ->
    MaximumVal = list_to_integer(
        config:get("dreyfus", Prop, Default)),
    case parse_int_param(Name, Val) of
    IntVal when IntVal > MaximumVal ->
        Fmt = "Value for ~s is too large, must not exceed ~p",
        Msg = io_lib:format(Fmt, [Name, MaximumVal]),
        throw({query_parse_error, ?l2b(Msg)});
    IntVal when IntVal > 0 ->
        IntVal;
    IntVal when IntVal =< 0 ->
        Fmt = "~s must be greater than zero",
        Msg = io_lib:format(Fmt, [Name]),
        throw({query_parse_error, ?l2b(Msg)});
    _ ->
        Fmt = "Invalid value for ~s: ~p",
        Msg = io_lib:format(Fmt, [Name, Val]),
        throw({query_parse_error, ?l2b(Msg)})
    end.

parse_positive_int_param2(Name, Val) ->
    case parse_int_param(Name, Val) of
    IntVal when IntVal > 0 ->
        IntVal;
    IntVal when IntVal =< 0 ->
        Fmt = "~s must be greater than zero",
        Msg = io_lib:format(Fmt, [Name]),
        throw({query_parse_error, ?l2b(Msg)});
    _ ->
        Fmt = "Invalid value for ~s: ~p",
        Msg = io_lib:format(Fmt, [Name, Val]),
        throw({query_parse_error, ?l2b(Msg)})
    end.

parse_non_negative_int_param(Name, Val, Prop, Default) ->
    MaximumVal = list_to_integer(
        config:get("dreyfus", Prop, Default)),
    case parse_int_param(Name, Val) of
    IntVal when IntVal > MaximumVal ->
        Fmt = "Value for ~s is too large, must not exceed ~p",
        Msg = io_lib:format(Fmt, [Name, MaximumVal]),
        throw({query_parse_error, ?l2b(Msg)});
    IntVal when IntVal >= 0 ->
        IntVal;
    IntVal when IntVal < 0 ->
        Fmt = "~s must be greater than or equal to zero",
        Msg = io_lib:format(Fmt, [Name]),
        throw({query_parse_error, ?l2b(Msg)});
    _ ->
        Fmt = "Invalid value for ~s: ~p",
        Msg = io_lib:format(Fmt, [Name, Val]),
        throw({query_parse_error, ?l2b(Msg)})
    end.


hits_to_json(DbName, IncludeDocs, Hits) ->
    {Ids, HitData} = lists:unzip(lists:map(fun get_hit_data/1, Hits)),
    if IncludeDocs ->
        {ok, JsonDocs} = dreyfus_fabric:get_json_docs(DbName, Ids),
        lists:zipwith(fun(Hit, {Id, Doc}) ->
                case Hit of
                    {Id, Order, Fields} ->
                        {[{id, Id}, {order, Order}, {fields, {Fields}}, Doc]};
                    {Id, Order, Fields, Highlights} ->
                        {[{id, Id}, {order, Order}, {fields, {Fields}},
                          {highlights, {Highlights}}, Doc]}
                end
            end, HitData, JsonDocs);

    true ->
        lists:map(fun(Hit) ->
                case Hit of
                    {Id, Order, Fields} ->
                      {[{id, Id}, {order, Order}, {fields, {Fields}}]};
                    {Id, Order, Fields, Highlights} ->
                      {[{id, Id}, {order, Order}, {fields, {Fields}}, {highlights, {Highlights}}]}
                end
          end, HitData)
    end.

get_hit_data(Hit) ->
    Id = couch_util:get_value(<<"_id">>, Hit#hit.fields),
    Fields = lists:keydelete(<<"_id">>, 1, Hit#hit.fields),
    case couch_util:get_value(<<"_highlights">>, Hit#hit.fields) of
        undefined ->
            {Id, {Id, Hit#hit.order, Fields}};
        Highlights ->
            Fields0 = lists:keydelete(<<"_highlights">>, 1, Fields),
            {Id, {Id, Hit#hit.order, Fields0, Highlights}}
    end.

group_to_json(DbName, IncludeDocs, {Name, TotalHits, Hits}, UseNewApi) ->
    {TotalHitsKey, HitsKey} = case UseNewApi of
        true -> {total_rows, rows};
        false -> {total_hits, hits}
    end,
    {[{by, Name},
      {TotalHitsKey, TotalHits},
      {HitsKey, hits_to_json(DbName, IncludeDocs, Hits)}]}.

facets_to_json(Facets) ->
    {[facet_to_json(F) || F <- Facets]}.

facet_to_json({K, V, []}) ->
    {hd(K), V};
facet_to_json({K0, _V0, C0}) ->
    C2 = [{tl(K1), V1, C1} || {K1, V1, C1} <- C0],
    {hd(K0), facets_to_json(C2)}.

send_grouped_response(Req, {TotalHits, TotalGroupedHits, Groups}, UseNewApi) ->
    GroupResponsePairs = case UseNewApi of
        true -> [{total_rows, TotalHits}, {groups, Groups}];
        false -> [{total_hits, TotalHits}, {total_grouped_hits, TotalGroupedHits}, {groups, Groups}]
    end,
    send_json(Req, 200, {GroupResponsePairs}).

handle_error(Req, Db, DDoc, RetryCount, RetryPause, {exit, _}) ->
    backoff_and_retry(Req, Db, DDoc, RetryCount, RetryPause);
handle_error(Req, Db, DDoc, RetryCount, RetryPause, {{normal, _}, _}) ->
    backoff_and_retry(Req, Db, DDoc, RetryPause, RetryCount);
handle_error(Req, _Db, _DDoc, _RetryCount, _RetryPause, Reason) ->
    send_error(Req, Reason).

backoff_and_retry(Req, Db, DDoc, RetryCount, RetryPause) ->
    RetryLimit = list_to_integer(config:get("dreyfus", "retry_limit", "5")),
    case RetryCount > RetryLimit of
        true ->
            send_error(Req, timeout);
        false ->
            timer:sleep(RetryPause),
            handle_search_req(Req, Db, DDoc, RetryCount + 1, RetryPause * 2)
    end.
