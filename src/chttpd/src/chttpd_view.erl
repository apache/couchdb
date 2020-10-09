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
-include_lib("couch_mrview/include/couch_mrview.hrl").

-export([
    handle_view_req/3,
    validate_args/2,
    parse_queries/4,
    view_cb/2
]).

-define(DEFAULT_ALL_DOCS_PAGE_SIZE, 2000).
-define(DEFAULT_VIEWS_PAGE_SIZE, 2000).

multi_query_view(Req, Db, DDoc, ViewName, Queries) ->
    Args = couch_views_http:parse_params(Req, undefined),
    case couch_views_util:is_paginated(Args) of
        false ->
            stream_multi_query_view(Req, Db, DDoc, ViewName, Args, Queries);
        true ->
            paginate_multi_query_view(Req, Db, DDoc, ViewName, Args, Queries)
    end.


stream_multi_query_view(Req, Db, DDoc, ViewName, Args0, Queries) ->
    {ok, #mrst{views=Views}} = couch_mrview_util:ddoc_to_mrst(Db, DDoc),
    Args1 = couch_mrview_util:set_view_type(Args0, ViewName, Views),
    ArgQueries = parse_queries(Req, Args1, Queries, fun(QueryArg) ->
        couch_mrview_util:set_view_type(QueryArg, ViewName, Views)
    end),
    VAcc0 = #vacc{db=Db, req=Req, prepend="\r\n"},
    FirstChunk = "{\"results\":[",
    {ok, Resp0} = chttpd:start_delayed_json_response(VAcc0#vacc.req, 200, [], FirstChunk),
    VAcc1 = VAcc0#vacc{resp=Resp0},
    VAcc2 = lists:foldl(fun(Args, Acc0) ->
        Fun = fun view_cb/2,
        {ok, Acc1} = couch_views:query(Db, DDoc, ViewName, Fun, Acc0, Args),
        Acc1
    end, VAcc1, ArgQueries),
    {ok, Resp1} = chttpd:send_delayed_chunk(VAcc2#vacc.resp, "\r\n]}"),
    chttpd:end_delayed_json_response(Resp1).


paginate_multi_query_view(Req, Db, DDoc, ViewName, Args0, Queries) ->
    {ok, #mrst{views=Views}} = couch_mrview_util:ddoc_to_mrst(Db, DDoc),
    ArgQueries = parse_queries(Req, Args0, Queries, fun(QueryArg) ->
        couch_mrview_util:set_view_type(QueryArg, ViewName, Views)
    end),
    KeyFun = fun({Props}) ->
        {couch_util:get_value(id, Props), couch_util:get_value(key, Props)}
    end,
    #mrargs{page_size = PageSize} = Args0,
    #httpd{path_parts = Parts} = Req,
    UpdateSeq = fabric2_db:get_update_seq(Db),
    EtagTerm = {Parts, UpdateSeq, Args0},
    Response = couch_views_http:paginated(
        Req, EtagTerm, PageSize, ArgQueries, KeyFun,
        fun(Args) ->
           {ok, #vacc{meta=MetaMap, buffer=Items}} = couch_views:query(
               Db, DDoc, ViewName, fun view_cb/2, #vacc{paginated=true}, Args),
           {MetaMap, Items}
        end),
    chttpd:send_json(Req, Response).


design_doc_post_view(Req, Props, Db, DDoc, ViewName, Keys) ->
    Args = couch_mrview_http:parse_body_and_query(Req, Props, Keys),
    fabric_query_view(Db, Req, DDoc, ViewName, Args).

design_doc_view(Req, Db, DDoc, ViewName, Keys) ->
    Args = couch_views_http:parse_params(Req, Keys),
    fabric_query_view(Db, Req, DDoc, ViewName, Args).


fabric_query_view(Db, Req, DDoc, ViewName, Args) ->
    case couch_views_util:is_paginated(Args) of
        false ->
            stream_fabric_query_view(Db, Req, DDoc, ViewName, Args);
        true ->
            paginate_fabric_query_view(Db, Req, DDoc, ViewName, Args)
    end.


stream_fabric_query_view(Db, Req, DDoc, ViewName, Args) ->
    Max = chttpd:chunked_response_buffer_size(),
    Fun = fun view_cb/2,
    VAcc = #vacc{db=Db, req=Req, threshold=Max},
    {ok, Resp} = couch_views:query(Db, DDoc, ViewName, Fun, VAcc, Args),
    {ok, Resp#vacc.resp}.


paginate_fabric_query_view(Db, Req, DDoc, ViewName, Args0) ->
    KeyFun = fun({Props}) ->
        {couch_util:get_value(id, Props), couch_util:get_value(key, Props)}
    end,
    #httpd{path_parts = Parts} = Req,
    UpdateSeq = fabric2_db:get_update_seq(Db),
    ETagTerm = {Parts, UpdateSeq, Args0},
    Response = couch_views_http:paginated(
        Req, ETagTerm, Args0, KeyFun,
        fun(Args) ->
            VAcc0 = #vacc{paginated=true},
            {ok, VAcc1} = couch_views:query(Db, DDoc, ViewName, fun view_cb/2, VAcc0, Args),
            #vacc{meta=Meta, buffer=Items} = VAcc1,
            {Meta, Items}
        end),
    chttpd:send_json(Req, Response).

view_cb({row, Row} = Msg, Acc) ->
    case lists:keymember(doc, 1, Row) of
        true -> chttpd_stats:incr_reads();
        false -> ok
    end,
    chttpd_stats:incr_rows(),
    couch_views_http:view_cb(Msg, Acc);

view_cb(Msg, Acc) ->
    couch_views_http:view_cb(Msg, Acc).


handle_view_req(#httpd{method='POST',
    path_parts=[_, _, _, _, ViewName, <<"queries">>]}=Req, Db, DDoc) ->
    chttpd:validate_ctype(Req, "application/json"),
    Props = couch_httpd:json_body_obj(Req),
    case couch_mrview_util:get_view_queries(Props) of
        undefined ->
            throw({bad_request,
                <<"POST body must include `queries` parameter.">>});
        Queries ->
            multi_query_view(Req, Db, DDoc, ViewName, Queries)
    end;

handle_view_req(#httpd{path_parts=[_, _, _, _, _, <<"queries">>]}=Req,
    _Db, _DDoc) ->
    chttpd:send_method_not_allowed(Req, "POST");

handle_view_req(#httpd{method='GET',
        path_parts=[_, _, _, _, ViewName]}=Req, Db, DDoc) ->
    couch_stats:increment_counter([couchdb, httpd, view_reads]),
    Keys = chttpd:qs_json_value(Req, "keys", undefined),
    design_doc_view(Req, Db, DDoc, ViewName, Keys);

handle_view_req(#httpd{method='POST',
        path_parts=[_, _, _, _, ViewName]}=Req, Db, DDoc) ->
    chttpd:validate_ctype(Req, "application/json"),
    Props = couch_httpd:json_body_obj(Req),
    assert_no_queries_param(couch_mrview_util:get_view_queries(Props)),
    Keys = couch_mrview_util:get_view_keys(Props),
    couch_stats:increment_counter([couchdb, httpd, view_reads]),
    design_doc_post_view(Req, Props, Db, DDoc, ViewName, Keys);

handle_view_req(Req, _Db, _DDoc) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,HEAD").


% See https://github.com/apache/couchdb/issues/2168
assert_no_queries_param(undefined) ->
    ok;
assert_no_queries_param(_) ->
    throw({
        bad_request,
        "The `queries` parameter is no longer supported at this endpoint"
    }).


validate_args(Req, #mrargs{page_size = PageSize} = Args) when is_integer(PageSize) ->
    MaxPageSize = max_page_size(Req),
    couch_views_util:validate_args(Args, [{page_size, MaxPageSize}]);

validate_args(_Req, #mrargs{} = Args) ->
    couch_views_util:validate_args(Args, []).


max_page_size(#httpd{path_parts=[_Db, <<"_all_docs">>, <<"queries">>]}) ->
    config:get_integer(
        "request_limits", "_all_docs/queries", ?DEFAULT_ALL_DOCS_PAGE_SIZE);

max_page_size(#httpd{path_parts=[_Db, <<"_all_docs">>]}) ->
    config:get_integer(
        "request_limits", "_all_docs", ?DEFAULT_ALL_DOCS_PAGE_SIZE);

max_page_size(#httpd{path_parts=[_Db, <<"_local_docs">>, <<"queries">>]}) ->
    config:get_integer(
        "request_limits", "_all_docs/queries", ?DEFAULT_ALL_DOCS_PAGE_SIZE);

max_page_size(#httpd{path_parts=[_Db, <<"_local_docs">>]}) ->
    config:get_integer(
        "request_limits", "_all_docs", ?DEFAULT_ALL_DOCS_PAGE_SIZE);

max_page_size(#httpd{path_parts=[_Db, <<"_design_docs">>, <<"queries">>]}) ->
    config:get_integer(
        "request_limits", "_all_docs/queries", ?DEFAULT_ALL_DOCS_PAGE_SIZE);

max_page_size(#httpd{path_parts=[_Db, <<"_design_docs">>]}) ->
    config:get_integer(
        "request_limits", "_all_docs", ?DEFAULT_ALL_DOCS_PAGE_SIZE);

max_page_size(#httpd{path_parts=[
        _Db, <<"_design">>, _DDocName, <<"_view">>, _View, <<"queries">>]}) ->
    config:get_integer(
        "request_limits", "_view/queries", ?DEFAULT_VIEWS_PAGE_SIZE);

max_page_size(#httpd{path_parts=[
        _Db, <<"_design">>, _DDocName, <<"_view">>, _View]}) ->
    config:get_integer(
        "request_limits", "_view", ?DEFAULT_VIEWS_PAGE_SIZE).


parse_queries(Req, #mrargs{page_size = PageSize} = Args0, Queries, Fun)
        when is_integer(PageSize) ->
    MaxPageSize = max_page_size(Req),
    if length(Queries) < PageSize -> ok; true ->
        throw({
            query_parse_error,
            <<"Provided number of queries is more than given page_size">>
        })
    end,
    couch_views_util:validate_args(Fun(Args0), [{page_size, MaxPageSize}]),
    Args = Args0#mrargs{page_size = undefined},
    lists:map(fun({Query}) ->
        Args1 = couch_views_http:parse_params(Query, undefined, Args, [decoded]),
        if not is_integer(Args1#mrargs.page_size) -> ok; true ->
            throw({
               query_parse_error,
                <<"You cannot specify `page_size` inside the query">>
            })
        end,
        Args2 = maybe_set_page_size(Args1, MaxPageSize),
        couch_views_util:validate_args(Fun(Args2), [{page_size, MaxPageSize}])
    end, Queries);

parse_queries(_Req, #mrargs{} = Args, Queries, Fun) ->
    lists:map(fun({Query}) ->
        Args1 = couch_views_http:parse_params(Query, undefined, Args, [decoded]),
        couch_views_util:validate_args(Fun(Args1))
    end, Queries).


maybe_set_page_size(#mrargs{page_size = undefined} = Args, MaxPageSize) ->
    Args#mrargs{page_size = MaxPageSize};

maybe_set_page_size(#mrargs{} = Args, _MaxPageSize) ->
    Args.


-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


check_multi_query_reduce_view_overrides_test_() ->
    {
        setup,
        fun setup_all/0,
        fun teardown_all/1,
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                t_check_include_docs_throw_validation_error(),
                t_check_user_can_override_individual_query_type()
            ]
        }
    }.


t_check_include_docs_throw_validation_error() ->
    ?_test(begin
        Req = #httpd{qs = []},
        Db = #{name => <<"foo">>},
        Query = {[{<<"include_docs">>, true}]},
        Throw = {query_parse_error, <<"`include_docs` is invalid for reduce">>},
        ?assertThrow(Throw, multi_query_view(Req, Db, ddoc, <<"v">>, [Query]))
    end).


t_check_user_can_override_individual_query_type() ->
    ?_test(begin
        Req = #httpd{qs = []},
        Db = #{name => <<"foo">>},
        Query = {[{<<"include_docs">>, true}, {<<"reduce">>, false}]},
        multi_query_view(Req, Db, ddoc, <<"v">>, [Query]),
        ?assertEqual(1, meck:num_calls(chttpd, start_delayed_json_response, '_'))
    end).


setup_all() ->
    Views = [#mrview{reduce_funs = [{<<"v">>, <<"_count">>}]}],
    meck:expect(couch_mrview_util, ddoc_to_mrst, 2, {ok, #mrst{views = Views}}),
    meck:expect(chttpd, start_delayed_json_response, 4, {ok, resp}),
    meck:expect(couch_views, query, 6, {ok, #vacc{}}),
    meck:expect(chttpd, send_delayed_chunk, 2, {ok, resp}),
    meck:expect(chttpd, end_delayed_json_response, 1, ok).


teardown_all(_) ->
    meck:unload().


setup() ->
    meck:reset([
        chttpd,
        couch_views,
        couch_mrview_util
    ]).


teardown(_) ->
    ok.


-endif.
