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

-export([handle_view_req/3, handle_temp_view_req/2]).

multi_query_view(Req, Db, DDoc, ViewName, Queries) ->
    Args0 = couch_mrview_http:parse_params(Req, undefined),
    {ok, #mrst{views=Views}} = couch_mrview_util:ddoc_to_mrst(Db, DDoc),
    Args1 = couch_mrview_util:set_view_type(Args0, ViewName, Views),
    ArgQueries = lists:map(fun({Query}) ->
        QueryArg = couch_mrview_http:parse_params(Query, undefined,
            Args1, [decoded]),
        QueryArg1 = couch_mrview_util:set_view_type(QueryArg, ViewName, Views),
        couch_mrview_util:validate_args(QueryArg1)
    end, Queries),
    Options = [{user_ctx, Req#httpd.user_ctx}],
    VAcc0 = #vacc{db=Db, req=Req, prepend="\r\n"},
    FirstChunk = "{\"results\":[",
    {ok, Resp0} = chttpd:start_delayed_json_response(VAcc0#vacc.req, 200, [], FirstChunk),
    VAcc1 = VAcc0#vacc{resp=Resp0},
    VAcc2 = lists:foldl(fun(Args, Acc0) ->
        {ok, Acc1} = fabric:query_view(Db, Options, DDoc, ViewName,
            fun couch_mrview_http:view_cb/2, Acc0, Args),
        Acc1
    end, VAcc1, ArgQueries),
    {ok, Resp1} = chttpd:send_delayed_chunk(VAcc2#vacc.resp, "\r\n]}"),
    chttpd:end_delayed_json_response(Resp1).


design_doc_view(Req, Db, DDoc, ViewName, Keys) ->
    Args = couch_mrview_http:parse_params(Req, Keys),
    Max = chttpd:chunked_response_buffer_size(),
    VAcc = #vacc{db=Db, req=Req, threshold=Max},
    Options = [{user_ctx, Req#httpd.user_ctx}],
    {ok, Resp} = fabric:query_view(Db, Options, DDoc, ViewName,
        fun couch_mrview_http:view_cb/2, VAcc, Args),
    {ok, Resp#vacc.resp}.

handle_view_req(#httpd{method='GET',
        path_parts=[_, _, _, _, ViewName]}=Req, Db, DDoc) ->
    couch_stats:increment_counter([couchdb, httpd, view_reads]),
    Keys = chttpd:qs_json_value(Req, "keys", undefined),
    design_doc_view(Req, Db, DDoc, ViewName, Keys);

handle_view_req(#httpd{method='POST',
        path_parts=[_, _, _, _, ViewName]}=Req, Db, DDoc) ->
    chttpd:validate_ctype(Req, "application/json"),
    Props = couch_httpd:json_body_obj(Req),
    Keys = couch_mrview_util:get_view_keys(Props),
    Queries = couch_mrview_util:get_view_queries(Props),
    case {Queries, Keys} of
        {Queries, undefined} when is_list(Queries) ->
            [couch_stats:increment_counter([couchdb, httpd, view_reads]) || _I <- Queries],
            multi_query_view(Req, Db, DDoc, ViewName, Queries);
        {undefined, Keys} when is_list(Keys) ->
            couch_stats:increment_counter([couchdb, httpd, view_reads]),
            design_doc_view(Req, Db, DDoc, ViewName, Keys);
        {undefined, undefined} ->
            throw({
                bad_request,
                "POST body must contain `keys` or `queries` field"
            });
        {_, _} ->
            throw({bad_request, "`keys` and `queries` are mutually exclusive"})
    end;

handle_view_req(Req, _Db, _DDoc) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,HEAD").

handle_temp_view_req(Req, _Db) ->
    Msg = <<"Temporary views are not supported in CouchDB">>,
    chttpd:send_error(Req, 410, gone, Msg).



-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").


check_multi_query_reduce_view_overrides_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            t_check_include_docs_throw_validation_error(),
            t_check_user_can_override_individual_query_type()
        ]
    }.


t_check_include_docs_throw_validation_error() ->
    ?_test(begin
        Req = #httpd{qs = []},
        Query = {[{<<"include_docs">>, true}]},
        Throw = {query_parse_error, <<"`include_docs` is invalid for reduce">>},
        ?assertThrow(Throw, multi_query_view(Req, db, ddoc, <<"v">>, [Query]))
    end).


t_check_user_can_override_individual_query_type() ->
    ?_test(begin
        Req = #httpd{qs = []},
        Query = {[{<<"include_docs">>, true}, {<<"reduce">>, false}]},
        multi_query_view(Req, db, ddoc, <<"v">>, [Query]),
        ?assertEqual(1, meck:num_calls(chttpd, start_delayed_json_response, '_'))
    end).


setup() ->
    Views = [#mrview{reduce_funs = [{<<"v">>, <<"_count">>}]}],
    meck:expect(couch_mrview_util, ddoc_to_mrst, 2, {ok, #mrst{views = Views}}),
    meck:expect(chttpd, start_delayed_json_response, 4, {ok, resp}),
    meck:expect(fabric, query_view, 7, {ok, #vacc{}}),
    meck:expect(chttpd, send_delayed_chunk, 2, {ok, resp}),
    meck:expect(chttpd, end_delayed_json_response, 1, ok).


teardown(_) ->
    meck:unload().


-endif.
