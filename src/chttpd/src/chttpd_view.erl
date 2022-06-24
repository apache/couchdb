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
    {ok, #mrst{views = Views}} = couch_mrview_util:ddoc_to_mrst(Db, DDoc),
    Args1 = couch_mrview_util:set_view_type(Args0, ViewName, Views),
    ArgQueries = lists:map(
        fun({Query}) ->
            QueryArg = couch_mrview_http:parse_params(
                Query,
                undefined,
                Args1,
                [decoded]
            ),
            QueryArg1 = couch_mrview_util:set_view_type(QueryArg, ViewName, Views),
            fabric_util:validate_args(Db, DDoc, QueryArg1)
        end,
        Queries
    ),
    Options = [{user_ctx, Req#httpd.user_ctx}],
    VAcc0 = #vacc{db = Db, req = Req, prepend = "\r\n"},
    FirstChunk = "{\"results\":[",
    {ok, Resp0} = chttpd:start_delayed_json_response(VAcc0#vacc.req, 200, [], FirstChunk),
    VAcc1 = VAcc0#vacc{resp = Resp0},
    VAcc2 = lists:foldl(
        fun(Args, Acc0) ->
            {ok, Acc1} = fabric:query_view(
                Db,
                Options,
                DDoc,
                ViewName,
                fun view_cb/2,
                Acc0,
                Args
            ),
            Acc1
        end,
        VAcc1,
        ArgQueries
    ),
    {ok, Resp1} = chttpd:send_delayed_chunk(VAcc2#vacc.resp, "\r\n]}"),
    chttpd:end_delayed_json_response(Resp1).

design_doc_post_view(Req, Props, Db, DDoc, ViewName, Keys) ->
    Args = couch_mrview_http:parse_body_and_query(Req, Props, Keys),
    fabric_query_view(Db, Req, DDoc, ViewName, Args).

design_doc_view(Req, Db, DDoc, ViewName, Keys) ->
    Args = couch_mrview_http:parse_params(Req, Keys),
    fabric_query_view(Db, Req, DDoc, ViewName, Args).

fabric_query_view(Db, Req, DDoc, ViewName, Args) ->
    Max = chttpd:chunked_response_buffer_size(),
    VAcc = #vacc{db = Db, req = Req, threshold = Max},
    Options = [{user_ctx, Req#httpd.user_ctx}],
%    {ok, Resp} = fabric:query_view(Db, Options, DDoc, ViewName,
%            fun view_cb/2, VAcc, Args),
%    {ok, Resp#vacc.resp}.
%    % TODO: This might just be a debugging leftover, we might be able
%    %       to undo this by just returning {ok, Resp#vacc.resp}
%    %       However, this *might* be here because we need to handle
%    %       errors here now, because access might tell us to.
%    case fabric:query_view(Db, Options, DDoc, ViewName,
%            fun view_cb/2, VAcc, Args) of
%        {ok, Resp} ->
%            {ok, Resp#vacc.resp};
%        {error, Error} ->
%            throw(Error)
%    end.

    {ok, Resp} = fabric:query_view(
        Db,
        Options,
        DDoc,
        ViewName,
        fun view_cb/2,
        VAcc,
        Args
    ),
    {ok, Resp#vacc.resp}.

view_cb({row, Row} = Msg, Acc) ->
    case lists:keymember(doc, 1, Row) of
        true -> chttpd_stats:incr_reads();
        false -> ok
    end,
    chttpd_stats:incr_rows(),
    couch_mrview_http:view_cb(Msg, Acc);
view_cb(Msg, Acc) ->
    couch_mrview_http:view_cb(Msg, Acc).

handle_view_req(
    #httpd{
        method = 'POST',
        path_parts = [_, _, _, _, ViewName, <<"queries">>]
    } = Req,
    Db,
    DDoc
) ->
    chttpd:validate_ctype(Req, "application/json"),
    Props = couch_httpd:json_body_obj(Req),
    case couch_mrview_util:get_view_queries(Props) of
        undefined ->
            throw({bad_request, <<"POST body must include `queries` parameter.">>});
        Queries ->
            multi_query_view(Req, Db, DDoc, ViewName, Queries)
    end;
handle_view_req(
    #httpd{path_parts = [_, _, _, _, _, <<"queries">>]} = Req,
    _Db,
    _DDoc
) ->
    chttpd:send_method_not_allowed(Req, "POST");
handle_view_req(
    #httpd{
        method = 'GET',
        path_parts = [_, _, _, _, ViewName]
    } = Req,
    Db,
    DDoc
) ->
    couch_stats:increment_counter([couchdb, httpd, view_reads]),
    Keys = chttpd:qs_json_value(Req, "keys", undefined),
    design_doc_view(Req, Db, DDoc, ViewName, Keys);
handle_view_req(
    #httpd{
        method = 'POST',
        path_parts = [_, _, _, _, ViewName]
    } = Req,
    Db,
    DDoc
) ->
    chttpd:validate_ctype(Req, "application/json"),
    Props = couch_httpd:json_body_obj(Req),
    assert_no_queries_param(couch_mrview_util:get_view_queries(Props)),
    Keys = couch_mrview_util:get_view_keys(Props),
    couch_stats:increment_counter([couchdb, httpd, view_reads]),
    design_doc_post_view(Req, Props, Db, DDoc, ViewName, Keys);
handle_view_req(Req, _Db, _DDoc) ->
    chttpd:send_method_not_allowed(Req, "GET,POST,HEAD").

handle_temp_view_req(Req, _Db) ->
    Msg = <<"Temporary views are not supported in CouchDB">>,
    chttpd:send_error(Req, 410, gone, Msg).

% See https://github.com/apache/couchdb/issues/2168
assert_no_queries_param(undefined) ->
    ok;
assert_no_queries_param(_) ->
    throw({
        bad_request,
        "The `queries` parameter is no longer supported at this endpoint"
    }).

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
        Db = test_util:fake_db([{name, <<"foo">>}]),
        Query = {[{<<"include_docs">>, true}]},
        Throw = {query_parse_error, <<"`include_docs` is invalid for reduce">>},
        ?assertThrow(Throw, multi_query_view(Req, Db, ddoc, <<"v">>, [Query]))
    end).

t_check_user_can_override_individual_query_type() ->
    ?_test(begin
        Req = #httpd{qs = []},
        Db = test_util:fake_db([{name, <<"foo">>}]),
        Query = {[{<<"include_docs">>, true}, {<<"reduce">>, false}]},
        multi_query_view(Req, Db, ddoc, <<"v">>, [Query]),
        ?assertEqual(1, meck:num_calls(chttpd, start_delayed_json_response, '_'))
    end).

setup_all() ->
    Views = [#mrview{reduce_funs = [{<<"v">>, <<"_count">>}]}],
    meck:expect(couch_mrview_util, ddoc_to_mrst, 2, {ok, #mrst{views = Views}}),
    meck:expect(chttpd, start_delayed_json_response, 4, {ok, resp}),
    meck:expect(fabric, query_view, 7, {ok, #vacc{}}),
    meck:expect(chttpd, send_delayed_chunk, 2, {ok, resp}),
    meck:expect(chttpd, end_delayed_json_response, 1, ok).

teardown_all(_) ->
    meck:unload().

setup() ->
    meck:reset([
        chttpd,
        couch_mrview_util,
        fabric
    ]).

teardown(_) ->
    ok.

-endif.
