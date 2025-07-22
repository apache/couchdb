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

-module(csrt).

-include_lib("couch/include/couch_db.hrl").
-include_lib("csrt.hrl").

%% PidRef API
-export([
    destroy_pid_ref/0,
    destroy_pid_ref/1,
    create_pid_ref/0,
    get_pid_ref/0,
    get_pid_ref/1,
    set_pid_ref/1
]).

%% Context Lifecycle API
-export([
    create_context/2,
    create_coordinator_context/2,
    create_worker_context/3,
    destroy_context/0,
    destroy_context/1,
    get_resource/0,
    get_resource/1,
    set_context_dbname/1,
    set_context_dbname/2,
    set_context_handler_fun/1,
    set_context_handler_fun/2,
    set_context_username/1,
    set_context_username/2
]).

%% Public API
-export([
    clear_pdict_markers/0,
    do_report/2,
    is_enabled/0,
    is_enabled_init_p/0,
    is_enabled_reporting/0,
    is_enabled_rpc_reporting/0,
    maybe_report/2,
    to_json/1
]).

%% Stats Collection API
-export([
    accumulate_delta/1,
    add_delta/2,
    docs_written/1,
    extract_delta/1,
    get_delta/0,
    inc/1,
    inc/2,
    ioq_called/0,
    js_filtered/1,
    make_delta/0,
    rctx_delta/2,
    maybe_add_delta/1,
    maybe_add_delta/2,
    maybe_inc/2,
    should_track_init_p/1
]).

%% RPC API
-export([
    rpc_run/1,
    rpc_unsafe_run/1
]).

%% Aggregate Query API
-export([
    active/0,
    active/1,
    active_coordinators/0,
    active_coordinators/1,
    active_workers/0,
    active_workers/1,
    find_by_nonce/1,
    find_by_pid/1,
    find_by_pidref/1,
    find_workers_by_pidref/1,
    query_matcher/1,
    query_matcher/2
]).

%% Recon API Ports of https://github.com/ferd/recon/releases/tag/2.5.6
-export([
    pid_ref_attrs/1,
    pid_ref_matchspec/1,
    proc_window/3
]).

-export([
    query/1,
    from/1,
    group_by/1,
    group_by/2,
    sort_by/1,
    sort_by/2,
    count_by/1,
    options/1,
    unlimited/0,
    with_limit/1,

    run/1,
    unsafe_run/1
]).

-export_type([
    query/0,
    query_expression/0,
    query_option/0
]).

-opaque query() :: csrt_query:query().
-opaque query_expression() :: csrt_query:query_expression().
-opaque query_option() :: csrt_query:query_option().

%%
%% RPC Operations
%%

-spec rpc_run(Query :: query()) ->
    [
        #{
            node => node(),
            result => [{aggregation_key(), pos_integer()}],
            errors => [atom()]
        }
    ].
rpc_run(Query) ->
    Nodes = mem3:nodes(),
    merge_results(Nodes, erpc:multicall(Nodes, ?MODULE, run, [Query])).

-spec rpc_unsafe_run(Query :: query()) ->
    [
        #{
            node => node(),
            result => [{aggregation_key(), pos_integer()}],
            errors => [atom()]
        }
    ].
rpc_unsafe_run(Query) ->
    Nodes = mem3:nodes(),
    merge_results(Nodes, erpc:multicall(Nodes, ?MODULE, unsafe_run, [Query])).

merge_results(Nodes, Resp) ->
    %% The result of erpc:multicall is returned as a list where the result from each
    %% node is placed at the same position as the node name is placed in Nodes.
    %% That is why we can use `lists:zip/2` here.
    lists:map(fun format_response/1, lists:zip(Nodes, Resp)).

format_response({Node, {ok, {ok, Result}}}) ->
    #{
        node => Node,
        result => Result,
        errors => []
    };
format_response({Node, {ok, {error, Reason}}}) ->
    #{
        node => Node,
        result => none,
        errors => [Reason]
    };
format_response({Node, {ok, Result}}) ->
    #{
        node => Node,
        result => Result,
        errors => []
    };
format_response({Node, {error, {erpc, Reason}}}) ->
    #{
        node => Node,
        result => none,
        errors => [Reason]
    };
format_response({Node, {Tag, _}}) ->
    #{
        node => Node,
        result => none,
        errors => [Tag]
    }.

%%
%% PidRef Operations
%%

-spec get_pid_ref() -> maybe_pid_ref().
get_pid_ref() ->
    csrt_util:get_pid_ref().

-spec get_pid_ref(Rctx :: rctx()) -> pid_ref().
get_pid_ref(Rctx) ->
    csrt_util:get_pid_ref(Rctx).

-spec set_pid_ref(PidRef :: pid_ref()) -> pid_ref().
set_pid_ref(PidRef) ->
    csrt_util:set_pid_ref(PidRef).

-spec create_pid_ref() -> pid_ref().
create_pid_ref() ->
    csrt_server:create_pid_ref().

-spec destroy_pid_ref() -> maybe_pid_ref().
destroy_pid_ref() ->
    destroy_pid_ref(get_pid_ref()).

%%destroy_pid_ref(undefined) ->
%%    undefined;
-spec destroy_pid_ref(PidRef :: maybe_pid_ref()) -> maybe_pid_ref().
destroy_pid_ref(_PidRef) ->
    erase(?PID_REF).

%%
%% Context Lifecycle API
%%

-spec create_worker_context(From, MFA, Nonce) -> pid_ref() | false when
    From :: pid_ref(), MFA :: mfa(), Nonce :: nonce().
create_worker_context(From, {M, F, _A}, Nonce) ->
    case is_enabled() of
        true ->
            Type = #rpc_worker{from = From, mod = M, func = F},
            create_context(Type, Nonce);
        false ->
            false
    end.

-spec create_coordinator_context(Httpd, Path) -> pid_ref() | false when
    Httpd :: #httpd{}, Path :: list().
create_coordinator_context(#httpd{method = Verb, nonce = Nonce}, Path0) ->
    case is_enabled() of
        true ->
            Path = list_to_binary([$/ | Path0]),
            Type = #coordinator{method = Verb, path = Path},
            create_context(Type, Nonce);
        false ->
            false
    end.

-spec create_context(Type :: rctx_type(), Nonce :: term()) -> pid_ref() | false.
create_context(Type, Nonce) ->
    Rctx = csrt_server:new_context(Type, Nonce),
    PidRef = get_pid_ref(Rctx),
    set_pid_ref(PidRef),
    try
        csrt_util:put_delta_a(Rctx),
        csrt_util:put_updated_at(Rctx),
        csrt_server:create_resource(Rctx),
        csrt_logger:track(Rctx),
        PidRef
    catch
        _:_ ->
            csrt_server:destroy_resource(PidRef),
            %% calling destroy_context(PidRef) clears the tracker too
            destroy_context(PidRef),
            false
    end.

-spec set_context_dbname(DbName :: binary()) -> boolean().
set_context_dbname(DbName) ->
    set_context_dbname(DbName, get_pid_ref()).

-spec set_context_dbname(DbName, PidRef) -> boolean() when
    DbName :: binary(), PidRef :: maybe_pid_ref().
set_context_dbname(_, undefined) ->
    false;
set_context_dbname(DbName, PidRef) ->
    is_enabled() andalso csrt_server:set_context_dbname(DbName, PidRef).

-spec set_context_handler_fun(Handler) -> boolean() when
    Handler :: function() | {atom(), atom()}.
set_context_handler_fun(Handler) ->
    set_context_handler_fun(Handler, get_pid_ref()).

-spec set_context_handler_fun(Handler, PidRef) -> boolean() when
    Handler :: function() | {atom(), atom()}, PidRef :: maybe_pid_ref().
set_context_handler_fun(_, undefined) ->
    false;
set_context_handler_fun(Fun, PidRef) when is_function(Fun) ->
    case is_enabled() of
        false ->
            false;
        true ->
            FProps = erlang:fun_info(Fun),
            Mod = proplists:get_value(module, FProps),
            Func = proplists:get_value(name, FProps),
            set_context_handler_fun({Mod, Func}, PidRef)
    end;
set_context_handler_fun({Mod, Func}, PidRef) ->
    case is_enabled() of
        false ->
            false;
        true ->
            csrt_server:set_context_handler_fun({Mod, Func}, PidRef)
    end.

%% @equiv set_context_username(User, get_pid_ref())
set_context_username(User) ->
    set_context_username(User, get_pid_ref()).

-spec set_context_username(User, PidRef) -> boolean() when
    User :: null | undefined | #httpd{} | #user_ctx{} | binary(),
    PidRef :: maybe_pid_ref().
set_context_username(null, _) ->
    false;
set_context_username(_, undefined) ->
    false;
set_context_username(#httpd{user_ctx = Ctx}, PidRef) ->
    set_context_username(Ctx, PidRef);
set_context_username(#user_ctx{name = Name}, PidRef) ->
    set_context_username(Name, PidRef);
set_context_username(UserName, PidRef) ->
    is_enabled() andalso csrt_server:set_context_username(UserName, PidRef).

-spec destroy_context() -> ok.
destroy_context() ->
    destroy_context(get_pid_ref()).

-spec destroy_context(PidRef :: maybe_pid_ref()) -> ok.
destroy_context(undefined) ->
    ok;
destroy_context(PidRef) ->
    %% Stopping the tracker clears the ets entry for PidRef on its way out
    csrt_logger:stop_tracker(),
    destroy_pid_ref(PidRef),
    clear_pdict_markers(),
    ok.

-spec clear_pdict_markers() -> ok.
clear_pdict_markers() ->
    ok = lists:foreach(
        fun
            ({{csrt, _} = K, _V}) ->
                erlang:erase(K);
            (_) ->
                ok
        end,
        erlang:get()
    ).

%%
%% Public API
%%

%% @equiv csrt_util:is_enabled().
-spec is_enabled() -> boolean().
is_enabled() ->
    csrt_util:is_enabled().

%% @equiv csrt_util:is_enabled_reporting().
-spec is_enabled_reporting() -> boolean().
is_enabled_reporting() ->
    csrt_util:is_enabled_reporting().

%% @equiv csrt_util:is_enabled_rpc_reporting().
-spec is_enabled_rpc_reporting() -> boolean().
is_enabled_rpc_reporting() ->
    csrt_util:is_enabled_rpc_reporting().

%% @equiv csrt_util:is_enabled_init_p().
-spec is_enabled_init_p() -> boolean().
is_enabled_init_p() ->
    csrt_util:is_enabled_init_p().

-spec get_resource() -> maybe_rctx().
get_resource() ->
    get_resource(get_pid_ref()).

-spec get_resource(PidRef :: maybe_pid_ref()) -> maybe_rctx().
get_resource(PidRef) ->
    csrt_server:get_resource(PidRef).

%% Log a CSRT report if any filters match
-spec maybe_report(ReportName :: string(), PidRef :: pid_ref()) -> ok.
maybe_report(ReportName, PidRef) ->
    csrt_logger:maybe_report(ReportName, PidRef).

%% Direct report logic skipping should log filters
-spec do_report(ReportName :: string(), PidRef :: pid_ref()) -> boolean().
do_report(ReportName, PidRef) ->
    csrt_logger:do_report(ReportName, get_resource(PidRef)).

-spec to_json(Rctx :: maybe_rctx()) -> map() | null.
to_json(undefined) ->
    null;
to_json(Rctx) ->
    csrt_entry:to_json(Rctx).

%%
%% Stat Collection API
%%

-spec inc(Key :: rctx_field()) -> non_neg_integer().
inc(Key) ->
    case is_enabled() of
        true ->
            csrt_server:inc(get_pid_ref(), Key);
        false ->
            0
    end.

-spec inc(Key :: rctx_field(), N :: non_neg_integer()) -> non_neg_integer().
inc(Key, N) when is_integer(N) andalso N >= 0 ->
    case is_enabled() of
        true ->
            csrt_server:inc(get_pid_ref(), Key, N);
        false ->
            0
    end.

-spec maybe_inc(Stat :: atom(), Val :: non_neg_integer()) -> non_neg_integer().
maybe_inc(Stat, Val) ->
    case maps:is_key(Stat, ?STATS_TO_KEYS) of
        true ->
            inc(maps:get(Stat, ?STATS_TO_KEYS), Val);
        false ->
            0
    end.

-spec should_track_init_p(Stat :: [atom()]) -> boolean().
%% "Example to extend CSRT"
%% should_track_init_p([fabric_rpc, foo, spawned]) ->
%%    is_enabled_init_p();
should_track_init_p([fabric_rpc, all_docs, spawned]) ->
    is_enabled_init_p();
should_track_init_p([fabric_rpc, changes, spawned]) ->
    is_enabled_init_p();
should_track_init_p([fabric_rpc, get_all_security, spawned]) ->
    is_enabled_init_p();
should_track_init_p([fabric_rpc, map_view, spawned]) ->
    is_enabled_init_p();
should_track_init_p([fabric_rpc, open_doc, spawned]) ->
    is_enabled_init_p();
should_track_init_p([fabric_rpc, open_shard, spawned]) ->
    is_enabled_init_p();
should_track_init_p([fabric_rpc, reduce_view, spawned]) ->
    is_enabled_init_p();
should_track_init_p([fabric_rpc, update_docs, spawned]) ->
    is_enabled_init_p();
should_track_init_p(_Metric) ->
    false.

-spec ioq_called() -> non_neg_integer().
ioq_called() ->
    inc(ioq_calls).

%% we cannot yet use stats couchdb.query_server.*.ddoc_filter because those
%% are collected in a dedicated process.
%% TODO: funnel back stats from background worker processes to the RPC worker
js_filtered(N) ->
    inc(js_filter),
    inc(js_filtered_docs, N).

docs_written(N) ->
    inc(docs_written, N).

-spec accumulate_delta(Delta :: map() | undefined) -> ok.
accumulate_delta(Delta) when is_map(Delta) ->
    is_enabled() andalso csrt_server:update_counters(get_pid_ref(), Delta),
    ok;
accumulate_delta(undefined) ->
    ok.

-spec make_delta() -> maybe_delta().
make_delta() ->
    case is_enabled() of
        false ->
            undefined;
        true ->
            csrt_util:make_delta(get_pid_ref())
    end.

-spec rctx_delta(TA :: maybe_rctx(), TB :: maybe_rctx()) -> maybe_delta().
rctx_delta(TA, TB) ->
    csrt_util:rctx_delta(TA, TB).

%%
%% Aggregate Query API
%%

-spec active() -> [rctx()].
active() ->
    csrt_query:active().

-spec active(Type :: json) -> [rctx()].
active(Type) ->
    csrt_query:active(Type).

-spec active_coordinators() -> [coordinator_rctx()].
active_coordinators() ->
    csrt_query:active_coordinators().

%% TODO: cleanup json logic here
-spec active_coordinators(Type :: json) -> [coordinator_rctx()].
active_coordinators(Type) ->
    csrt_query:active_coordinators(Type).

-spec active_workers() -> [rpc_worker_rctx()].
active_workers() ->
    csrt_query:active_workers().

-spec active_workers(Type :: json) -> [rpc_worker_rctx()].
active_workers(Type) ->
    csrt_query:active_workers(Type).

find_by_nonce(Nonce) ->
    csrt_query:find_by_nonce(Nonce).

find_by_pid(Pid) ->
    csrt_query:find_by_pid(Pid).

find_by_pidref(PidRef) ->
    csrt_query:find_by_pidref(PidRef).

find_workers_by_pidref(PidRef) ->
    csrt_query:find_workers_by_pidref(PidRef).

-spec pid_ref_matchspec(AttrName :: rctx_field()) -> term() | throw(any()).
pid_ref_matchspec(AttrName) ->
    csrt_logger:pid_ref_matchspec(AttrName).

-spec pid_ref_attrs(AttrName :: rctx_field()) -> term() | throw(any()).
pid_ref_attrs(AttrName) ->
    csrt_logger:pid_ref_attrs(AttrName).

%% This is a recon:proc_window/3 [1] port with the same core logic but
%% recon_lib:proc_attrs/1 replaced with csrt_logger:pid_ref_attrs/1, and
%% returning on pid_ref() rather than pid().
%% [1] https://github.com/ferd/recon/blob/c2a76855be3a226a3148c0dfc21ce000b6186ef8/src/recon.erl#L268-L300
-spec proc_window(AttrName, Num, Time) -> term() | throw(any()) when
    AttrName :: rctx_field(), Num :: non_neg_integer(), Time :: pos_integer().
proc_window(AttrName, Num, Time) ->
    csrt_logger:proc_window(AttrName, Num, Time).

-spec query_matcher(MatcherName :: matcher_name()) ->
    {ok, query_result()}
    | {error, any()}.
query_matcher(MatcherName) ->
    csrt_query:query_matcher(MatcherName).

-spec query_matcher(MatcherName :: matcher_name(), Limit :: pos_integer()) ->
    {ok, query_result()}
    | {error, any()}.
query_matcher(MatcherName, Limit) ->
    csrt_query:query_matcher(MatcherName, Limit).

%%
%% Delta API
%%

-spec add_delta(T :: term(), Delta :: maybe_delta()) -> term_delta().
add_delta(T, Delta) ->
    csrt_util:add_delta(T, Delta).

-spec extract_delta(T :: term_delta()) -> {term(), maybe_delta()}.
extract_delta(T) ->
    csrt_util:extract_delta(T).

-spec get_delta() -> tagged_delta().
get_delta() ->
    csrt_util:get_delta(get_pid_ref()).

-spec maybe_add_delta(T :: term()) -> term_delta().
maybe_add_delta(T) ->
    csrt_util:maybe_add_delta(T).

-spec maybe_add_delta(T :: term(), Delta :: maybe_delta()) -> term_delta().
maybe_add_delta(T, Delta) ->
    csrt_util:maybe_add_delta(T, Delta).

%%
%% Query API functions
%%
%%

%% @doc Construct query from the expressions.
%% There are following types of expressions allowed in the query.
%%   <li>group_by/1 @see group_by/1</li>
%%   <li>group_by/2 @see group_by/1</li>
%%   <li>sort_by/1 @see sort_by/1</li>
%%   <li>count_by/1 @see count_by/1</li>
%%   <li>options/1 @see options/1</li>
%%   <li>from/1 @see from/1</li>
%% The order of expressions doesn't matter.
%% <code>
%% Q = query([
%%    from("docs_read"),
%%    group_by(username, dbname, ioq_calls),
%%    options([
%%       with_limit(10)
%%    ])
%% ]),
%% </code>
%% @end
-spec query(QueryExpression :: [query_expression()]) ->
    query() | {error, any()}.
query(QueryExpression) ->
    csrt_query:query(QueryExpression).

%% @doc Specify the matcher to use for the query.
%% If atom 'all' is used then all entries would be in the scope of the query.
%% Also the use of 'all' makes the query 'unsafe'. Because it scans through all entries
%% and can return many matching rows.
%% Unsafe queries can only be run using 'unsafe_run/1'.
%% <code>
%% Q = query([
%%    ...
%%    from("docs_read")
%% ]),
%% </code>
%% @end
-spec from(MatcherNameOrAll :: string() | all) ->
    query_expression() | {error, any()}.
from(MatcherNameOrAll) ->
    csrt_query:from(MatcherNameOrAll).

%% @doc Request 'group_by' aggregation of results.
%% <code>
%% Q = query([
%%    ...
%%    group_by([username, dbname])
%% ]),
%% </code>
%% @end
-spec group_by(AggregationKeys) ->
    query_expression() | {error, any()}
when
    AggregationKeys ::
        binary()
        | rctx_field()
        | [binary()]
        | [rctx_field()].
group_by(AggregationKeys) ->
    csrt_query:group_by(AggregationKeys).

%% @doc Request 'group_by' aggregation of results.
%% <code>
%% Q = query([
%%    ...
%%    group_by([username, dbname], ioq_calls)
%% ]),
%% </code>
%% @end
-spec group_by(AggregationKeys, ValueKey) ->
    query_expression() | {error, any()}
when
    AggregationKeys ::
        binary()
        | rctx_field()
        | [binary()]
        | [rctx_field()],
    ValueKey ::
        binary()
        | rctx_field().
group_by(AggregationKeys, ValueKey) ->
    csrt_query:group_by(AggregationKeys, ValueKey).

%% @doc Request 'sort_by' aggregation of results.
%% <code>
%% Q = query([
%%    ...
%%    sort_by([username, dbname])
%% ]),
%% </code>
%% @end
-spec sort_by(AggregationKeys) ->
    query_expression() | {error, any()}
when
    AggregationKeys ::
        binary()
        | rctx_field()
        | [binary()]
        | [rctx_field()].
sort_by(AggregationKeys) ->
    csrt_query:sort_by(AggregationKeys).

%% @doc Request 'sort_by' aggregation of results.
%% <code>
%% Q = query([
%%    ...
%%    sort_by([username, dbname], ioq_calls)
%% ]),
%% </code>
%% @end
-spec sort_by(AggregationKeys, ValueKey) ->
    query_expression() | {error, any()}
when
    AggregationKeys ::
        binary()
        | rctx_field()
        | [binary()]
        | [rctx_field()],
    ValueKey ::
        binary()
        | rctx_field().
sort_by(AggregationKeys, ValueKey) ->
    csrt_query:sort_by(AggregationKeys, ValueKey).

%% @doc Request 'count_by' aggregation of results.
%% <code>
%% Q = query([
%%    ...
%%    count_by(username)
%% ]),
%% </code>
%% @end
-spec count_by(AggregationKeys) ->
    query_expression() | {error, any()}
when
    AggregationKeys ::
        binary()
        | rctx_field()
        | [binary()]
        | [rctx_field()].
count_by(AggregationKeys) ->
    csrt_query:count_by(AggregationKeys).

%% @doc Construct 'options' query expression.
%% There are following types of expressions allowed in the query.
%%   <li>unlimited/0 @see unlimited/0 (cannot be used with 'with_limit/1')</li>
%%   <li>with_limit/1 @see with_limit/1 (cannot be used with 'unlimited/0')</li>
%% The order of expressions doesn't matter.
%% <code>
%% Q = query([
%%    ...
%%    options([
%%      ...
%%    ])
%% ]),
%% </code>
%% @end
-spec options([query_option()]) ->
    query_expression() | {error, any()}.
options(OptionsExpression) ->
    csrt_query:options(OptionsExpression).

%% @doc Enable unlimited number of results from the query.
%% The use of 'unlimited' makes the query 'unsafe'. Because it can return many matching rows.
%% Unsafe queries can only be run using 'unsafe_run/1'.
%% <code>
%% Q = query([
%%    ...
%%    options([
%%      unlimited()
%%    ])
%% ]),
%% </code>
%% @end
-spec unlimited() ->
    query_expression().
unlimited() ->
    csrt_query:unlimited().

%% @doc Set limit on number of results returned from the query.
%% The construction of the query fail if the 'limit' is greater than
%% allowed for this cluster.
%% <code>
%% Q = query([
%%    ...
%%    options([
%%      with_limit(100)
%%    ])
%% ]),
%% </code>
%% @end
-spec with_limit(Limit :: pos_integer()) ->
    query_expression() | {error, any()}.
with_limit(Limit) ->
    csrt_query:with_limit(Limit).

%% @doc Executes provided query. Only 'safe' queries can be executed using 'run'.
%% The query considered 'unsafe' if any of the conditions bellow are met:
%%   <li>Query uses 'unlimited/0'</li>
%%   <li>Query uses 'from(all)'</li>
%% <code>
%% Q = query([
%%    from("docs_read"),
%%    group_by(username, dbname, ioq_calls),
%%    options([
%%       with_limit(10)
%%    ])
%% ]),
%% run(Q)
%% </code>
%% @end
-spec run(query()) ->
    {ok, [{aggregation_key(), pos_integer()}]}
    | {limit, [{aggregation_key(), pos_integer()}]}.
run(Query) ->
    csrt_query:run(Query).

%% @doc Executes provided query. This function is similar to 'run/1',
%% however it supports 'unsafe' queries. Be very careful using it.
%% Pay attention to cardinality of the result.
%% The query considered 'unsafe' if any of the conditions bellow are met:
%%   <li>Query uses 'unlimited/0'</li>
%%   <li>Query uses 'from(all)'</li>
%% <code>
%% Q = query([
%%    from("docs_read"),
%%    group_by(username, dbname, ioq_calls),
%%    options([
%%       with_limit(10)
%%    ])
%% ]),
%% unsafe_run(Q)
%% </code>
%% @end
-spec unsafe_run(query()) ->
    {ok, [{aggregation_key(), pos_integer()}]}
    | {limit, [{aggregation_key(), pos_integer()}]}.
unsafe_run(Query) ->
    csrt_query:unsafe_run(Query).

%%
%% Tests
%%

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

couch_stats_resource_tracker_test_() ->
    {
        foreach,
        fun setup/0,
        fun teardown/1,
        [
            ?TDEF_FE(t_should_track_init_p_enabled),
            ?TDEF_FE(t_should_not_track_init_p_enabled),
            ?TDEF_FE(t_should_not_track_init_p_disabled),
            ?TDEF_FE(t_static_map_translations),
            ?TDEF_FE(t_should_extract_fields_properly)
        ]
    }.

setup() ->
    Ctx = test_util:start_couch(),
    config:set_boolean(?CSRT, "randomize_testing", false, false),
    Ctx.

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

t_static_map_translations(_) ->
    %% Bit of a hack to delete duplicated rows_read between views and changes
    SingularStats = lists:delete(rows_read, maps:values(?STATS_TO_KEYS)),
    ?assert(lists:all(fun(E) -> maps:is_key(E, ?STAT_KEYS_TO_FIELDS) end, SingularStats)),
    %% TODO: properly handle ioq_calls field
    ?assertEqual(
        lists:sort(SingularStats),
        lists:sort(
            lists:foldl(
                fun(E, A) ->
                    %% Ignore fields regarding external processes
                    Deletions = [docs_written, ioq_calls, js_filter, js_filtered_docs],
                    case lists:member(E, Deletions) of
                        true ->
                            A;
                        false ->
                            [E | A]
                    end
                end,
                [],
                maps:keys(?STAT_KEYS_TO_FIELDS)
            )
        )
    ).

t_should_not_track_init_p_enabled(_) ->
    enable_init_p(),
    Metrics = [
        [couch_db, name, spawned],
        [couch_db, get_db_info, spawned],
        [couch_db, open, spawned],
        [fabric_rpc, get_purge_seq, spawned]
    ],
    [?assert(should_track_init_p(M) =:= false, M) || M <- Metrics].

t_should_track_init_p_enabled(_) ->
    enable_init_p(),
    [?assert(should_track_init_p(M), M) || M <- base_metrics()].

t_should_not_track_init_p_disabled(_) ->
    disable_init_p(),
    [?assert(should_track_init_p(M) =:= false, M) || M <- base_metrics()].

t_should_extract_fields_properly(_) ->
    Rctx = #rctx{},
    #{fields := Fields} = csrt_entry:record_info(),
    %% csrt_entry:value/2 throws on invalid fields, assert that the function succeeded
    TestField = fun(Field) ->
        try
            csrt_entry:value(Field, Rctx),
            true
        catch
            _:_ -> false
        end
    end,
    [?assert(TestField(Field)) || Field <- Fields].

enable_init_p() ->
    config:set(?CSRT, "enable_init_p", "true", false).

disable_init_p() ->
    config:set(?CSRT, "enable_init_p", "false", false).

base_metrics() ->
    [
        [fabric_rpc, all_docs, spawned],
        [fabric_rpc, changes, spawned],
        [fabric_rpc, map_view, spawned],
        [fabric_rpc, reduce_view, spawned],
        [fabric_rpc, get_all_security, spawned],
        [fabric_rpc, open_doc, spawned],
        [fabric_rpc, update_docs, spawned],
        [fabric_rpc, open_shard, spawned]
    ].

-endif.
