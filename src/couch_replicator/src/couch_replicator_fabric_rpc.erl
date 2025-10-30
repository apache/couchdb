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

-module(couch_replicator_fabric_rpc).

-export([
    docs/3
]).

-include_lib("fabric/include/fabric.hrl").
-include_lib("couch_mrview/include/couch_mrview.hrl").

docs(DbName, Options, Args0) ->
    set_io_priority(DbName, Options),
    #mrargs{skip = Skip, limit = Limit, extra = Extra} = Args0,
    Args = Args0#mrargs{skip = 0, limit = Skip + Limit},
    HealthThreshold = couch_replicator_scheduler:health_threshold(),
    {ok, Db} = couch_db:open_int(DbName, Options),
    Acc = {DbName, HealthThreshold, Extra},
    couch_mrview:query_all_docs(Db, Args, fun docs_cb/2, Acc).

docs_cb({meta, Meta}, Acc) ->
    ok = rexi:stream2({meta, Meta}),
    {ok, Acc};
docs_cb({row, Props}, {DbName, HealthThreshold, Options} = Acc) ->
    States = couch_util:get_value(filter_states, Options),
    Id = couch_util:get_value(id, Props),
    Doc = couch_util:get_value(doc, Props),
    case rep_doc_state(DbName, Id, Doc, States, HealthThreshold) of
        skip ->
            ok;
        Other ->
            ViewRow0 = fabric_view_row:from_props(Props, Options),
            ViewRow = fabric_view_row:set_doc(ViewRow0, Other),
            ok = rexi:stream2(ViewRow)
    end,
    {ok, Acc};
docs_cb(complete, Acc) ->
    ok = rexi:stream_last(complete),
    {ok, Acc}.

set_io_priority(DbName, Options) ->
    case lists:keyfind(io_priority, 1, Options) of
        {io_priority, Pri} ->
            erlang:put(io_priority, Pri);
        false ->
            erlang:put(io_priority, {interactive, DbName})
    end.

%% Get the state of the replication document. If it is found and has a terminal
%% state then it can be filtered and either included in the results or skipped.
%% If it is not in a terminal state, look it up in the local doc processor ETS
%% table. If it is there then filter by state. If it is not found there either
%% then mark it as `undecided` and let the coordinator try to fetch it. The
%% The idea is to do as much work as possible locally and leave the minimum
%% amount of work for the coordinator.
rep_doc_state(_Shard, <<"_design/", _/binary>>, _, _, _) ->
    skip;
rep_doc_state(Shard, Id, {[_ | _]} = Doc, States, HealthThreshold) ->
    DbName = mem3:dbname(Shard),
    DocInfo = couch_replicator:info_from_doc(DbName, Doc),
    case get_doc_state(DocInfo) of
        null ->
            % Fetch from local doc processor. If there, filter by state.
            % If not there, mark as undecided. Let coordinator figure it out.
            case
                couch_replicator_doc_processor:doc_lookup(
                    Shard,
                    Id,
                    HealthThreshold
                )
            of
                {ok, nil} ->
                    % Could have been just completed. Let the coordinator
                    % try to refetch it.
                    undecided;
                {ok, EtsInfo} ->
                    State = get_doc_state(EtsInfo),
                    couch_replicator_utils:filter_state(State, States, EtsInfo);
                {error, not_found} ->
                    undecided
            end;
        OtherState when is_atom(OtherState) ->
            couch_replicator_utils:filter_state(OtherState, States, DocInfo)
    end.

get_doc_state({Props}) ->
    couch_util:get_value(state, Props).

-ifdef(TEST).

-include_lib("couch/include/couch_eunit.hrl").

docs_test_() ->
    {
        foreach,
        fun() -> ok end,
        fun(_) -> ok end,
        [
            ?TDEF_FE(t_docs, 15)
        ]
    }.

t_docs(_) ->
    Options1 = [],
    Options2 = [{io_priority, priority}],
    QueryArgs1 = #mrargs{skip = 3, limit = 7, extra = extra},
    QueryArgs2 = #mrargs{skip = 0, limit = 10, extra = extra},
    Accumulator = {db_name, health_threshold, extra},
    meck:expect(couch_replicator_scheduler, health_threshold, [], meck:val(health_threshold)),
    meck:expect(couch_db, open_int, [db_name, '_'], meck:val({ok, db})),
    meck:expect(
        couch_mrview,
        query_all_docs,
        [db, QueryArgs2, '_', Accumulator],
        meck:val(all_docs)
    ),
    ?assertEqual(all_docs, docs(db_name, Options1, QueryArgs1)),
    IoPrio1 = get(io_priority),
    ?assertEqual({interactive, db_name}, IoPrio1),
    ?assertEqual(all_docs, docs(db_name, Options2, QueryArgs1)),
    IoPrio2 = get(io_priority),
    ?assertEqual(priority, IoPrio2).

docs_cb_test_() ->
    {
        foreach,
        fun() ->
            meck:new(mem3),
            meck:new(rexi)
        end,
        fun(_) -> meck:unload() end,
        [
            ?TDEF_FE(t_docs_cb_meta, 15),
            ?TDEF_FE(t_docs_cb_row_skip, 15),
            ?TDEF_FE(t_docs_cb_row, 15),
            ?TDEF_FE(t_docs_cb_complete, 15)
        ]
    }.

t_docs_cb_meta(_) ->
    Meta = {meta, meta},
    meck:expect(rexi, stream2, [Meta], meck:val(ok)),
    ?assertEqual({ok, accumulator}, docs_cb(Meta, accumulator)).

t_docs_cb_row_skip(_) ->
    Accumulator1 = {db_name, health_threshold, []},
    Accumulator2 = {db_name, health_threshold, [{view_row_map, true}]},
    Row = {row, [{id, <<"_design/ddoc">>}, {doc, doc}]},
    meck:reset(rexi),
    meck:expect(rexi, stream2, ['_'], undefined),
    ?assertEqual({ok, Accumulator1}, docs_cb(Row, Accumulator1)),
    ?assertNot(meck:called(rexi, stream2, '_')),
    meck:reset(rexi),
    meck:expect(rexi, stream2, ['_'], undefined),
    ?assertEqual({ok, Accumulator2}, docs_cb(Row, Accumulator2)),
    ?assertNot(meck:called(rexi, stream2, '_')).

t_docs_cb_row(_) ->
    Accumulator1 = {db_name, health_threshold, [{filter_states, []}]},
    Accumulator2 = {db_name, health_threshold, [{filter_states, []}, {view_row_map, true}]},
    Doc = {[{<<"_id">>, id}, {<<"_rev">>, rev}]},
    DocInfo1 = {[{state, other}]},
    DocInfo2 = {[{state, null}]},
    EtsInfo = {[{state, other}]},
    Row = {row, [{id, id}, {doc, Doc}]},
    ViewRow1 = #view_row{id = id, doc = DocInfo1},
    ViewRow2 = {view_row, #{id => id, doc => EtsInfo}},
    ViewRow3 = {view_row, #{id => id, doc => undecided}},
    meck:expect(mem3, dbname, [db_name], meck:val(mem3_db_name)),
    meck:expect(couch_replicator, info_from_doc, [mem3_db_name, Doc], meck:val(DocInfo1)),
    meck:expect(rexi, stream2, [ViewRow1], meck:val(ok)),
    ?assertEqual({ok, Accumulator1}, docs_cb(Row, Accumulator1)),
    meck:expect(couch_replicator, info_from_doc, [mem3_db_name, Doc], meck:val(DocInfo2)),
    meck:expect(
        couch_replicator_doc_processor,
        doc_lookup,
        [db_name, id, health_threshold],
        meck:val({ok, EtsInfo})
    ),
    meck:expect(rexi, stream2, [ViewRow2], meck:val(ok)),
    ?assertEqual({ok, Accumulator2}, docs_cb(Row, Accumulator2)),
    meck:expect(couch_replicator, info_from_doc, [mem3_db_name, Doc], meck:val(DocInfo2)),
    meck:expect(
        couch_replicator_doc_processor,
        doc_lookup,
        [db_name, id, health_threshold],
        meck:val({error, not_found})
    ),
    meck:expect(rexi, stream2, [ViewRow3], meck:val(ok)),
    ?assertEqual({ok, Accumulator2}, docs_cb(Row, Accumulator2)).

t_docs_cb_complete(_) ->
    meck:expect(rexi, stream_last, [complete], meck:val(ok)),
    ?assertEqual({ok, accumulator}, docs_cb(complete, accumulator)).

-endif.
