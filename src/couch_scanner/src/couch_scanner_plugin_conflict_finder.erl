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

-module(couch_scanner_plugin_conflict_finder).
-behaviour(couch_scanner_plugin).

-export([
    start/2,
    resume/2,
    complete/1,
    checkpoint/1,
    db/2,
    doc_id/3
]).

-include_lib("couch_scanner/include/couch_scanner_plugin.hrl").

-record(st, {
    sid,
    opts = #{},
    dbname,
    report = #{},
    run_on_first_node = true,
    doc_report = false
}).

-define(CONFLICTS, <<"conflicts">>).
-define(DELETED_CONFLICTS, <<"deleted_conflicts">>).

-define(OPTS, #{
    ?CONFLICTS => true,
    ?DELETED_CONFLICTS => false
}).

% Behavior callbacks

start(SId, #{}) ->
    St = init_config(#st{sid = SId}),
    case should_run(St) of
        true ->
            ?INFO("Starting.", [], #{sid => SId}),
            {ok, St};
        false ->
            ?INFO("Not starting. Not on first node.", [], #{sid => SId}),
            skip
    end.

resume(SId, #{<<"opts">> := OldOpts}) ->
    St = init_config(#st{sid = SId}),
    case {OldOpts == St#st.opts, should_run(St)} of
        {true, true} ->
            ?INFO("Resuming.", [], #{sid => SId}),
            {ok, St};
        {false, true} ->
            ?INFO("Resetting. Config changed.", [], #{sid => SId}),
            reset;
        {_, false} ->
            ?INFO("Not resuming. Not on first node.", [], #{sid => SId}),
            skip
    end.

complete(#st{sid = SId, dbname = DbName, report = Report} = St) ->
    report_per_db(St, DbName, Report),
    ?INFO("Completed", [], #{sid => SId}),
    {ok, #{}}.

checkpoint(#st{sid = SId, opts = Opts}) ->
    case Opts == opts() of
        true ->
            {ok, #{<<"opts">> => Opts}};
        false ->
            ?INFO("Resetting. Config changed.", [], #{sid => SId}),
            reset
    end.

db(#st{} = St, DbName) ->
    case St#st.run_on_first_node of
        true ->
            {ok, St};
        false ->
            % If we run on all nodes spread db checks across nodes
            case couch_scanner_util:consistent_hash_nodes(DbName) of
                true -> {ok, St};
                false -> {skip, St}
            end
    end.

doc_id(#st{} = St, <<?DESIGN_DOC_PREFIX, _/binary>>, _Db) ->
    {skip, St};
doc_id(#st{} = St, DocId, Db) ->
    {ok, #doc_info{revs = Revs}} = couch_db:get_doc_info(Db, DocId),
    DbName = mem3:dbname(couch_db:name(Db)),
    {ok, check(St, DbName, DocId, Revs)}.

% Private

init_config(#st{} = St) ->
    St#st{
        opts = opts(),
        run_on_first_node = cfg_bool("run_on_first_node", St#st.run_on_first_node)
    }.

should_run(#st{run_on_first_node = true}) ->
    couch_scanner_util:on_first_node();
should_run(#st{run_on_first_node = false}) ->
    true.

check(#st{} = St, _, _, Revs) when length(Revs) =< 1 ->
    St;
check(#st{doc_report = true, opts = Opts} = St, DbName, DocId, Revs) ->
    {DeletedConflicts, Conflicts} =
        lists:partition(fun(R) -> R#rev_info.deleted end, Revs),
    ConflictsReport = gen_report(doc, ?CONFLICTS, Opts, Conflicts),
    DeletedConflictsReport = gen_report(doc, ?DELETED_CONFLICTS, Opts, DeletedConflicts),
    DocReport = maps:merge(ConflictsReport, DeletedConflictsReport),
    report_per_doc(#st{} = St, DbName, DocId, DocReport),
    DbReport = maps:from_list([{K, tuple_size(V)} || {K, V} <- maps:to_list(DocReport)]),
    report(#st{} = St, DbName, DbReport);
check(#st{opts = Opts} = St, DbName, _DocId, Revs) ->
    {DeletedConflicts, Conflicts} =
        lists:partition(fun(R) -> R#rev_info.deleted end, Revs),
    ConflictsReport = gen_report(db, ?CONFLICTS, Opts, Conflicts),
    DeletedConflictsReport = gen_report(db, ?DELETED_CONFLICTS, Opts, DeletedConflicts),
    DbReport = maps:merge(ConflictsReport, DeletedConflictsReport),
    report(#st{} = St, DbName, DbReport).

gen_report(db, ?CONFLICTS, #{?CONFLICTS := true}, Revs) ->
    #{?CONFLICTS => length(Revs)};
gen_report(db, ?DELETED_CONFLICTS, #{?DELETED_CONFLICTS := true}, Revs) ->
    #{?DELETED_CONFLICTS => length(Revs)};
gen_report(doc, ?CONFLICTS, #{?CONFLICTS := true}, Revs) ->
    #{?CONFLICTS => list_to_tuple([couch_doc:rev_to_str(R#rev_info.rev) || R <- Revs])};
gen_report(doc, ?DELETED_CONFLICTS, #{?DELETED_CONFLICTS := true}, Revs) ->
    #{?DELETED_CONFLICTS => list_to_tuple([couch_doc:rev_to_str(R#rev_info.rev) || R <- Revs])};
gen_report(_Type, _Key, #{} = _Opts, _Revs) ->
    #{}.

report(#st{} = St, DbName, Report) ->
    #st{report = Total, dbname = PrevDbName} = St,
    case is_binary(PrevDbName) andalso DbName =/= PrevDbName of
        true ->
            % We switched dbs, so report stats for old db
            % and make the new one the current one
            report_per_db(St, PrevDbName, Total),
            St#st{report = Report, dbname = DbName};
        false ->
            % Keep accumulating per-db stats
            St#st{report = merge_report(Total, Report), dbname = DbName}
    end.

merge_report(#{} = Total, #{} = Update) ->
    Fun = fun(_K, V1, V2) -> V1 + V2 end,
    maps:merge_with(Fun, Total, Update).

report_per_db(#st{sid = SId}, DbName, #{} = Report) when
    map_size(Report) > 0, is_binary(DbName)
->
    {Fmt, Args} = report_fmt(Report),
    Meta = #{sid => SId, db => DbName},
    ?WARN(Fmt, Args, Meta);
report_per_db(#st{}, _, _) ->
    ok.

report_per_doc(#st{sid = SId}, DbName, DocId, Report) when
    map_size(Report) > 0, is_binary(DbName)
->
    {Fmt, Args} = report_fmt(Report),
    Meta = #{sid => SId, db => DbName, doc => DocId},
    ?WARN(Fmt, Args, Meta);
report_per_doc(#st{}, _, _, _) ->
    ok.

report_fmt(Report) ->
    Sorted = lists:sort(maps:to_list(Report)),
    FmtArgs = [{"~s:~p ", [K, V]} || {K, V} <- Sorted],
    {Fmt1, Args1} = lists:unzip(FmtArgs),
    Fmt2 = lists:flatten(Fmt1),
    Args2 = lists:flatten(Args1),
    {Fmt2, Args2}.

opts() ->
    Fun = fun(Key, Default) -> cfg_bool(binary_to_list(Key), Default) end,
    maps:map(Fun, ?OPTS).

cfg_bool(Key, Default) when is_list(Key), is_boolean(Default) ->
    config:get_boolean(atom_to_list(?MODULE), Key, Default).
