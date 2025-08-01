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
    doc_fdi/3
]).

-include_lib("couch_scanner/include/couch_scanner_plugin.hrl").

-record(st, {
    sid,
    opts = #{},
    dbname,
    report = #{},
    doc_report = true,
    max_revs = 10
}).

-define(CONFLICTS, <<"conflicts">>).
-define(DELETED_CONFLICTS, <<"deleted_conflicts">>).

-define(OPTS, #{
    ?CONFLICTS => true,
    ?DELETED_CONFLICTS => true
}).

% Behavior callbacks

start(SId, #{}) ->
    St = init_config(#st{sid = SId}),
    ?INFO("Starting.", [], #{sid => SId}),
    {ok, St}.

resume(SId, #{<<"opts">> := OldOpts}) ->
    St = init_config(#st{sid = SId}),
    case OldOpts == St#st.opts of
        true ->
            ?INFO("Resuming.", [], #{sid => SId}),
            {ok, St};
        false ->
            ?INFO("Resetting. Config changed.", [], #{sid => SId}),
            reset
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

db(#st{} = St, _DbName) ->
    {ok, St}.

doc_fdi(#st{} = St, #full_doc_info{} = FDI, Db) ->
    #doc_info{revs = Revs} = couch_doc:to_doc_info(FDI),
    DbName = mem3:dbname(couch_db:name(Db)),
    {ok, check(St, DbName, FDI#full_doc_info.id, Revs)}.

% Private

init_config(#st{} = St) ->
    St#st{
        opts = opts(),
        doc_report = cfg_bool("doc_report", St#st.doc_report),
        max_revs = cfg_int("max_revs", St#st.max_revs)
    }.

cfg_int(Key, Default) when is_list(Key), is_integer(Default) ->
    config:get_integer(atom_to_list(?MODULE), Key, Default).

cfg_bool(Key, Default) when is_list(Key), is_boolean(Default) ->
    config:get_boolean(atom_to_list(?MODULE), Key, Default).

opts() ->
    Fun = fun(Key, Default) -> cfg_bool(binary_to_list(Key), Default) end,
    maps:map(Fun, ?OPTS).

check(#st{} = St, _, _, Revs) when length(Revs) =< 1 ->
    St;
check(#st{doc_report = true, max_revs = Max, opts = Opts} = St, DbName, DocId, Revs) ->
    {DeletedConflicts, Conflicts} = lists:partition(fun(R) -> R#rev_info.deleted end, Revs),
    ConflictsReport = gen_report(doc, ?CONFLICTS, Opts, Conflicts, Max),
    DeletedConflictsReport = gen_report(doc, ?DELETED_CONFLICTS, Opts, DeletedConflicts, Max),
    DocReport = maps:merge(ConflictsReport, DeletedConflictsReport),
    report_per_doc(#st{} = St, DbName, DocId, DocReport),
    DbReport = maps:map(
        fun(_K, V) ->
            case V of
                V when is_list(V) -> length(V);
                N when is_number(N) -> N
            end
        end,
        DocReport
    ),
    report(#st{} = St, DbName, DbReport);
check(#st{max_revs = _Max, opts = Opts} = St, DbName, _DocId, Revs) ->
    {DeletedConflicts, Conflicts} =
        lists:partition(fun(R) -> R#rev_info.deleted end, Revs),
    ConflictsReport = gen_report(db, ?CONFLICTS, Opts, Conflicts, _Max),
    DeletedConflictsReport = gen_report(db, ?DELETED_CONFLICTS, Opts, DeletedConflicts, _Max),
    DbReport = maps:merge(ConflictsReport, DeletedConflictsReport),
    report(#st{} = St, DbName, DbReport).

gen_report(doc, ?CONFLICTS, #{?CONFLICTS := true}, Revs, Max) when length(Revs) =< Max ->
    #{?CONFLICTS => [?b2l(couch_doc:rev_to_str(R#rev_info.rev)) || R <- Revs]};
gen_report(doc, ?DELETED_CONFLICTS, #{?DELETED_CONFLICTS := true}, Revs, Max) when
    length(Revs) =< Max
->
    #{?DELETED_CONFLICTS => [?b2l(couch_doc:rev_to_str(R#rev_info.rev)) || R <- Revs]};
gen_report(_ReportType, ?CONFLICTS, #{?CONFLICTS := true}, Revs, _Max) ->
    #{?CONFLICTS => length(Revs)};
gen_report(_ReportType, ?DELETED_CONFLICTS, #{?DELETED_CONFLICTS := true}, Revs, _Max) ->
    #{?DELETED_CONFLICTS => length(Revs)};
gen_report(_ReportType, _Key, #{} = _Opts, _Revs, _Max) ->
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
    Args2 = flatten_one_level(Args1),
    {Fmt2, Args2}.

flatten_one_level(List) when is_list(List) ->
    case lists:flatten(List) =:= List of
        true ->
            List;
        false ->
            lists:append([
                case is_list(E) of
                    true -> E;
                    false -> [E]
                end
             || E <- List
            ])
    end.

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").
flatten_one_level_test() ->
    ?assertEqual([1, 2, 3], flatten_one_level([1, 2, 3])),
    ?assertEqual([1, 2, 3], flatten_one_level([[1], 2, 3])),
    ?assertEqual([1, 2, 3], flatten_one_level([[1, 2], 3])),
    ?assertEqual([1, 2, 3], flatten_one_level([[1], 2, [3]])),
    ?assertEqual([1, [2], 3], flatten_one_level([[1, [2]], 3])),
    ?assertEqual([1, [2], 3], flatten_one_level([[1, [2]], [3]])),
    ?assertEqual([1, [2, [3]]], flatten_one_level([1, [[2, [3]]]])),
    ?assertError(function_clause, flatten_one_level(wrong)).
-endif.
