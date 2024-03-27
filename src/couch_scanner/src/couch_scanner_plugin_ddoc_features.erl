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

% Scanner plugin to detect various design document features
%
% By default when enabled it will scan design documents for features slated to be
% deprecated in 4.0 such as: rewrites, lists, shows, updates. There are options
% to enable other feature checks: custom JS reducers, JS libraries, VDU functions,
% and JS filters.
%
% By default the scanning will start and run on the first node of the cluster only.
% It's possible to make the plugin run on all the and each node will pick a fraction
% of dbs to scan. That will make the scan go fast but might consume more resources. That options
% is contorlled via the run_on_first_node boolean setting.
%
% When features are detected they are aggregated per database and reported once all the
% design documents for a particular database have been processed. To get details about
% the specific design document enable the ddoc_report = true setting.
%

-module(couch_scanner_plugin_ddoc_features).
-behaviour(couch_scanner_plugin).

-export([
    start/2,
    resume/2,
    stop/1,
    db/2,
    ddoc/3
]).

-include_lib("couch_scanner/include/couch_scanner_plugin.hrl").

-record(st, {
    sid,
    dbname,
    report = #{},
    updates = true,
    rewrites = true,
    shows = true,
    lists = true,
    filters = false,
    libs = false,
    jsreducers = false,
    vdus = false,
    run_on_first_node = true,
    ddoc_report = false
}).

% DDoc fields

-define(FILTERS, <<"filters">>).
-define(VIEWS, <<"views">>).
-define(REDUCE, <<"reduce">>).
-define(LIB, <<"lib">>).
-define(VDU, <<"validate_doc_update">>).
-define(UPDATES, <<"updates">>).
-define(SHOWS, <<"shows">>).
-define(LISTS, <<"lists">>).
-define(REWRITES, <<"rewrites">>).

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

resume(SId, #{}) ->
    St = init_config(#st{sid = SId}),
    case should_run(St) of
        true ->
            ?INFO("Resuming.", [], #{sid => SId}),
            {ok, St};
        false ->
            ?INFO("Not resuming. Not on first node.", [], #{sid => SId}),
            skip
    end.

stop(#st{sid = SId, dbname = DbName, report = Total} = St) ->
    report_per_db(St, DbName, Total),
    ?INFO("Stopped", [], #{sid => SId}),
    {ok, #{<<"sid">> => SId}}.

db(#st{} = St, DbName) ->
    % If we run on all nodes spread db checks across nodes
    case should_pick_db(DbName, St) of
        true -> {ok, St};
        false -> {skip, St}
    end.

ddoc(#st{} = St, _DbName, #doc{id = <<"_design/_", _/binary>>}) ->
    % These are auto-inserted ddocs _design/_auth, etc.
    {ok, St};
ddoc(#st{} = St, DbName, #doc{} = DDoc) ->
    #doc{body = {Props = [_ | _]}} = DDoc,
    case couch_util:get_value(<<"language">>, Props, <<"javascript">>) of
        <<"javascript">> -> {ok, check_ddoc(St, DbName, DDoc)};
        _ -> {ok, St}
    end.

% Private

init_config(#st{} = St) ->
    St#st{
        updates = cfg_bool("updates", St#st.updates),
        rewrites = cfg_bool("rewrites", St#st.rewrites),
        shows = cfg_bool("shows", St#st.shows),
        lists = cfg_bool("lists", St#st.lists),
        filters = cfg_bool("filters", St#st.filters),
        libs = cfg_bool("libs", St#st.libs),
        jsreducers = cfg_bool("jsreducers", St#st.jsreducers),
        vdus = cfg_bool("vdus", St#st.vdus),
        run_on_first_node = cfg_bool("run_first_node", St#st.run_on_first_node),
        ddoc_report = cfg_bool("ddoc_report", St#st.ddoc_report)
    }.

cfg_bool(Key, Default) when is_list(Key), is_boolean(Default) ->
    config:get_boolean(atom_to_list(?MODULE), Key, Default).

should_run(#st{run_on_first_node = true}) ->
    hd(mem3_util:live_nodes()) =:= node();
should_run(#st{run_on_first_node = false}) ->
    true.

should_pick_db(_DbName, #st{run_on_first_node = true}) ->
    true;
should_pick_db(DbName, #st{run_on_first_node = false}) ->
    Nodes = mem3_util:live_nodes(),
    hd(mem3_util:rotate_list(DbName, Nodes)) =:= node().

check_ddoc(#st{} = St, DbName, #doc{} = DDoc0) ->
    #doc{id = DDocId, body = Body} = DDoc0,
    DDoc = couch_scanner_plugin:ejson_map(Body),
    {St1, Report} = maps:fold(fun check/3, {St, #{}}, DDoc),
    report(St1, DbName, DDocId, Report).

check(?UPDATES, #{} = Map, {#st{updates = true} = St, Acc}) ->
    bump(St, updates, map_size(Map), Acc);
check(?REWRITES, #{} = Map, {#st{rewrites = true} = St, Acc}) ->
    bump(St, rewrites, map_size(Map), Acc);
check(?SHOWS, #{} = Map, {#st{shows = true} = St, Acc}) ->
    bump(St, shows, map_size(Map), Acc);
check(?LISTS, #{} = Map, {#st{lists = true} = St, Acc}) ->
    bump(St, lists, map_size(Map), Acc);
check(?FILTERS, #{} = Map, {#st{filters = true} = St, Acc}) ->
    bump(St, filters, map_size(Map), Acc);
check(?VDU, _, {#st{vdus = true} = St, Acc}) ->
    bump(St, vdus, 1, Acc);
check(?LIB, _, {#st{libs = true} = St, Acc}) ->
    bump(St, libs, 1, Acc);
check(?VIEWS, #{} = Views, {#st{jsreducers = true} = St, Acc}) ->
    maps:fold(fun check_view/3, {St, Acc}, Views);
check(<<_/binary>>, _, {#st{} = St, Acc}) ->
    {St, Acc}.

check_view(_Name, #{?REDUCE := <<"_", _/binary>>}, {#st{} = St, Acc}) ->
    % Built-in reducers
    {St, Acc};
check_view(_Name, #{?REDUCE := <<_/binary>>}, {#st{} = St, Acc}) ->
    bump(St, jsreducers, 1, Acc);
check_view(_Name, _, {#st{} = St, Acc}) ->
    {St, Acc}.

bump(#st{} = St, Field, N, #{} = Map) ->
    {St, maps:update_with(Field, fun(V) -> V + N end, N, Map)}.

report(#st{} = St, _, _, #{} = Report) when map_size(Report) == 0 ->
    St;
report(#st{} = St, DbName, DDocId, #{} = Report) ->
    #st{report = Total, dbname = PrevDbName} = St,
    report_per_ddoc(#st{} = St, DbName, DDocId, Report),
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

report_per_ddoc(#st{ddoc_report = false}, _DbName, _DDocId, _Report) ->
    ok;
report_per_ddoc(#st{ddoc_report = true, sid = SId}, DbName, DDocId, Report) ->
    {Fmt, Args} = report_fmt(Report),
    Meta = #{sid => SId, db => DbName, ddoc => DDocId},
    ?WARN(Fmt, Args, Meta).

report_fmt(Report) ->
    Sorted = lists:sort(maps:to_list(Report)),
    FmtArgs = [{"~s:~p ", [K, V]} || {K, V} <- Sorted],
    {Fmt1, Args1} = lists:unzip(FmtArgs),
    Fmt2 = lists:flatten(Fmt1),
    Args2 = lists:flatten(Args1),
    {Fmt2, Args2}.
