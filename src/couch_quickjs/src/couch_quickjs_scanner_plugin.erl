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

-module(couch_quickjs_scanner_plugin).
-behaviour(couch_scanner_plugin).

-export([
    start/2,
    resume/2,
    complete/1,
    checkpoint/1,
    db/2,
    ddoc/3,
    shards/2,
    db_opened/2,
    doc_id/3,
    doc/3,
    db_closing/2
]).

-include_lib("couch_scanner/include/couch_scanner_plugin.hrl").

-record(st, {
    sid,
    ddocs = #{},
    docs = [],
    docs_size = 0,
    qjs_proc,
    sm_proc,
    ddoc_cnt = 0,
    doc_cnt = 0,
    doc_step = 0,
    max_ddocs = 100,
    max_shards = 4,
    max_docs = 1000,
    max_step = 1000,
    % These match defaults in couch_mrview_updater
    max_batch_items = 100,
    max_batch_size = 16777216
}).

% DDoc fields

-define(FILTERS, <<"filters">>).
-define(VIEWS, <<"views">>).
-define(MAP, <<"map">>).
-define(REDUCE, <<"reduce">>).
-define(LIB, <<"lib">>).
-define(VDU, <<"validate_doc_update">>).

% Behavior callbacks

start(SId, #{}) ->
    case couch_server:with_spidermonkey() of
        true ->
            ?WARN("starting.", [], #{sid => SId}),
            {ok, init_config(#st{sid = SId})};
        false ->
            ?WARN("not starting. Spidermonkey not enabled", [], #{}),
            skip
    end.

resume(SId, #{}) ->
    case couch_server:with_spidermonkey() of
        true ->
            ?WARN("resuming", [], #{sid => SId}),
            {ok, init_config(#st{sid = SId})};
        false ->
            ?WARN("not resuming. Spidermonkey not enabled", [], #{}),
            skip
    end.

complete(#st{sid = SId}) ->
    ?WARN("completed", [], #{sid => SId}),
    {ok, #{}}.

checkpoint(#st{sid = SId}) ->
    ?INFO(#{sid => SId}),
    {ok, #{<<"sid">> => SId}}.

db(#st{sid = SId} = St, DbName) ->
    St1 = reset_per_db_state(St),
    ?INFO(#{sid => SId, db => DbName}),
    {ok, St1}.

ddoc(#st{sid = SId, ddoc_cnt = C, max_ddocs = M} = St, DbName, _) when C > M ->
    Meta = #{sid => SId, db => DbName},
    ?INFO("reached max ddocs ~p", [M], Meta),
    {stop, St};
ddoc(#st{sid = SId} = St, DbName, #doc{id = DDocId} = DDoc) ->
    ?INFO(#{sid => SId, db => DbName, ddoc => DDocId}),
    #st{ddoc_cnt = Cnt} = St,
    St1 = St#st{ddoc_cnt = Cnt + 1},
    #doc{body = {Props}} = DDoc,
    case couch_util:get_value(<<"language">>, Props, <<"javascript">>) of
        <<"javascript">> -> {ok, process_ddoc(St1, DbName, DDoc)};
        _ -> {ok, St1}
    end.

shards(#st{max_shards = Max, ddocs = DDocs} = St, Shards) ->
    case {map_size(DDocs), lists:sublist(Shards, Max)} of
        {0, _} -> {[], St};
        {_, []} -> {[], St};
        {_, Shards1} -> {Shards1, St}
    end.

db_opened(#st{} = St, Db) ->
    #st{max_docs = MaxDocs, max_step = MaxStep} = St,
    {ok, DocTotal} = couch_db:get_doc_count(Db),
    Step = min(MaxStep, max(1, DocTotal div MaxDocs)),
    {ok, St#st{doc_cnt = 0, doc_step = Step, docs = []}}.

doc_id(#st{} = St, <<?DESIGN_DOC_PREFIX, _/binary>>, _Db) ->
    {skip, St};
doc_id(#st{sid = SId, doc_cnt = C, max_docs = M} = St, _DocId, Db) when C > M ->
    Meta = #{sid => SId, db => Db},
    ?INFO("reached max docs ~p", [M], Meta),
    {stop, St};
doc_id(#st{doc_cnt = C, doc_step = S} = St, _DocId, _Db) when C rem S /= 0 ->
    {skip, St#st{doc_cnt = C + 1}};
doc_id(#st{doc_cnt = C} = St, _DocId, _Db) ->
    {ok, St#st{doc_cnt = C + 1}}.

doc(#st{} = St, Db, #doc{id = DocId} = Doc) ->
    #st{
        sid = SId,
        ddocs = DDocs,
        docs = Docs,
        docs_size = DocsSize,
        max_batch_items = MaxItems,
        max_batch_size = MaxSize
    } = St,
    JsonDoc = couch_query_servers:json_doc(Doc),
    DDocFun = fun(DDocId, #{} = DDoc) ->
        try
            Filters = maps:get(?FILTERS, DDoc, undefined),
            filter_doc_validate(St, DDocId, Filters, JsonDoc),
            VDU = maps:get(?VDU, DDoc, undefined),
            vdu_doc_validate(St, DDocId, VDU, JsonDoc)
        catch
            throw:{validate, Error} ->
                Meta = #{sid => SId, db => Db, ddoc => DDocId, doc => DocId},
                ?WARN("doc validation failed ~p", [Error], Meta)
        end
    end,
    maps:foreach(DDocFun, DDocs),
    DocsSize1 = DocsSize + ?term_size(JsonDoc),
    case DocsSize1 < MaxSize andalso length(Docs) < MaxItems of
        true ->
            St1 = St#st{docs = [JsonDoc | Docs], docs_size = DocsSize1},
            {ok, St1};
        false ->
            {_Db, St1} = maps:fold(fun views_validate/3, {Db, St}, DDocs),
            St2 = St1#st{docs = [], docs_size = 0},
            {ok, St2}
    end.

db_closing(#st{docs = []} = St, _Db) ->
    {ok, St#st{doc_cnt = 0, doc_step = 0}};
db_closing(#st{ddocs = DDocs} = St, Db) ->
    {_Db, St1} = maps:fold(fun views_validate/3, {Db, St}, DDocs),
    {ok, St1#st{doc_cnt = 0, doc_step = 0, docs = []}}.

% Private

init_config(#st{} = St) ->
    St#st{
        max_ddocs = cfg_int("max_ddocs", St#st.max_ddocs),
        max_shards = cfg_int("max_shards", St#st.max_shards),
        max_docs = cfg_int("max_docs", St#st.max_docs),
        max_step = cfg_int("max_step", St#st.max_step),
        max_batch_items = cfg_int("max_batch_items", St#st.max_batch_items),
        max_batch_size = cfg_int("max_batch_size", St#st.max_batch_size)
    }.

cfg_int(Key, Default) when is_list(Key), is_integer(Default) ->
    config:get_integer(atom_to_list(?MODULE), Key, Default).

process_ddoc(#st{} = St, DbName, #doc{} = DDoc0) ->
    #st{sid = SId, ddocs = DDocs} = St,
    #doc{id = DDocId, body = Body} = DDoc0,
    DDoc = couch_scanner_util:ejson_map(Body),
    case map_size(DDoc) > 0 of
        true ->
            St1 = start_or_reset_procs(St),
            try
                lib_load(St1, maps:get(?LIB, DDoc, undefined)),
                views_load(St1, maps:get(?VIEWS, DDoc, undefined)),
                filters_load(St1, maps:get(?FILTERS, DDoc, undefined)),
                vdu_load(St1, maps:get(?VDU, DDoc, undefined)),
                reset_procs(St1),
                teach_ddoc_validate(St1, DDocId, DDoc),
                St1#st{ddocs = DDocs#{DDocId => DDoc}}
            catch
                throw:{validate, Error} ->
                    Meta = #{sid => SId, db => DbName, ddoc => DDocId},
                    ?WARN("ddoc validation failed ~p", [Error], Meta),
                    St1
            end;
        false ->
            St
    end.

views_validate(DDocId, #{?VIEWS := Views} = DDoc, {Db, #st{} = St0}) when
    map_size(Views) > 0
->
    St = reset_procs(St0),
    #st{sid = SId, docs = Docs} = St,
    try
        lib_load(St, maps:get(?LIB, DDoc, undefined)),
        ViewList = lists:sort(maps:to_list(Views)),
        Fun = fun({Name, #{?MAP := Src}}) -> add_fun_load(St, Name, Src) end,
        lists:foreach(Fun, ViewList),
        {[_ | _], St1 = #st{}} = lists:foldl(fun mapred_fold/2, {ViewList, St}, Docs),
        {Db, St1}
    catch
        throw:{validate, Error} ->
            Meta = #{sid => SId, db => Db, ddoc => DDocId},
            ?WARN("view validation failed ~p", [Error], Meta),
            {Db, St}
    end;
views_validate(_DDocId, #{} = _DDoc, {Db, #st{} = St}) ->
    % No views
    {Db, St}.

mapred_fold({Props = [_ | _]} = Doc, {ViewList = [_ | _], #st{} = St}) ->
    #st{qjs_proc = Qjs, sm_proc = Sm} = St,
    DocId = couch_util:get_value(<<"_id">>, Props),
    QjsMapRes = map_doc(Qjs, Doc),
    SmMapRes = map_doc(Sm, Doc),
    case QjsMapRes == SmMapRes of
        true -> ok;
        false -> throw({validate, {map_doc, DocId, QjsMapRes, SmMapRes}})
    end,
    case QjsMapRes of
        [_ | _] ->
            MapResZip = lists:zip(ViewList, QjsMapRes),
            ReduceKVs = lists:filtermap(fun reduce_filter_map/1, MapResZip),
            view_reduce_validate(St, ReduceKVs);
        _ ->
            ok
    end,
    {ViewList, St}.

reset_per_db_state(#st{qjs_proc = QjsProc, sm_proc = SmProc} = St) ->
    proc_stop(QjsProc),
    proc_stop(SmProc),
    St#st{
        ddocs = #{},
        docs = [],
        qjs_proc = undefined,
        sm_proc = undefined,
        ddoc_cnt = 0
    }.

start_or_reset_procs(#st{} = St) ->
    St1 = start_or_reset_qjs_proc(St),
    start_or_reset_sm_proc(St1).

start_or_reset_qjs_proc(#st{qjs_proc = undefined} = St) ->
    Cmd = couch_quickjs:mainjs_cmd(),
    {ok, Pid} = couch_os_process:start_link(Cmd),
    Proc = proc(Pid),
    true = proc_reset(Proc),
    St#st{qjs_proc = Proc};
start_or_reset_qjs_proc(#st{qjs_proc = #proc{} = Proc} = St) ->
    try
        true = proc_reset(Proc),
        St
    catch
        Tag:Err ->
            ?WARN("failed to reset QuickJS proc ~p:~p", [Tag, Err]),
            proc_stop(Proc),
            start_or_reset_qjs_proc(St#st{qjs_proc = undefined})
    end.

start_or_reset_sm_proc(#st{sm_proc = undefined} = St) ->
    Cmd = os:getenv("COUCHDB_QUERY_SERVER_JAVASCRIPT"),
    {ok, Pid} = couch_os_process:start_link(Cmd),
    Proc = proc(Pid),
    true = proc_reset(Proc),
    St#st{sm_proc = Proc};
start_or_reset_sm_proc(#st{sm_proc = #proc{} = Proc} = St) ->
    try
        true = proc_reset(Proc),
        St
    catch
        Tag:Err ->
            ?WARN("failed to reset Spidermonkey proc ~p:~p", [Tag, Err]),
            proc_stop(Proc),
            start_or_reset_sm_proc(St#st{sm_proc = undefined})
    end.

lib_load(#st{}, undefined) ->
    ok;
lib_load(#st{qjs_proc = Qjs, sm_proc = Sm}, #{} = Lib) ->
    QjsRes = add_lib(Qjs, Lib),
    SmRes = add_lib(Sm, Lib),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {add_lib, QjsRes, SmRes}})
    end.

views_load(#st{}, undefined) ->
    ok;
views_load(#st{} = St, #{} = Views) ->
    Fun = fun(Name, #{} = View) -> view_load(St, Name, View) end,
    maps:foreach(Fun, Views).

view_load(#st{} = St, Name, View) ->
    #{?MAP := MapSrc} = View,
    add_fun_load(St, Name, MapSrc),
    RedSrc = maps:get(?REDUCE, View, undefined),
    add_fun_load(St, Name, RedSrc).

add_fun_load(#st{}, _, undefined) ->
    ok;
add_fun_load(#st{qjs_proc = Qjs, sm_proc = Sm}, Name, Src) ->
    QjsRes = add_fun(Qjs, Src),
    SmRes = add_fun(Sm, Src),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {add_fun, Name, QjsRes, SmRes}})
    end.

reduce_filter_map({{_Name, #{?REDUCE := <<"_", _/binary>>}}, _KVs}) ->
    % Likely built-in view
    false;
reduce_filter_map({{Name, #{?REDUCE := <<_/binary>> = Src}}, [_ | _] = KVs}) ->
    {true, {Name, Src, KVs}};
reduce_filter_map({{_Name, #{}}, _KVs}) ->
    false.

view_reduce_validate(#st{} = St, ReduceKVs) ->
    #st{qjs_proc = Qjs, sm_proc = Sm} = St,
    RedFun = fun({Name, RedSrc, KVs}) ->
        QjsRedRes = reduce(Qjs, RedSrc, KVs),
        SmRedRes = reduce(Sm, RedSrc, KVs),
        case QjsRedRes == SmRedRes of
            true -> ok;
            false -> throw({validate, {reduce, Name, QjsRedRes, SmRedRes}})
        end,
        case QjsRedRes of
            [true, [_ | _] = RedVals] ->
                QjsRRedRes = rereduce(Qjs, RedSrc, RedVals),
                SmRRedRes = rereduce(Sm, RedSrc, RedVals),
                case QjsRRedRes == SmRRedRes of
                    true -> ok;
                    false -> throw({validate, {rereduce, Name, QjsRRedRes, SmRRedRes}})
                end;
            _ ->
                ok
        end
    end,
    lists:foreach(RedFun, ReduceKVs).

filters_load(#st{}, undefined) ->
    ok;
filters_load(#st{} = St, #{} = Filters) ->
    Fun = fun(Name, Filter) -> filter_load(St, Name, Filter) end,
    maps:foreach(Fun, Filters).

filter_load(#st{qjs_proc = Qjs, sm_proc = Sm}, Name, Filter) ->
    QjsRes = add_fun(Qjs, Filter),
    SmRes = add_fun(Sm, Filter),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {filter, Name, QjsRes, SmRes}})
    end.

filter_doc_validate(#st{}, _, undefined, _) ->
    ok;
filter_doc_validate(#st{} = St, DDocId, #{} = Filters, Doc) ->
    #st{qjs_proc = Qjs, sm_proc = Sm} = St,
    Fun = fun(FName, _) ->
        QjsRes = filter_doc(Qjs, DDocId, FName, Doc),
        SmRes = filter_doc(Sm, DDocId, FName, Doc),
        case QjsRes == SmRes of
            true -> ok;
            false -> throw({validate, {filter_doc, FName, QjsRes, SmRes}})
        end
    end,
    maps:foreach(Fun, Filters).

vdu_load(#st{}, undefined) ->
    ok;
vdu_load(#st{qjs_proc = Qjs, sm_proc = Sm}, VDU) ->
    QjsRes = add_fun(Qjs, VDU),
    SmRes = add_fun(Sm, VDU),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {vdu, QjsRes, SmRes}})
    end.

vdu_doc_validate(#st{}, _DDocId, undefined, _Doc) ->
    % No VDU
    ok;
vdu_doc_validate(#st{} = St, DDocId, _VDU, Doc) ->
    #st{qjs_proc = Qjs, sm_proc = Sm} = St,
    QjsRes = vdu_doc(Qjs, DDocId, Doc),
    SmRes = vdu_doc(Sm, DDocId, Doc),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {vdu_doc, QjsRes, SmRes}})
    end.

teach_ddoc_validate(#st{qjs_proc = Qjs, sm_proc = Sm}, DDocId, DDoc) ->
    QjsRes = teach_ddoc(Qjs, DDocId, DDoc),
    SmRes = teach_ddoc(Sm, DDocId, DDoc),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {teach_ddoc, DDocId, QjsRes, SmRes}})
    end.

reset_procs(#st{qjs_proc = #proc{} = QjsProc, sm_proc = #proc{} = SmProc} = St) ->
    true = proc_reset(QjsProc),
    true = proc_reset(SmProc),
    St.

% Proc commands

add_lib(#proc{} = Proc, #{} = Lib) ->
    true = prompt(Proc, [<<"add_lib">>, Lib]).

map_doc(#proc{} = Proc, {[_ | _]} = Doc) ->
    prompt(Proc, [<<"map_doc">>, Doc]).

reduce(#proc{} = Proc, <<_/binary>> = Src, KVs) ->
    prompt(Proc, [<<"reduce">>, [Src], KVs]).

rereduce(#proc{} = Proc, <<_/binary>> = Src, Vals) ->
    prompt(Proc, [<<"rereduce">>, [Src], Vals]).

add_fun(#proc{}, undefined) ->
    ok;
add_fun(#proc{}, <<"_", _/binary>>) ->
    % Built-in reduce likely
    ok;
add_fun(#proc{} = Proc, <<_/binary>> = FunSrc) ->
    prompt(Proc, [<<"add_fun">>, FunSrc]).

filter_doc(#proc{} = Proc, DDocId, FName, {[_ | _]} = Doc) ->
    prompt(Proc, [<<"ddoc">>, DDocId, [<<"filters">>, FName], [[Doc], #{}]]).

vdu_doc(#proc{} = Proc, DDocId, {[_ | _]} = Doc) ->
    prompt(Proc, [<<"ddoc">>, DDocId, [<<"validate_doc_update">>], [Doc, Doc]]).

teach_ddoc(#proc{} = Proc, DDocId, DDoc) ->
    true = prompt(Proc, [<<"ddoc">>, <<"new">>, DDocId, DDoc]).

proc_reset(#proc{} = Proc) ->
    Timeout = config:get_integer("couchdb", "os_process_timeout", 5000),
    Cfg = [{<<"reduce_limit">>, true}, {<<"timeout">>, Timeout}],
    Result = prompt(Proc, [<<"reset">>, {Cfg}]),
    proc_set_timeout(Proc, Timeout),
    Result.

% End proc commands

% Proc utils

proc(Pid) ->
    #proc{
        pid = Pid,
        prompt_fun = {couch_os_process, prompt},
        set_timeout_fun = {couch_os_process, set_timeout},
        stop_fun = {couch_os_process, stop}
    }.

prompt(#proc{} = Proc, Prompt) ->
    try
        couch_query_servers:proc_prompt(Proc, Prompt)
    catch
        Tag:Err ->
            {error, {Tag, Err}}
    end.

proc_set_timeout(Proc, Timeout) ->
    {Mod, Func} = Proc#proc.set_timeout_fun,
    apply(Mod, Func, [Proc#proc.pid, Timeout]).

proc_stop(undefined) ->
    ok;
proc_stop(#proc{pid = Pid} = Proc) ->
    unlink(Pid),
    Ref = monitor(process, Pid),
    {Mod, Func} = Proc#proc.stop_fun,
    apply(Mod, Func, [Pid]),
    receive
        {'DOWN', Ref, _, _, _} -> ok
    end.
