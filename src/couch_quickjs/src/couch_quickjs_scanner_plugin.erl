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
    max_batch_items = 50,
    max_batch_size = 8388608
}).

% DDoc fields

-define(FILTERS, <<"filters">>).
-define(UPDATES, <<"updates">>).
-define(VIEWS, <<"views">>).
-define(CLOUSEAU, <<"indexes">>).
-define(NOUVEAU, <<"nouveau">>).
-define(INDEX, <<"index">>).
-define(MAP, <<"map">>).
-define(REDUCE, <<"reduce">>).
-define(LIB, <<"lib">>).
-define(VDU, <<"validate_doc_update">>).

% Avoid checking indeterministic functions. They always show as false positives
%
-define(INTEDERMINISM_HEURISTICS, [<<"Math.random()">>, <<"Date.now()">>, <<"new Date()">>]).

% Mock JS request object. Used by filter and update handlers
% See https://docs.couchdb.org/en/stable/json-structure.html#request-object
-define(MOCK_REQ, #{
    <<"info">> => #{
        <<"committed_update_seq">> => 42,
        <<"db_name">> => <<"foo">>,
        <<"doc_count">> => 42,
        <<"doc_del_count">> => 42,
        <<"sizes">> => #{<<"active">> => 42, <<"disk">> => 42, <<"external">> => 42}
    },
    <<"query">> => #{},
    <<"body">> => #{},
    <<"method">> => <<"POST">>,
    <<"headers">> => #{},
    <<"form">> => #{},
    <<"cookie">> => #{},
    <<"userCtx">> => #{},
    <<"secObj">> => #{},
    <<"path">> => [<<"a">>, <<"b">>, <<"c">>],
    <<"requested_path">> => [<<"a">>, <<"b">>, <<"c">>],
    <<"raw_path">> => <<"a/b/c">>,
    <<"peer">> => <<"127.0.0.1">>,
    <<"uuid">> => <<"2f3aaf5c54d94f982f384c6f8000197b">>
}).

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
    #st{sid = SId} = St,
    JsonDoc = couch_query_servers:json_doc(Doc),
    try
        St1 = maybe_reset_and_teach_ddocs(St),
        process_ddoc_functions(St1, Db, DocId, JsonDoc),
        process_doc_views(St1, Db, JsonDoc)
    catch
        Tag:Err:Stack ->
            Meta = #{sid => SId, db => Db, doc => DocId},
            ?ERR("doc validation exception ~p:~p:~p", [Tag, Err, Stack], Meta),
            {ok, St}
    end.

db_closing(#st{docs = []} = St, _Db) ->
    {ok, St#st{doc_cnt = 0, doc_step = 0}};
db_closing(#st{ddocs = DDocs} = St, Db) ->
    {_, St1} = maps:fold(fun views_validate/3, {Db, St}, DDocs),
    {_, St2} = maps:fold(fun clouseau_validate/3, {Db, St1}, DDocs),
    {_, St3} = maps:fold(fun nouveau_validate/3, {Db, St2}, DDocs),
    {ok, St3#st{doc_cnt = 0, doc_step = 0, docs = []}}.

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
                Views = maps:get(?VIEWS, DDoc, undefined),
                Clouseau = maps:get(?CLOUSEAU, DDoc, undefined),
                Nouveau = maps:get(?NOUVEAU, DDoc, undefined),
                lib_load(St1, Views),
                views_load(St1, valid_views(Views)),
                clouseau_load(St1, indexes(Clouseau)),
                nouveau_load(St1, indexes(Nouveau)),
                filters_load(St1, maps:get(?FILTERS, DDoc, undefined)),
                updates_load(St1, maps:get(?UPDATES, DDoc, undefined)),
                vdu_load(St1, maps:get(?VDU, DDoc, undefined)),
                St2 = start_or_reset_procs(St1),
                teach_ddoc_validate(St2, DDocId, DDoc),
                St2#st{ddocs = DDocs#{DDocId => DDoc}}
            catch
                throw:{validate, Error} ->
                    Meta = #{sid => SId, db => DbName, ddoc => DDocId},
                    validation_warning("ddoc validation failed ~p", Error, Meta),
                    St1;
                Tag:Err:Stack ->
                    Meta = #{sid => SId, db => DbName, ddoc => DDocId},
                    ?ERR("ddoc validation exception ~p:~p:~p", [Tag, Err, Stack], Meta),
                    St1
            end;
        false ->
            St
    end.

process_ddoc_functions(#st{} = St, Db, DocId, JsonDoc) ->
    #st{sid = SId, ddocs = DDocs} = St,
    DDocFun = fun(DDocId, #{} = DDoc) ->
        try
            Filters = maps:get(?FILTERS, DDoc, undefined),
            filter_doc_validate(St, DDocId, Filters, JsonDoc),
            VDU = maps:get(?VDU, DDoc, undefined),
            vdu_doc_validate(St, DDocId, VDU, JsonDoc),
            Updates = maps:get(?UPDATES, DDoc, undefined),
            update_doc_validate(St, DDocId, Updates, JsonDoc)
        catch
            throw:{validate, Error} ->
                Meta = #{sid => SId, db => Db, ddoc => DDocId, doc => DocId},
                validation_warning("doc validation failed ~p", Error, Meta)
        end
    end,
    try
        maps:foreach(DDocFun, DDocs)
    catch
        Tag:Err:Stack ->
            Meta = #{sid => SId, db => Db, doc => DocId},
            ?ERR("Exception when validating doc ~p:~p:~p", [Tag, Err, Stack], Meta)
    end.

process_doc_views(#st{} = St, Db, JsonDoc) ->
    #st{
        ddocs = DDocs,
        docs = Docs,
        docs_size = DocsSize,
        max_batch_items = MaxItems,
        max_batch_size = MaxSize
    } = St,
    DocsSize1 = DocsSize + ?term_size(JsonDoc),
    case DocsSize1 < MaxSize andalso length(Docs) < MaxItems of
        true ->
            St1 = St#st{docs = [JsonDoc | Docs], docs_size = DocsSize1},
            {ok, St1};
        false ->
            {_, St1} = maps:fold(fun views_validate/3, {Db, St}, DDocs),
            {_, St2} = maps:fold(fun clouseau_validate/3, {Db, St1}, DDocs),
            {_, St3} = maps:fold(fun nouveau_validate/3, {Db, St2}, DDocs),
            St4 = St3#st{docs = [], docs_size = 0},
            {ok, St4}
    end.

views_validate(DDocId, #{?VIEWS := Views}, {Db, #st{} = St0}) when
    map_size(Views) > 0
->
    St = start_or_reset_procs(St0),
    #st{sid = SId, docs = Docs} = St,
    try
        lib_load(St, Views),
        ViewList = lists:sort(maps:to_list(valid_views(Views))),
        case ViewList of
            [_ | _] ->
                Fun = fun({Name, #{?MAP := Src}}) -> add_fun_load(St, Name, Src) end,
                lists:foreach(Fun, ViewList),
                {[_ | _], St1 = #st{}} = lists:foldl(fun view_mapred_fold/2, {ViewList, St}, Docs),
                {Db, St1};
            [] ->
                % There may be no valid views left
                {Db, St}
        end
    catch
        throw:restart_procs ->
            #st{qjs_proc = Qjs, sm_proc = Sm} = St,
            proc_stop(Sm),
            proc_stop(Qjs),
            {Db, St#st{qjs_proc = undefined, sm_proc = undefined}};
        throw:{validate, Error} ->
            Meta = #{sid => SId, db => Db, ddoc => DDocId},
            validation_warning("view validation failed ~p", Error, Meta),
            {Db, St};
        Tag:Err:Stack ->
            Meta = #{sid => SId, db => Db, ddoc => DDocId},
            ?ERR("view validation exception ~p:~p:~p", [Tag, Err, Stack], Meta),
            {Db, St}
    end;
views_validate(_DDocId, #{} = _DDoc, {Db, #st{} = St}) ->
    % No views
    {Db, St}.

view_mapred_fold({Props = [_ | _]} = Doc, {ViewList = [_ | _], #st{} = St}) ->
    #st{qjs_proc = Qjs, sm_proc = Sm} = St,
    DocId = couch_util:get_value(<<"_id">>, Props),
    SmMapRes = map_doc(Sm, Doc),
    QjsMapRes = map_doc(Qjs, Doc),
    case QjsMapRes == SmMapRes of
        true -> ok;
        false -> throw({validate, {map_doc, DocId, QjsMapRes, SmMapRes}})
    end,
    % Calling list functions from a map will result in a result list
    % longer than the number of views, so we match that the view list
    % and the results match
    case length(QjsMapRes) =:= length(ViewList) of
        true ->
            case QjsMapRes of
                [_ | _] ->
                    MapResZip = lists:zip(ViewList, QjsMapRes),
                    ReduceKVs = lists:filtermap(fun reduce_filter_map/1, MapResZip),
                    view_reduce_validate(St, ReduceKVs);
                _ ->
                    ok
            end,
            {ViewList, St};
        false ->
            % Exit early as calling list functions from a map leads to breaking the
            % query protocol. To avoid the messed up state affecting other views
            % exit early and restart the processes.
            throw(restart_procs)
    end.

clouseau_validate(DDocId, #{?CLOUSEAU := Indexes0}, {Db, #st{} = St}) when map_size(Indexes0) > 0 ->
    Indexes = indexes(Indexes0),
    {Db1, _, St1} = maps:fold(fun clouseau_validate_mapfold/3, {Db, DDocId, St}, Indexes),
    {Db1, St1};
clouseau_validate(_DDocId, #{} = _DDoc, {Db, #st{} = St}) ->
    % No clouseau indexes
    {Db, St}.

nouveau_validate(DDocId, #{?NOUVEAU := Indexes0}, {Db, #st{} = St}) when map_size(Indexes0) > 0 ->
    Indexes = indexes(Indexes0),
    {Db1, _, St1} = maps:fold(fun nouveau_validate_mapfold/3, {Db, DDocId, St}, Indexes),
    {Db1, St1};
nouveau_validate(_DDocId, #{}, {Db, #st{} = St}) ->
    % No nouveau indexes
    {Db, St}.

clouseau_validate_mapfold(IndexName, IndexSrc, {Db, DDocId, #st{} = St0}) ->
    St = start_or_reset_procs(St0),
    #st{sid = SId, docs = Docs, qjs_proc = Qjs, sm_proc = Sm} = St,
    try
        add_fun(Sm, IndexSrc),
        add_fun(Qjs, IndexSrc),
        St1 = #st{} = lists:foldl(fun clouseau_foldl/2, St, Docs),
        {Db, DDocId, St1}
    catch
        throw:{validate, Error} ->
            Meta = #{sid => SId, db => Db, ddoc => DDocId, index => IndexName},
            validation_warning("clouseau validation failed ~p", Error, Meta),
            {Db, DDocId, St};
        Tag:Err:Stack ->
            Meta = #{sid => SId, db => Db, ddoc => DDocId, index => IndexName},
            ?ERR("clouseau validation exception ~p:~p:~p", [Tag, Err, Stack], Meta),
            {Db, DDocId, St}
    end.

nouveau_validate_mapfold(IndexName, IndexSrc, {Db, DDocId, #st{} = St0}) ->
    St = start_or_reset_procs(St0),
    #st{sid = SId, docs = Docs, qjs_proc = Qjs, sm_proc = Sm} = St,
    try
        nouveau_add_fun(Sm, IndexSrc),
        nouveau_add_fun(Qjs, IndexSrc),
        St1 = #st{} = lists:foldl(fun nouveau_foldl/2, St, Docs),
        {Db, DDocId, St1}
    catch
        throw:{validate, Error} ->
            Meta = #{sid => SId, db => Db, ddoc => DDocId, index => IndexName},
            validation_warning("nouveau validation failed ~p", Error, Meta),
            {Db, DDocId, St};
        Tag:Err:Stack ->
            Meta = #{sid => SId, db => Db, ddoc => DDocId, index => IndexName},
            ?ERR("nouveau validation exception ~p:~p:~p", [Tag, Err, Stack], Meta),
            {Db, DDocId, St}
    end.

clouseau_foldl({Props = [_ | _]} = Doc, #st{} = St) ->
    #st{qjs_proc = Qjs, sm_proc = Sm} = St,
    DocId = couch_util:get_value(<<"_id">>, Props),
    SmMapRes = clouseau_index_doc(Sm, Doc),
    QjsMapRes = clouseau_index_doc(Qjs, Doc),
    case QjsMapRes == SmMapRes of
        true -> St;
        false -> throw({validate, {clouseau_index, DocId, QjsMapRes, SmMapRes}})
    end.

nouveau_foldl({Props = [_ | _]} = Doc, #st{} = St) ->
    #st{qjs_proc = Qjs, sm_proc = Sm} = St,
    DocId = couch_util:get_value(<<"_id">>, Props),
    SmMapRes = nouveau_index_doc(Sm, Doc),
    QjsMapRes = nouveau_index_doc(Qjs, Doc),
    case QjsMapRes == SmMapRes of
        true -> St;
        false -> throw({validate, {nouveau_index, DocId, QjsMapRes, SmMapRes}})
    end.

reset_per_db_state(#st{qjs_proc = QjsProc, sm_proc = SmProc} = St) ->
    proc_stop(SmProc),
    proc_stop(QjsProc),
    St#st{
        ddocs = #{},
        docs = [],
        qjs_proc = undefined,
        sm_proc = undefined,
        ddoc_cnt = 0
    }.

start_or_reset_procs(#st{} = St) ->
    St1 = start_or_reset_sm_proc(St),
    start_or_reset_qjs_proc(St1).

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
        exit:{noproc, _} ->
            proc_stop(Proc),
            start_or_reset_qjs_proc(St#st{qjs_proc = undefined});
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
        exit:{noproc, _} ->
            proc_stop(Proc),
            start_or_reset_sm_proc(St#st{sm_proc = undefined});
        Tag:Err ->
            ?WARN("failed to reset Spidermonkey proc ~p:~p", [Tag, Err]),
            proc_stop(Proc),
            start_or_reset_sm_proc(St#st{sm_proc = undefined})
    end.

valid_views(#{} = Views) ->
    Fun = fun
        (?LIB, _) ->
            false;
        (<<_/binary>>, #{?MAP := <<MapFun/binary>>, ?REDUCE := <<RedFun/binary>>}) ->
            no_indeterminism(MapFun) andalso no_indeterminism(RedFun);
        (<<_/binary>>, #{?MAP := <<MapFun/binary>>}) ->
            no_indeterminism(MapFun);
        (_, _) ->
            false
    end,
    maps:filter(Fun, Views);
valid_views(_) ->
    #{}.

indexes(#{} = Indexes) ->
    Fun = fun
        (<<_/binary>> = IndexName, #{?INDEX := <<IndexFun/binary>>}, #{} = Acc) ->
            case no_indeterminism(IndexFun) of
                true -> Acc#{IndexName => IndexFun};
                false -> Acc
            end;
        (_, _, #{} = Acc) ->
            Acc
    end,
    maps:fold(Fun, #{}, Indexes);
indexes(_) ->
    #{}.

% Math.random(), Date.now() or new Date() will always show as false postives
%
no_indeterminism(<<FunSrc/binary>>) ->
    binary:match(FunSrc, ?INTEDERMINISM_HEURISTICS) =:= nomatch.

lib_load(#st{qjs_proc = Qjs, sm_proc = Sm}, #{} = Views) ->
    case maps:get(?LIB, Views, undefined) of
        Lib when is_map(Lib) andalso map_size(Lib) > 0 ->
            SmRes = add_lib(Sm, Lib),
            QjsRes = add_lib(Qjs, Lib),
            case QjsRes == SmRes of
                true -> ok;
                false -> throw({validate, {add_lib, QjsRes, SmRes}})
            end;
        _ ->
            ok
    end;
lib_load(#st{}, _) ->
    ok.

views_load(#st{} = St, #{} = Views) ->
    Fun = fun(Name, #{} = View) -> view_load(St, Name, View) end,
    maps:foreach(Fun, Views);
views_load(#st{}, _) ->
    ok.

view_load(#st{} = St, Name, View) ->
    #{?MAP := MapSrc} = View,
    add_fun_load(St, Name, MapSrc),
    RedSrc = maps:get(?REDUCE, View, undefined),
    add_fun_load(St, Name, RedSrc).

clouseau_load(#st{} = St, #{} = Indexes) ->
    % Note: we can re-use views add_fun_load here
    Fun = fun(Name, <<FunSrc/binary>>) -> add_fun_load(St, Name, FunSrc) end,
    maps:foreach(Fun, Indexes);
clouseau_load(#st{}, _) ->
    ok.

nouveau_load(#st{} = St, #{} = Indexes) ->
    Fun = fun(Name, <<FunSrc/binary>>) -> nouveau_add_fun_load(St, Name, FunSrc) end,
    maps:foreach(Fun, Indexes);
nouveau_load(#st{}, _) ->
    ok.

nouveau_add_fun_load(#st{qjs_proc = Qjs, sm_proc = Sm}, Name, <<_/binary>> = Src) ->
    SmRes = nouveau_add_fun(Sm, Src),
    QjsRes = nouveau_add_fun(Qjs, Src),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {nouveau_add_fun, Name, QjsRes, SmRes}})
    end;
nouveau_add_fun_load(#st{}, _, _) ->
    ok.

add_fun_load(#st{qjs_proc = Qjs, sm_proc = Sm}, Name, <<_/binary>> = Src) ->
    SmRes = add_fun(Sm, Src),
    QjsRes = add_fun(Qjs, Src),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {add_fun, Name, QjsRes, SmRes}})
    end;
add_fun_load(#st{}, _, _) ->
    ok.

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
        SmRedRes = reduce(Sm, RedSrc, KVs),
        QjsRedRes = reduce(Qjs, RedSrc, KVs),
        case QjsRedRes == SmRedRes of
            true -> ok;
            false -> throw({validate, {reduce, Name, QjsRedRes, SmRedRes}})
        end,
        case QjsRedRes of
            [true, [_ | _] = RedVals] ->
                SmRRedRes = rereduce(Sm, RedSrc, RedVals),
                QjsRRedRes = rereduce(Qjs, RedSrc, RedVals),
                case QjsRRedRes == SmRRedRes of
                    true -> ok;
                    false -> throw({validate, {rereduce, Name, QjsRRedRes, SmRRedRes}})
                end;
            _ ->
                ok
        end
    end,
    lists:foreach(RedFun, ReduceKVs).

filters_load(#st{} = St, #{} = Filters) ->
    Fun = fun(Name, Filter) -> filter_load(St, Name, Filter) end,
    maps:foreach(Fun, Filters);
filters_load(#st{}, _) ->
    ok.

filter_load(#st{qjs_proc = Qjs, sm_proc = Sm}, Name, Filter) ->
    SmRes = add_fun(Sm, Filter),
    QjsRes = add_fun(Qjs, Filter),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {filter, Name, QjsRes, SmRes}})
    end.

filter_doc_validate(#st{} = St, DDocId, #{} = Filters, Doc) ->
    #st{qjs_proc = Qjs, sm_proc = Sm} = St,
    Fun = fun(FName, _) ->
        SmRes = filter_doc(Sm, DDocId, FName, Doc),
        QjsRes = filter_doc(Qjs, DDocId, FName, Doc),
        case QjsRes == SmRes of
            true -> ok;
            false -> throw({validate, {filter_doc, FName, QjsRes, SmRes}})
        end
    end,
    maps:foreach(Fun, Filters);
filter_doc_validate(#st{}, _, _, _) ->
    ok.

updates_load(#st{} = St, #{} = Updates) ->
    Fun = fun(Name, Update) -> update_load(St, Name, Update) end,
    maps:foreach(Fun, Updates);
updates_load(#st{}, _) ->
    ok.

update_load(#st{qjs_proc = Qjs, sm_proc = Sm}, Name, Update) ->
    SmRes = add_fun(Sm, Update),
    QjsRes = add_fun(Qjs, Update),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {update, Name, QjsRes, SmRes}})
    end.

update_doc_validate(#st{} = St, DDocId, #{} = Updates, Doc) ->
    #st{qjs_proc = Qjs, sm_proc = Sm} = St,
    Fun = fun(UName, _) ->
        SmRes = update_doc(Sm, DDocId, UName, Doc),
        QjsRes = update_doc(Qjs, DDocId, UName, Doc),
        case QjsRes == SmRes of
            true -> ok;
            false -> throw({validate, {update_doc, UName, QjsRes, SmRes}})
        end
    end,
    maps:foreach(Fun, Updates);
update_doc_validate(#st{}, _, _, _) ->
    ok.

vdu_load(#st{qjs_proc = Qjs, sm_proc = Sm}, <<_/binary>> = VDU) ->
    SmRes = add_fun(Sm, VDU),
    QjsRes = add_fun(Qjs, VDU),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {vdu, QjsRes, SmRes}})
    end;
vdu_load(#st{}, _) ->
    ok.

vdu_doc_validate(#st{} = St, DDocId, _VDU = <<_/binary>>, Doc) ->
    #st{qjs_proc = Qjs, sm_proc = Sm} = St,
    SmRes = vdu_doc(Sm, DDocId, Doc),
    QjsRes = vdu_doc(Qjs, DDocId, Doc),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {vdu_doc, QjsRes, SmRes}})
    end;
vdu_doc_validate(#st{}, _DDocId, _VDU, _Doc) ->
    ok.

teach_ddoc_validate(#st{qjs_proc = Qjs, sm_proc = Sm}, DDocId, DDoc) ->
    SmRes = teach_ddoc(Sm, DDocId, DDoc),
    QjsRes = teach_ddoc(Qjs, DDocId, DDoc),
    case QjsRes == SmRes of
        true -> ok;
        false -> throw({validate, {teach_ddoc, DDocId, QjsRes, SmRes}})
    end.

% During view validation if a JS process crashes and exists the next filter and
% vdu validation for the same db will fail as the "taught" design documents
% will not be in loaded in the JS process ddoc cache. To make sure we continue
% validating, try to restart the process and reload all the ddocs.
%
maybe_reset_and_teach_ddocs(#st{ddocs = DDocs} = St) ->
    St1 =
        case is_proc_alive(St#st.sm_proc) of
            true ->
                St;
            false ->
                StSm = start_or_reset_sm_proc(St),
                FunSm = fun(DDocId, DDoc) -> teach_ddoc(StSm#st.sm_proc, DDocId, DDoc) end,
                maps:foreach(FunSm, DDocs),
                StSm
        end,
    case is_proc_alive(St#st.qjs_proc) of
        true ->
            St1;
        false ->
            StQjs = start_or_reset_qjs_proc(St1),
            FunQjs = fun(DDocId, DDoc) -> teach_ddoc(StQjs#st.qjs_proc, DDocId, DDoc) end,
            maps:foreach(FunQjs, DDocs),
            StQjs
    end.

validation_warning(Fmt, Error, #{} = Meta) when is_list(Fmt), is_tuple(Error) ->
    case expected_error(Error) of
        true -> ok;
        false -> ?WARN(Fmt, [Error], Meta)
    end.

% Malformed documents and functions will fail with different error messages
% Eliminate some false positives to avoid polluting the logs with junk
%
expected_error({Op, QjsRes, SmRes}) when is_atom(Op) ->
    expected_error(QjsRes, SmRes);
expected_error({Op, _Name, QjsRes, SmRes}) when is_atom(Op) ->
    expected_error(QjsRes, SmRes);
expected_error(_) ->
    false.

expected_error({error, {_, compilation_error, _}}, {error, {_, compilation_error, _}}) ->
    true;
expected_error({error, {_, {compilation_error, _}}}, {error, {_, {compilation_error, _}}}) ->
    true;
expected_error({error, {_, <<"TypeError">>, _}}, {error, {_, <<"TypeError">>, _}}) ->
    true;
expected_error({error, {_, {<<"TypeError">>, _}}}, {error, {_, {<<"TypeError">>, _}}}) ->
    true;
expected_error({error, {_, {<<"unnamed_error">>, _}}}, {error, {_, {<<"unnamed_error">>, _}}}) ->
    true;
expected_error({error, {_, {<<"SyntaxError">>, _}}}, {error, {_, {<<"SyntaxError">>, _}}}) ->
    true;
expected_error({error, {_, {<<"ReferenceError">>, _}}}, {error, {_, {<<"ReferenceError">>, _}}}) ->
    true;
expected_error({error, {_, {<<"render_error">>, _}}}, {error, {_, {<<"render_error">>, _}}}) ->
    true;
expected_error(_, _) ->
    false.

map_sort_kvs(KVs) when is_list(KVs) ->
    lists:sort(KVs);
map_sort_kvs(KVs) ->
    KVs.

% Proc commands

add_lib(#proc{} = Proc, #{} = Lib) ->
    true = prompt(Proc, [<<"add_lib">>, Lib]).

map_doc(#proc{} = Proc, {[_ | _]} = Doc) ->
    % couch_mrview_updater:insert_results/4 sorts the KVs, so we have to do the
    % same. Otherwise, we risk showing false positives if emit order changes
    % between engine versions, for instance.
    case prompt(Proc, [<<"map_doc">>, Doc]) of
        Results when is_list(Results) -> lists:map(fun map_sort_kvs/1, Results);
        Results -> Results
    end.

reduce(#proc{} = Proc, <<_/binary>> = Src, KVs) ->
    prompt(Proc, [<<"reduce">>, [Src], KVs]).

rereduce(#proc{} = Proc, <<_/binary>> = Src, Vals) ->
    prompt(Proc, [<<"rereduce">>, [Src], Vals]).

add_fun(#proc{}, <<"_", _/binary>>) ->
    % Built-in reduce likely
    ok;
add_fun(#proc{} = Proc, <<_/binary>> = FunSrc) ->
    prompt(Proc, [<<"add_fun">>, FunSrc]);
add_fun(#proc{}, _) ->
    ok.

nouveau_add_fun(#proc{} = Proc, <<_/binary>> = FunSrc) ->
    prompt(Proc, [<<"add_fun">>, FunSrc, <<"nouveau">>]);
nouveau_add_fun(#proc{}, _) ->
    ok.

clouseau_index_doc(#proc{} = Proc, {[_ | _]} = Doc) ->
    [Fields | _] = prompt(Proc, [<<"index_doc">>, Doc]),
    lists:sort(Fields).

nouveau_index_doc(#proc{} = Proc, {[_ | _]} = Doc) ->
    [Fields | _] = prompt(Proc, [<<"nouveau_index_doc">>, Doc]),
    lists:sort(Fields).

filter_doc(#proc{} = Proc, DDocId, FName, {[_ | _]} = Doc) ->
    % Add a mock request object so param access doesn't throw a TypeError
    prompt(Proc, [<<"ddoc">>, DDocId, [<<"filters">>, FName], [[Doc], ?MOCK_REQ]]).

update_doc(#proc{} = Proc, DDocId, UName, {[_ | _] = Props} = Doc) ->
    % Use a mock object. It's better than nothing at least. We don't know
    % what the user might post.
    MockReq = ?MOCK_REQ,
    MockReq1 =
        case couch_util:get_value(<<"_id">>, Props) of
            Id when is_binary(Id) -> MockReq#{<<"id">> => Id};
            _ -> MockReq
        end,
    prompt(Proc, [<<"ddoc">>, DDocId, [<<"updates">>, UName], [Doc, MockReq1]]).

vdu_doc(#proc{} = Proc, DDocId, {[_ | _]} = Doc) ->
    prompt(Proc, [<<"ddoc">>, DDocId, [<<"validate_doc_update">>], [Doc, Doc]]).

teach_ddoc(#proc{} = Proc, DDocId, DDoc) ->
    prompt(Proc, [<<"ddoc">>, <<"new">>, DDocId, DDoc]).

proc_reset(#proc{} = Proc) ->
    Timeout = config:get_integer("couchdb", "os_process_timeout", 5000),
    Cfg = [{<<"timeout">>, Timeout}],
    Result = prompt(Proc, [<<"reset">>, {Cfg}]),
    proc_set_timeout(Proc, Timeout),
    Result.

% End proc commands

% Proc utils

is_proc_alive(undefined) ->
    false;
is_proc_alive(#proc{pid = Pid}) ->
    is_process_alive(Pid).

proc(Pid) ->
    #proc{
        pid = Pid,
        prompt_fun = {couch_os_process, prompt},
        set_timeout_fun = {couch_os_process, set_timeout},
        stop_fun = {couch_os_process, stop}
    }.

prompt(#proc{} = Proc, Prompt) ->
    % Make sure to transform ejson with key values {[{K, V}, ...]} to
    % one containing maps to avoid false positives resulting from
    % differences in json key encoding order
    try
        couch_util:ejson_to_map(couch_query_servers:proc_prompt(Proc, Prompt))
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
