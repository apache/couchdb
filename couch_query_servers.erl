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

-module(couch_query_servers).
-behaviour(gen_server).

-export([start_link/0, config_change/1]).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,code_change/3]).
-export([start_doc_map/3, map_docs/2, map_doc_raw/2, stop_doc_map/1, raw_to_ejson/1]).
-export([reduce/3, rereduce/3,validate_doc_update/5]).
-export([filter_docs/5]).
-export([filter_view/3]).

-export([with_ddoc_proc/2, proc_prompt/2, ddoc_prompt/3, ddoc_proc_prompt/3, json_doc/1]).

% For 210-os-proc-pool.t
-export([get_os_process/1, ret_os_process/1]).

-include("couch_db.hrl").

-record(proc, {
    pid,
    lang,
    ddoc_keys = [],
    prompt_fun,
    set_timeout_fun,
    stop_fun
}).

-record(qserver, {
    langs, % Keyed by language name, value is {Mod,Func,Arg}
    pid_procs, % Keyed by PID, valus is a #proc record.
    lang_procs, % Keyed by language name, value is a #proc record
    lang_limits, % Keyed by language name, value is {Lang, Limit, Current}
    waitlist = [],
    config
}).

start_link() ->
    gen_server:start_link({local, couch_query_servers}, couch_query_servers, [], []).

start_doc_map(Lang, Functions, Lib) ->
    Proc = get_os_process(Lang),
    case Lib of
    {[]} -> ok;
    Lib ->
        true = proc_prompt(Proc, [<<"add_lib">>, Lib])
    end,
    lists:foreach(fun(FunctionSource) ->
        true = proc_prompt(Proc, [<<"add_fun">>, FunctionSource])
    end, Functions),
    {ok, Proc}.

map_docs(Proc, Docs) ->
    % send the documents
    Results = lists:map(
        fun(Doc) ->
            Json = couch_doc:to_json_obj(Doc, []),

            FunsResults = proc_prompt(Proc, [<<"map_doc">>, Json]),
            % the results are a json array of function map yields like this:
            % [FunResults1, FunResults2 ...]
            % where funresults is are json arrays of key value pairs:
            % [[Key1, Value1], [Key2, Value2]]
            % Convert the key, value pairs to tuples like
            % [{Key1, Value1}, {Key2, Value2}]
            lists:map(
                fun(FunRs) ->
                    [list_to_tuple(FunResult) || FunResult <- FunRs]
                end,
            FunsResults)
        end,
        Docs),
    {ok, Results}.

map_doc_raw(Proc, Doc) ->
    Json = couch_doc:to_json_obj(Doc, []),
    {ok, proc_prompt_raw(Proc, [<<"map_doc">>, Json])}.


stop_doc_map(nil) ->
    ok;
stop_doc_map(Proc) ->
    ok = ret_os_process(Proc).

group_reductions_results([]) ->
    [];
group_reductions_results(List) ->
    {Heads, Tails} = lists:foldl(
        fun([H|T], {HAcc,TAcc}) ->
            {[H|HAcc], [T|TAcc]}
        end, {[], []}, List),
    case Tails of
    [[]|_] -> % no tails left
        [Heads];
    _ ->
     [Heads | group_reductions_results(Tails)]
    end.

rereduce(_Lang, [], _ReducedValues) ->
    {ok, []};
rereduce(Lang, RedSrcs, ReducedValues) ->
    Grouped = group_reductions_results(ReducedValues),
    Results = lists:zipwith(
        fun
        (<<"_", _/binary>> = FunSrc, Values) ->
            {ok, [Result]} = builtin_reduce(rereduce, [FunSrc], [[[], V] || V <- Values], []),
            Result;
        (FunSrc, Values) ->
            os_rereduce(Lang, [FunSrc], Values)
        end, RedSrcs, Grouped),
    {ok, Results}.

reduce(_Lang, [], _KVs) ->
    {ok, []};
reduce(Lang, RedSrcs, KVs) ->
    {OsRedSrcs, BuiltinReds} = lists:partition(fun
        (<<"_", _/binary>>) -> false;
        (_OsFun) -> true
    end, RedSrcs),
    {ok, OsResults} = os_reduce(Lang, OsRedSrcs, KVs),
    {ok, BuiltinResults} = builtin_reduce(reduce, BuiltinReds, KVs, []),
    recombine_reduce_results(RedSrcs, OsResults, BuiltinResults, []).

recombine_reduce_results([], [], [], Acc) ->
    {ok, lists:reverse(Acc)};
recombine_reduce_results([<<"_", _/binary>>|RedSrcs], OsResults, [BRes|BuiltinResults], Acc) ->
    recombine_reduce_results(RedSrcs, OsResults, BuiltinResults, [BRes|Acc]);
recombine_reduce_results([_OsFun|RedSrcs], [OsR|OsResults], BuiltinResults, Acc) ->
    recombine_reduce_results(RedSrcs, OsResults, BuiltinResults, [OsR|Acc]).

os_reduce(_Lang, [], _KVs) ->
    {ok, []};
os_reduce(Lang, OsRedSrcs, KVs) ->
    Proc = get_os_process(Lang),
    OsResults = try proc_prompt(Proc, [<<"reduce">>, OsRedSrcs, KVs]) of
        [true, Reductions] -> Reductions
    after
        ok = ret_os_process(Proc)
    end,
    {ok, OsResults}.

os_rereduce(Lang, OsRedSrcs, KVs) ->
    Proc = get_os_process(Lang),
    try proc_prompt(Proc, [<<"rereduce">>, OsRedSrcs, KVs]) of
        [true, [Reduction]] -> Reduction
    after
        ok = ret_os_process(Proc)
    end.


builtin_reduce(_Re, [], _KVs, Acc) ->
    {ok, lists:reverse(Acc)};
builtin_reduce(Re, [<<"_sum",_/binary>>|BuiltinReds], KVs, Acc) ->
    Sum = builtin_sum_rows(KVs),
    builtin_reduce(Re, BuiltinReds, KVs, [Sum|Acc]);
builtin_reduce(reduce, [<<"_count",_/binary>>|BuiltinReds], KVs, Acc) ->
    Count = length(KVs),
    builtin_reduce(reduce, BuiltinReds, KVs, [Count|Acc]);
builtin_reduce(rereduce, [<<"_count",_/binary>>|BuiltinReds], KVs, Acc) ->
    Count = builtin_sum_rows(KVs),
    builtin_reduce(rereduce, BuiltinReds, KVs, [Count|Acc]);
builtin_reduce(Re, [<<"_stats",_/binary>>|BuiltinReds], KVs, Acc) ->
    Stats = builtin_stats(Re, KVs),
    builtin_reduce(Re, BuiltinReds, KVs, [Stats|Acc]).

builtin_sum_rows(KVs) ->
    lists:foldl(fun
        ([_Key, Value], Acc) when is_number(Value), is_number(Acc) ->
            Acc + Value;
        ([_Key, Value], Acc) when is_list(Value), is_list(Acc) ->
            sum_terms(Acc, Value);
        ([_Key, Value], Acc) when is_number(Value), is_list(Acc) ->
            sum_terms(Acc, [Value]);
        ([_Key, Value], Acc) when is_list(Value), is_number(Acc) ->
            sum_terms([Acc], Value);
        (_Else, _Acc) ->
            throw({invalid_value, <<"builtin _sum function requires map values to be numbers or lists of numbers">>})
    end, 0, KVs).

sum_terms([], []) ->
    [];
sum_terms([_|_]=Xs, []) ->
    Xs;
sum_terms([], [_|_]=Ys) ->
    Ys;
sum_terms([X|Xs], [Y|Ys]) when is_number(X), is_number(Y) ->
    [X+Y | sum_terms(Xs,Ys)];
sum_terms(_, _) ->
    throw({invalid_value, <<"builtin _sum function requires map values to be numbers or lists of numbers">>}).

builtin_stats(reduce, []) ->
    {[]};
builtin_stats(reduce, [[_,First]|Rest]) when is_number(First) ->
    Stats = lists:foldl(fun([_K,V], {S,C,Mi,Ma,Sq}) when is_number(V) ->
        {S+V, C+1, lists:min([Mi, V]), lists:max([Ma, V]), Sq+(V*V)};
    (_, _) ->
        throw({invalid_value,
            <<"builtin _stats function requires map values to be numbers">>})
    end, {First,1,First,First,First*First}, Rest),
    {Sum, Cnt, Min, Max, Sqr} = Stats,
    {[{sum,Sum}, {count,Cnt}, {min,Min}, {max,Max}, {sumsqr,Sqr}]};

builtin_stats(rereduce, [[_,First]|Rest]) ->
    {[{sum,Sum0}, {count,Cnt0}, {min,Min0}, {max,Max0}, {sumsqr,Sqr0}]} = First,
    Stats = lists:foldl(fun([_K,Red], {S,C,Mi,Ma,Sq}) ->
        {[{sum,Sum}, {count,Cnt}, {min,Min}, {max,Max}, {sumsqr,Sqr}]} = Red,
        {Sum+S, Cnt+C, lists:min([Min, Mi]), lists:max([Max, Ma]), Sqr+Sq}
    end, {Sum0,Cnt0,Min0,Max0,Sqr0}, Rest),
    {Sum, Cnt, Min, Max, Sqr} = Stats,
    {[{sum,Sum}, {count,Cnt}, {min,Min}, {max,Max}, {sumsqr,Sqr}]}.

% use the function stored in ddoc.validate_doc_update to test an update.
validate_doc_update(DDoc, EditDoc, DiskDoc, Ctx, SecObj) ->
    JsonEditDoc = couch_doc:to_json_obj(EditDoc, [revs]),
    JsonDiskDoc = json_doc(DiskDoc),
    case ddoc_prompt(DDoc, [<<"validate_doc_update">>], [JsonEditDoc, JsonDiskDoc, Ctx, SecObj]) of
        1 ->
            ok;
        {[{<<"forbidden">>, Message}]} ->
            throw({forbidden, Message});
        {[{<<"unauthorized">>, Message}]} ->
            throw({unauthorized, Message})
    end.

json_doc(nil) -> null;
json_doc(Doc) ->
    couch_doc:to_json_obj(Doc, [revs]).

filter_view(DDoc, VName, Docs) ->
    JsonDocs = [couch_doc:to_json_obj(Doc, [revs]) || Doc <- Docs],
    [true, Passes] = ddoc_prompt(DDoc, [<<"views">>, VName, <<"map">>], [JsonDocs]),
    {ok, Passes}.

filter_docs(Req, Db, DDoc, FName, Docs) ->
    JsonReq = case Req of
    {json_req, JsonObj} ->
        JsonObj;
    #httpd{} = HttpReq ->
        couch_httpd_external:json_req_obj(HttpReq, Db)
    end,
    JsonDocs = [couch_doc:to_json_obj(Doc, [revs]) || Doc <- Docs],
    [true, Passes] = ddoc_prompt(DDoc, [<<"filters">>, FName],
        [JsonDocs, JsonReq]),
    {ok, Passes}.

ddoc_proc_prompt({Proc, DDocId}, FunPath, Args) ->
    proc_prompt(Proc, [<<"ddoc">>, DDocId, FunPath, Args]).

ddoc_prompt(DDoc, FunPath, Args) ->
    with_ddoc_proc(DDoc, fun({Proc, DDocId}) ->
        proc_prompt(Proc, [<<"ddoc">>, DDocId, FunPath, Args])
    end).

with_ddoc_proc(#doc{id=DDocId,revs={Start, [DiskRev|_]}}=DDoc, Fun) ->
    Rev = couch_doc:rev_to_str({Start, DiskRev}),
    DDocKey = {DDocId, Rev},
    Proc = get_ddoc_process(DDoc, DDocKey),
    try Fun({Proc, DDocId})
    after
        ok = ret_os_process(Proc)
    end.

init([]) ->
    % register async to avoid deadlock on restart_child
    Self = self(),
    spawn(couch_config, register, [fun ?MODULE:config_change/1, Self]),

    Langs = ets:new(couch_query_server_langs, [set, private]),
    LangLimits = ets:new(couch_query_server_lang_limits, [set, private]),
    PidProcs = ets:new(couch_query_server_pid_langs, [set, private]),
    LangProcs = ets:new(couch_query_server_procs, [set, private]),

    ProcTimeout = list_to_integer(couch_config:get(
                        "couchdb", "os_process_timeout", "5000")),
    ReduceLimit = list_to_atom(
        couch_config:get("query_server_config","reduce_limit","true")),
    OsProcLimit = list_to_integer(
        couch_config:get("query_server_config","os_process_limit","10")),

    % 'query_servers' specifies an OS command-line to execute.
    lists:foreach(fun({Lang, Command}) ->
        true = ets:insert(LangLimits, {?l2b(Lang), OsProcLimit, 0}),
        true = ets:insert(Langs, {?l2b(Lang),
                          couch_os_process, start_link, [Command]})
    end, couch_config:get("query_servers")),
    % 'native_query_servers' specifies a {Module, Func, Arg} tuple.
    lists:foreach(fun({Lang, SpecStr}) ->
        {ok, {Mod, Fun, SpecArg}} = couch_util:parse_term(SpecStr),
        true = ets:insert(LangLimits, {?l2b(Lang), 0, 0}), % 0 means no limit
        true = ets:insert(Langs, {?l2b(Lang),
                          Mod, Fun, SpecArg})
    end, couch_config:get("native_query_servers")),


    process_flag(trap_exit, true),
    {ok, #qserver{
        langs = Langs, % Keyed by language name, value is {Mod,Func,Arg}
        pid_procs = PidProcs, % Keyed by PID, valus is a #proc record.
        lang_procs = LangProcs, % Keyed by language name, value is a #proc record
        lang_limits = LangLimits, % Keyed by language name, value is {Lang, Limit, Current}
        config = {[{<<"reduce_limit">>, ReduceLimit},{<<"timeout">>, ProcTimeout}]}
    }}.

terminate(_Reason, #qserver{pid_procs=PidProcs}) ->
    [couch_util:shutdown_sync(P) || {P,_} <- ets:tab2list(PidProcs)],
    ok.

handle_call({get_proc, DDoc1, DDocKey}, From, Server) ->
    #doc{body = {Props}} = DDoc = couch_doc:with_ejson_body(DDoc1),
    Lang = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    case lang_proc(Lang, Server, fun(Procs) ->
            % find a proc in the set that has the DDoc
            proc_with_ddoc(DDoc, DDocKey, Procs)
        end) of
    {ok, Proc} ->
        {reply, {ok, Proc, Server#qserver.config}, Server};
    wait ->
        {noreply, add_to_waitlist({DDoc, DDocKey}, From, Server)};
    Error ->
        {reply, Error, Server}
    end;
handle_call({get_proc, Lang}, From, Server) ->
    case lang_proc(Lang, Server, fun([P|_Procs]) ->
            {ok, P}
        end) of
    {ok, Proc} ->
        {reply, {ok, Proc, Server#qserver.config}, Server};
    wait ->
        {noreply, add_to_waitlist({Lang}, From, Server)};
    Error ->
        {reply, Error, Server}
    end;
handle_call({unlink_proc, Pid}, _From, Server) ->
    unlink(Pid),
    {reply, ok, Server};
handle_call({ret_proc, Proc}, _From, #qserver{
        pid_procs=PidProcs,
        lang_procs=LangProcs}=Server) ->
    % Along with max process limit, here we should check
    % if we're over the limit and discard when we are.
    case is_process_alive(Proc#proc.pid) of
        true ->
            add_value(PidProcs, Proc#proc.pid, Proc),
            add_to_list(LangProcs, Proc#proc.lang, Proc),
            link(Proc#proc.pid);
        false ->
            ok
    end,
    {reply, true, service_waitlist(Server)}.

handle_cast(_Whatever, Server) ->
    {noreply, Server}.

handle_info({'EXIT', _, _}, Server) ->
    {noreply, Server};
handle_info({'DOWN', _, process, Pid, Status}, #qserver{
        pid_procs=PidProcs,
        lang_procs=LangProcs,
        lang_limits=LangLimits}=Server) ->
    case ets:lookup(PidProcs, Pid) of
    [{Pid, Proc}] ->
        case Status of
        normal -> ok;
        _ -> ?LOG_DEBUG("Linked process died abnormally: ~p (reason: ~p)", [Pid, Status])
        end,
        rem_value(PidProcs, Pid),
        catch rem_from_list(LangProcs, Proc#proc.lang, Proc),
        [{Lang, Lim, Current}] = ets:lookup(LangLimits, Proc#proc.lang),
        true = ets:insert(LangLimits, {Lang, Lim, Current-1}),
        {noreply, service_waitlist(Server)};
    [] ->
        case Status of
        normal ->
            {noreply, Server};
        _ ->
            {stop, Status, Server}
        end
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

config_change("query_servers") ->
    supervisor:terminate_child(couch_secondary_services, query_servers),
    supervisor:restart_child(couch_secondary_services, query_servers);
config_change("native_query_servers") ->
    supervisor:terminate_child(couch_secondary_services, query_servers),
    supervisor:restart_child(couch_secondary_services, query_servers);
config_change("query_server_config") ->
    supervisor:terminate_child(couch_secondary_services, query_servers),
    supervisor:restart_child(couch_secondary_services, query_servers).

% Private API

add_to_waitlist(Info, From, #qserver{waitlist=Waitlist}=Server) ->
    Server#qserver{waitlist=[{Info, From}|Waitlist]}.

service_waitlist(#qserver{waitlist=[]}=Server) ->
    Server;
service_waitlist(#qserver{waitlist=Waitlist}=Server) ->
    [Oldest|RevWList] = lists:reverse(Waitlist),
    case service_waiting(Oldest, Server) of
    ok ->
        Server#qserver{waitlist=lists:reverse(RevWList)};
    wait ->
        Server#qserver{waitlist=Waitlist}
    end.

% todo get rid of duplication
service_waiting({{#doc{body={Props}}=DDoc, DDocKey}, From}, Server) ->
    Lang = couch_util:get_value(<<"language">>, Props, <<"javascript">>),
    case lang_proc(Lang, Server, fun(Procs) ->
            % find a proc in the set that has the DDoc
            proc_with_ddoc(DDoc, DDocKey, Procs)
        end) of
    {ok, Proc} ->
        gen_server:reply(From, {ok, Proc, Server#qserver.config}),
        ok;
    wait -> % this should never happen
        wait;
    Error ->
        gen_server:reply(From, Error),
        ok
    end;
service_waiting({{Lang}, From}, Server) ->
    case lang_proc(Lang, Server, fun([P|_Procs]) ->
            {ok, P}
        end) of
    {ok, Proc} ->
        gen_server:reply(From, {ok, Proc, Server#qserver.config}),
        ok;
    wait -> % this should never happen
        wait;
    Error ->
        gen_server:reply(From, Error),
        ok
    end.

lang_proc(Lang, #qserver{
        langs=Langs,
        pid_procs=PidProcs,
        lang_procs=LangProcs,
        lang_limits=LangLimits}, PickFun) ->
    % Note to future self. Add max process limit.
    case ets:lookup(LangProcs, Lang) of
    [{Lang, [P|Procs]}] ->
        {ok, Proc} = PickFun([P|Procs]),
        rem_from_list(LangProcs, Lang, Proc),
        {ok, Proc};
    _ ->
        case (catch new_process(Langs, LangLimits, Lang)) of
        {ok, Proc} ->
            add_value(PidProcs, Proc#proc.pid, Proc),
            PickFun([Proc]);
        ErrorOrWait ->
            ErrorOrWait
        end
    end.

new_process(Langs, LangLimits, Lang) ->
    [{Lang, Lim, Current}] = ets:lookup(LangLimits, Lang),
    if (Lim == 0) or (Current < Lim) -> % Lim == 0 means no limit
        % we are below the limit for our language, make a new one
        case ets:lookup(Langs, Lang) of
        [{Lang, Mod, Func, Arg}] ->
            {ok, Pid} = apply(Mod, Func, Arg),
            erlang:monitor(process, Pid),
            true = ets:insert(LangLimits, {Lang, Lim, Current+1}),
            {ok, #proc{lang=Lang,
                       pid=Pid,
                       % Called via proc_prompt, proc_set_timeout, and proc_stop
                       prompt_fun={Mod, prompt},
                       set_timeout_fun={Mod, set_timeout},
                       stop_fun={Mod, stop}}};
        _ ->
            {unknown_query_language, Lang}
        end;
    true ->
        wait
    end.

proc_with_ddoc(DDoc, DDocKey, LangProcs) ->
    DDocProcs = lists:filter(fun(#proc{ddoc_keys=Keys}) ->
            lists:any(fun(Key) ->
                Key == DDocKey
            end, Keys)
        end, LangProcs),
    case DDocProcs of
        [DDocProc|_] ->
            ?LOG_DEBUG("DDocProc found for DDocKey: ~p",[DDocKey]),
            {ok, DDocProc};
        [] ->
            [TeachProc|_] = LangProcs,
            ?LOG_DEBUG("Teach ddoc to new proc ~p with DDocKey: ~p",[TeachProc, DDocKey]),
            {ok, SmartProc} = teach_ddoc(DDoc, DDocKey, TeachProc),
            {ok, SmartProc}
    end.

proc_prompt(Proc, Args) ->
     case proc_prompt_raw(Proc, Args) of
     {json, Json} ->
         ?JSON_DECODE(Json);
     EJson ->
         EJson
     end.

proc_prompt_raw(#proc{prompt_fun = {Mod, Func}} = Proc, Args) ->
    apply(Mod, Func, [Proc#proc.pid, Args]).

raw_to_ejson({json, Json}) ->
    ?JSON_DECODE(Json);
raw_to_ejson(EJson) ->
    EJson.

proc_stop(Proc) ->
    {Mod, Func} = Proc#proc.stop_fun,
    apply(Mod, Func, [Proc#proc.pid]).

proc_set_timeout(Proc, Timeout) ->
    {Mod, Func} = Proc#proc.set_timeout_fun,
    apply(Mod, Func, [Proc#proc.pid, Timeout]).

teach_ddoc(DDoc, {DDocId, _Rev}=DDocKey, #proc{ddoc_keys=Keys}=Proc) ->
    % send ddoc over the wire
    % we only share the rev with the client we know to update code
    % but it only keeps the latest copy, per each ddoc, around.
    true = proc_prompt(Proc, [<<"ddoc">>, <<"new">>, DDocId, couch_doc:to_json_obj(DDoc, [])]),
    % we should remove any other ddocs keys for this docid
    % because the query server overwrites without the rev
    Keys2 = [{D,R} || {D,R} <- Keys, D /= DDocId],
    % add ddoc to the proc
    {ok, Proc#proc{ddoc_keys=[DDocKey|Keys2]}}.

get_ddoc_process(#doc{} = DDoc, DDocKey) ->
    % remove this case statement
    case gen_server:call(couch_query_servers, {get_proc, DDoc, DDocKey}, infinity) of
    {ok, Proc, {QueryConfig}} ->
        % process knows the ddoc
        case (catch proc_prompt(Proc, [<<"reset">>, {QueryConfig}])) of
        true ->
            proc_set_timeout(Proc, couch_util:get_value(<<"timeout">>, QueryConfig)),
            link(Proc#proc.pid),
            gen_server:call(couch_query_servers, {unlink_proc, Proc#proc.pid}, infinity),
            Proc;
        _ ->
            catch proc_stop(Proc),
            get_ddoc_process(DDoc, DDocKey)
        end;
    Error ->
        throw(Error)
    end.

get_os_process(Lang) ->
    case gen_server:call(couch_query_servers, {get_proc, Lang}, infinity) of
    {ok, Proc, {QueryConfig}} ->
        case (catch proc_prompt(Proc, [<<"reset">>, {QueryConfig}])) of
        true ->
            proc_set_timeout(Proc, couch_util:get_value(<<"timeout">>, QueryConfig)),
            link(Proc#proc.pid),
            gen_server:call(couch_query_servers, {unlink_proc, Proc#proc.pid}, infinity),
            Proc;
        _ ->
            catch proc_stop(Proc),
            get_os_process(Lang)
        end;
    Error ->
        throw(Error)
    end.

ret_os_process(Proc) ->
    true = gen_server:call(couch_query_servers, {ret_proc, Proc}, infinity),
    catch unlink(Proc#proc.pid),
    ok.

add_value(Tid, Key, Value) ->
    true = ets:insert(Tid, {Key, Value}).

rem_value(Tid, Key) ->
    true = ets:delete(Tid, Key).

add_to_list(Tid, Key, Value) ->
    case ets:lookup(Tid, Key) of
    [{Key, Vals}] ->
        true = ets:insert(Tid, {Key, [Value|Vals]});
    [] ->
        true = ets:insert(Tid, {Key, [Value]})
    end.

rem_from_list(Tid, Key, Value) when is_record(Value, proc)->
    Pid = Value#proc.pid,
    case ets:lookup(Tid, Key) of
    [{Key, Vals}] ->
        % make a new values list that doesn't include the Value arg
        NewValues = [Val || #proc{pid=P}=Val <- Vals, P /= Pid],
        ets:insert(Tid, {Key, NewValues});
    [] -> ok
    end;
rem_from_list(Tid, Key, Value) ->
    case ets:lookup(Tid, Key) of
    [{Key, Vals}] ->
        % make a new values list that doesn't include the Value arg
        NewValues = [Val || Val <- Vals, Val /= Value],
        ets:insert(Tid, {Key, NewValues});
    [] -> ok
    end.
