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

-export([start_doc_map/3, map_docs/2, stop_doc_map/1]).
-export([reduce/3, rereduce/3,validate_doc_update/5]).
-export([filter_docs/5]).

-export([with_ddoc_proc/2, proc_prompt/2, ddoc_prompt/3, ddoc_proc_prompt/3, json_doc/1]).
-export([get_os_process/1, ret_os_process/1]).

% -export([test/0]).

-include("couch_db.hrl").

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
os_reduce(#proc{} = Proc, OsRedSrcs, KVs) ->
    [true, Reductions] = proc_prompt(Proc, [<<"reduce">>, OsRedSrcs, KVs]),
    {ok, Reductions};
os_reduce(Lang, OsRedSrcs, KVs) ->
    Proc = get_os_process(Lang),
    try os_reduce(Proc, OsRedSrcs, KVs) after ok = ret_os_process(Proc) end.

os_rereduce(#proc{} = Proc, OsRedSrcs, KVs) ->
    [true, [Reduction]] = proc_prompt(Proc, [<<"rereduce">>, OsRedSrcs, KVs]),
    Reduction;
os_rereduce(Lang, OsRedSrcs, KVs) ->
    Proc = get_os_process(Lang),
    try os_rereduce(Proc, OsRedSrcs, KVs) after ok = ret_os_process(Proc) end.

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

builtin_stats(_, []) ->
    {[{sum,0}, {count,0}, {min,0}, {max,0}, {sumsqr,0}]};

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

filter_docs(Req, Db, DDoc, FName, Docs) ->
    JsonReq = case Req of
    {json_req, JsonObj} ->
        JsonObj;
    #httpd{} = HttpReq ->
        couch_httpd_external:json_req_obj(HttpReq, Db)
    end,
    JsonDocs = [couch_doc:to_json_obj(Doc, [revs]) || Doc <- Docs],
    [true, Passes] = ddoc_prompt(DDoc, [<<"filters">>, FName], [JsonDocs, JsonReq]),
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

proc_prompt(Proc, Args) ->
    {Mod, Func} = Proc#proc.prompt_fun,
    apply(Mod, Func, [Proc#proc.pid, Args]).

proc_stop(Proc) ->
    {Mod, Func} = Proc#proc.stop_fun,
    apply(Mod, Func, [Proc#proc.pid]).

proc_set_timeout(Proc, Timeout) ->
    {Mod, Func} = Proc#proc.set_timeout_fun,
    apply(Mod, Func, [Proc#proc.pid, Timeout]).

get_ddoc_process(#doc{} = DDoc, DDocKey) ->
    % remove this case statement
    case gen_server:call(couch_proc_manager, {get_proc, DDoc, DDocKey}) of
    {ok, Proc, QueryConfig} ->
        % process knows the ddoc
        case (catch proc_prompt(Proc, [<<"reset">>, QueryConfig])) of
        true ->
            proc_set_timeout(Proc, list_to_integer(couch_config:get(
                                "couchdb", "os_process_timeout", "5000"))),
            Proc;
        _ ->
            catch proc_stop(Proc),
            get_ddoc_process(DDoc, DDocKey)
        end;
    Error ->
        throw(Error)
    end.

get_os_process(Lang) ->
    case gen_server:call(couch_proc_manager, {get_proc, Lang}) of
    {ok, Proc, QueryConfig} ->
        case (catch proc_prompt(Proc, [<<"reset">>, QueryConfig])) of
        true ->
            proc_set_timeout(Proc, list_to_integer(couch_config:get(
                                "couchdb", "os_process_timeout", "5000"))),
            Proc;
        _ ->
            catch proc_stop(Proc),
            get_os_process(Lang)
        end;
    Error ->
        throw(Error)
    end.

ret_os_process(Proc) ->
    true = gen_server:call(couch_proc_manager, {ret_proc, Proc}),
    catch unlink(Proc#proc.pid),
    ok.
