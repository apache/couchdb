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

-export([start_link/0]).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,code_change/3,stop/0]).
-export([start_doc_map/2, map_docs/2, stop_doc_map/1]).
-export([reduce/3, rereduce/3,validate_doc_update/5]).
-export([render_doc_show/6, render_doc_update/6, start_view_list/2,
        render_list_head/4, render_list_row/4, render_list_tail/1]).
-export([start_filter/2, filter_doc/4, end_filter/1]).
% -export([test/0]).

-include("couch_db.hrl").

-record(proc, {
    pid,
    lang,
    prompt_fun,
    set_timeout_fun,
    stop_fun
}).

start_link() ->
    gen_server:start_link({local, couch_query_servers}, couch_query_servers, [], []).

stop() ->
    exit(whereis(couch_query_servers), close).

start_doc_map(Lang, Functions) ->
    Proc = get_os_process(Lang),
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
    Proc = get_os_process(Lang),
    Grouped = group_reductions_results(ReducedValues),
    Results = try lists:zipwith(
        fun
        (<<"_", _/binary>> = FunSrc, Values) ->
            {ok, [Result]} = builtin_reduce(rereduce, [FunSrc], [[[], V] || V <- Values], []),
            Result;
        (FunSrc, Values) ->
            [true, [Result]] =
                proc_prompt(Proc, [<<"rereduce">>, [FunSrc], Values]),
            Result
        end, RedSrcs, Grouped)
    after
        ok = ret_os_process(Proc)
    end,
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

builtin_reduce(_Re, [], _KVs, Acc) ->
    {ok, lists:reverse(Acc)};
builtin_reduce(Re, [<<"_sum">>|BuiltinReds], KVs, Acc) ->
    Sum = builtin_sum_rows(KVs),
    builtin_reduce(Re, BuiltinReds, KVs, [Sum|Acc]);
builtin_reduce(reduce, [<<"_count">>|BuiltinReds], KVs, Acc) ->
    Count = length(KVs),
    builtin_reduce(reduce, BuiltinReds, KVs, [Count|Acc]);
builtin_reduce(rereduce, [<<"_count">>|BuiltinReds], KVs, Acc) ->
    Count = builtin_sum_rows(KVs),
    builtin_reduce(rereduce, BuiltinReds, KVs, [Count|Acc]).

builtin_sum_rows(KVs) ->
    lists:foldl(fun
        ([_Key, Value], Acc) when is_number(Value) ->
            Acc + Value;
        (_Else, _Acc) ->
            throw({invalid_value, <<"builtin _sum function requires map values to be numbers">>})
    end, 0, KVs).

validate_doc_update(Lang, FunSrc, EditDoc, DiskDoc, Ctx) ->
    Proc = get_os_process(Lang),
    JsonEditDoc = couch_doc:to_json_obj(EditDoc, [revs]),
    JsonDiskDoc =
    if DiskDoc == nil ->
        null;
    true ->
        couch_doc:to_json_obj(DiskDoc, [revs])
    end,
    try proc_prompt(Proc,
            [<<"validate">>, FunSrc, JsonEditDoc, JsonDiskDoc, Ctx]) of
    1 ->
        ok;
    {[{<<"forbidden">>, Message}]} ->
        throw({forbidden, Message});
    {[{<<"unauthorized">>, Message}]} ->
        throw({unauthorized, Message})
    after
        ok = ret_os_process(Proc)
    end.
% todo use json_apply_field
append_docid(DocId, JsonReqIn) ->
    [{<<"docId">>, DocId} | JsonReqIn].

render_doc_show(Lang, ShowSrc, DocId, Doc, Req, Db) ->
    Proc = get_os_process(Lang),
    {JsonReqIn} = couch_httpd_external:json_req_obj(Req, Db),

    {JsonReq, JsonDoc} = case {DocId, Doc} of
        {nil, nil} -> {{JsonReqIn}, null};
        {DocId, nil} -> {{append_docid(DocId, JsonReqIn)}, null};
        _ -> {{append_docid(DocId, JsonReqIn)}, couch_doc:to_json_obj(Doc, [revs])}
    end,
    try proc_prompt(Proc, [<<"show">>, ShowSrc, JsonDoc, JsonReq])
    after
        ok = ret_os_process(Proc)
    end.

render_doc_update(Lang, UpdateSrc, DocId, Doc, Req, Db) ->
    Proc = get_os_process(Lang),
    {JsonReqIn} = couch_httpd_external:json_req_obj(Req, Db),

    {JsonReq, JsonDoc} = case {DocId, Doc} of
        {nil, nil} -> {{JsonReqIn}, null};
        {DocId, nil} -> {{append_docid(DocId, JsonReqIn)}, null};
        _ -> {{append_docid(DocId, JsonReqIn)}, couch_doc:to_json_obj(Doc, [revs])}
    end,
    try proc_prompt(Proc, [<<"update">>, UpdateSrc, JsonDoc, JsonReq])
    after
        ok = ret_os_process(Proc)
    end.

start_view_list(Lang, ListSrc) ->
    Proc = get_os_process(Lang),
    proc_prompt(Proc, [<<"add_fun">>, ListSrc]),
    {ok, Proc}.

render_list_head(Proc, Req, Db, Head) ->
    JsonReq = couch_httpd_external:json_req_obj(Req, Db),
    proc_prompt(Proc, [<<"list">>, Head, JsonReq]).

render_list_row(Proc, Db, {{Key, DocId}, Value}, IncludeDoc) ->
    JsonRow = couch_httpd_view:view_row_obj(Db, {{Key, DocId}, Value}, IncludeDoc),
    proc_prompt(Proc, [<<"list_row">>, JsonRow]);

render_list_row(Proc, _, {Key, Value}, _IncludeDoc) ->
    JsonRow = {[{key, Key}, {value, Value}]},
    proc_prompt(Proc, [<<"list_row">>, JsonRow]).

render_list_tail(Proc) ->
    JsonResp = proc_prompt(Proc, [<<"list_end">>]),
    ok = ret_os_process(Proc),
    JsonResp.

start_filter(Lang, FilterSrc) ->
    Proc = get_os_process(Lang),
    true = proc_prompt(Proc, [<<"add_fun">>, FilterSrc]),
    {ok, Proc}.

filter_doc(Proc, Doc, Req, Db) ->
    JsonReq = couch_httpd_external:json_req_obj(Req, Db),
    JsonDoc = couch_doc:to_json_obj(Doc, [revs]),
    JsonCtx = couch_util:json_user_ctx(Db),
    [true, [Pass]] = proc_prompt(Proc,
        [<<"filter">>, [JsonDoc], JsonReq, JsonCtx]),
    {ok, Pass}.

end_filter(Proc) ->
    ok = ret_os_process(Proc).
    

init([]) ->

    % read config and register for configuration changes

    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.

    ok = couch_config:register(
        fun("query_servers" ++ _, _) ->
            ?MODULE:stop()
        end),
    ok = couch_config:register(
        fun("native_query_servers" ++ _, _) ->
            ?MODULE:stop()
        end),

    Langs = ets:new(couch_query_server_langs, [set, private]),
    PidProcs = ets:new(couch_query_server_pid_langs, [set, private]),
    LangProcs = ets:new(couch_query_server_procs, [set, private]),
    InUse = ets:new(couch_query_server_used, [set, private]),
    % 'query_servers' specifies an OS command-line to execute.
    lists:foreach(fun({Lang, Command}) ->
        true = ets:insert(Langs, {?l2b(Lang),
                          couch_os_process, start_link, [Command]})
    end, couch_config:get("query_servers")),
    % 'native_query_servers' specifies a {Module, Func, Arg} tuple.
    lists:foreach(fun({Lang, SpecStr}) ->
        {ok, {Mod, Fun, SpecArg}} = couch_util:parse_term(SpecStr),
        true = ets:insert(Langs, {?l2b(Lang),
                          Mod, Fun, SpecArg})
    end, couch_config:get("native_query_servers")),
    process_flag(trap_exit, true),
    {ok, {Langs, % Keyed by language name, value is {Mod,Func,Arg}
          PidProcs, % Keyed by PID, valus is a #proc record.
          LangProcs, % Keyed by language name, value is a #proc record
          InUse % Keyed by PID, value is #proc record.
          }}.

terminate(_Reason, _Server) ->
    ok.


handle_call({get_proc, Lang}, _From, {Langs, PidProcs, LangProcs, InUse}=Server) ->
    % Note to future self. Add max process limit.
    case ets:lookup(LangProcs, Lang) of
    [{Lang, [Proc|_]}] ->
        add_value(PidProcs, Proc#proc.pid, Proc),
        rem_from_list(LangProcs, Lang, Proc),
        add_to_list(InUse, Lang, Proc),
        {reply, {recycled, Proc, get_query_server_config()}, Server};
    _ ->
        case (catch new_process(Langs, Lang)) of
        {ok, Proc} ->
            add_to_list(InUse, Lang, Proc),
            {reply, {new, Proc}, Server};
        Error ->
            {reply, Error, Server}
        end
    end;
handle_call({ret_proc, Proc}, _From, {_, _, LangProcs, InUse}=Server) ->
    % Along with max process limit, here we should check
    % if we're over the limit and discard when we are.
    add_to_list(LangProcs, Proc#proc.lang, Proc),
    rem_from_list(InUse, Proc#proc.lang, Proc),
    {reply, true, Server}.

handle_cast(_Whatever, Server) ->
    {noreply, Server}.

handle_info({'EXIT', Pid, Status}, {_, PidProcs, LangProcs, InUse}=Server) ->
    case ets:lookup(PidProcs, Pid) of
    [{Pid, Proc}] ->
        case Status of
        normal -> ok;
        _ -> ?LOG_DEBUG("Linked process died abnormally: ~p (reason: ~p)", [Pid, Status])
        end,
        rem_value(PidProcs, Pid),
        catch rem_from_list(LangProcs, Proc#proc.lang, Proc),
        catch rem_from_list(InUse, Proc#proc.lang, Proc),
        {noreply, Server};
    [] ->
        ?LOG_DEBUG("Unknown linked process died: ~p (reason: ~p)", [Pid, Status]),
        {stop, Status, Server}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API

get_query_server_config() ->
    ReduceLimit = list_to_atom(
        couch_config:get("query_server_config","reduce_limit","true")),
    {[{<<"reduce_limit">>, ReduceLimit}]}.

new_process(Langs, Lang) ->
    case ets:lookup(Langs, Lang) of
    [{Lang, Mod, Func, Arg}] ->
        {ok, Pid} = apply(Mod, Func, Arg),
        {ok, #proc{lang=Lang,
                   pid=Pid,
                   % Called via proc_prompt, proc_set_timeout, and proc_stop
                   prompt_fun={Mod, prompt},
                   set_timeout_fun={Mod, set_timeout},
                   stop_fun={Mod, stop}}};
    _ ->
        {unknown_query_language, Lang}
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

get_os_process(Lang) ->
    case gen_server:call(couch_query_servers, {get_proc, Lang}) of
    {new, Proc} ->
        proc_set_timeout(Proc, list_to_integer(couch_config:get(
                            "couchdb", "os_process_timeout", "5000"))),
        link(Proc#proc.pid),
        Proc;
    {recycled, Proc, QueryConfig} ->
        case (catch proc_prompt(Proc, [<<"reset">>, QueryConfig])) of
        true ->
            proc_set_timeout(Proc, list_to_integer(couch_config:get(
                                "couchdb", "os_process_timeout", "5000"))),
            link(Proc#proc.pid),
            Proc;
        _ ->
            catch proc_stop(Proc),
            get_os_process(Lang)
        end;
    Error ->
        throw(Error)
    end.

ret_os_process(Proc) ->
    true = gen_server:call(couch_query_servers, {ret_proc, Proc}),
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

rem_from_list(Tid, Key, Value) ->
    case ets:lookup(Tid, Key) of
    [{Key, Vals}] ->
        ets:insert(Tid, {Key, [Val || Val <- Vals, Val /= Value]});
    [] -> ok
    end.
