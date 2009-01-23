% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License.  You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_query_servers).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,code_change/3,stop/0]).
-export([start_doc_map/2, map_docs/2, stop_doc_map/1]).
-export([reduce/3, rereduce/3,validate_doc_update/5]).
-export([render_doc_show/5,start_view_list/2,render_list_head/5, render_list_row/4, render_list_tail/3]).
% -export([test/0]).

-include("couch_db.hrl").

start_link() ->
    gen_server:start_link({local, couch_query_servers}, couch_query_servers, [], []).

stop() ->
    exit(whereis(couch_query_servers), close).

start_doc_map(Lang, Functions) ->
    Pid = get_os_process(Lang),
    lists:foreach(fun(FunctionSource) ->
        true = couch_os_process:prompt(Pid, [<<"add_fun">>, FunctionSource])
    end, Functions),
    {ok, {Lang, Pid}}.

map_docs({_Lang, Pid}, Docs) ->
    % send the documents
    Results = lists:map(
        fun(Doc) ->
            Json = couch_doc:to_json_obj(Doc, []),
            
            FunsResults = couch_os_process:prompt(Pid, [<<"map_doc">>, Json]),
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
stop_doc_map({Lang, Pid}) ->
    ok = ret_os_process(Lang, Pid).

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
    Pid = get_os_process(Lang),
    Grouped = group_reductions_results(ReducedValues),
    Results = lists:zipwith(
        fun(FunSrc, Values) ->
            [true, [Result]] = 
                couch_os_process:prompt(Pid, [<<"rereduce">>, [FunSrc], Values]),
            Result
        end, RedSrcs, Grouped),
        
    ok = ret_os_process(Lang, Pid),
    {ok, Results}.

reduce(_Lang, [], _KVs) ->
    {ok, []};
reduce(Lang, RedSrcs, KVs) ->
    Pid = get_os_process(Lang),
    [true, Results] = couch_os_process:prompt(Pid, 
            [<<"reduce">>, RedSrcs, KVs]),
    ok = ret_os_process(Lang, Pid),
    {ok, Results}.

validate_doc_update(Lang, FunSrc, EditDoc, DiskDoc, Ctx) ->
    Pid = get_os_process(Lang),
    JsonEditDoc = couch_doc:to_json_obj(EditDoc, [revs]),
    JsonDiskDoc =
    if DiskDoc == nil ->
        null;
    true -> 
        couch_doc:to_json_obj(DiskDoc, [revs])
    end,
    try couch_os_process:prompt(Pid, 
            [<<"validate">>, FunSrc, JsonEditDoc, JsonDiskDoc, Ctx]) of
    1 ->
        ok;
    {[{<<"forbidden">>, Message}]} ->
        throw({forbidden, Message});
    {[{<<"unauthorized">>, Message}]} ->
        throw({unauthorized, Message})
    after
        ok = ret_os_process(Lang, Pid)
    end.

render_doc_show(Lang, ShowSrc, Doc, Req, Db) ->
    Pid = get_os_process(Lang),
    JsonDoc = couch_doc:to_json_obj(Doc, [revs]),
    JsonReq = couch_httpd_external:json_req_obj(Req, Db),
    try couch_os_process:prompt(Pid, 
        [<<"show_doc">>, ShowSrc, JsonDoc, JsonReq]) of
    FormResp ->
        FormResp
    after
        ok = ret_os_process(Lang, Pid)
    end.

start_view_list(Lang, ListSrc) ->
    Pid = get_os_process(Lang),
    true = couch_os_process:prompt(Pid, [<<"add_fun">>, ListSrc]),
    {ok, {Lang, Pid}}.

render_list_head({_Lang, Pid}, Req, Db, TotalRows, Offset) ->
    Head = {[{<<"total_rows">>, TotalRows}, {<<"offset">>, Offset}]},
    JsonReq = couch_httpd_external:json_req_obj(Req, Db),
    couch_os_process:prompt(Pid, [<<"list_begin">>, Head, JsonReq]).

render_list_row({_Lang, Pid}, Req, Db, {{Key, DocId}, Value}) ->
    JsonRow = couch_httpd_view:view_row_obj(Db, {{Key, DocId}, Value}, false),
    JsonReq = couch_httpd_external:json_req_obj(Req, Db),
    couch_os_process:prompt(Pid, [<<"list_row">>, JsonRow, JsonReq]).

render_list_tail({Lang, Pid}, Req, Db) ->
    JsonReq = couch_httpd_external:json_req_obj(Req, Db),
    JsonResp = couch_os_process:prompt(Pid, [<<"list_tail">>, JsonReq]),
    ok = ret_os_process(Lang, Pid),
    JsonResp.
    
    

init([]) ->
    
    % read config and register for configuration changes
    
    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.
    
    ok = couch_config:register(
        fun("query_servers" ++ _, _) ->
            ?MODULE:stop()
        end),

    Langs = ets:new(couch_query_server_langs, [set, private]),
    PidLangs = ets:new(couch_query_server_pid_langs, [set, private]),
    Pids = ets:new(couch_query_server_procs, [set, private]),
    InUse = ets:new(couch_query_server_used, [set, private]),
    lists:foreach(fun({Lang, Command}) ->
        true = ets:insert(Langs, {?l2b(Lang), Command})
    end, couch_config:get("query_servers")),
    {ok, {Langs, PidLangs, Pids, InUse}}.

terminate(_Reason, _Server) ->
    ok.


handle_call({get_proc, Lang}, _From, {Langs, PidLangs, Pids, InUse}=Server) ->
    % Note to future self. Add max process limit.
    case ets:lookup(Pids, Lang) of
    [{Lang, [Pid|_]}] ->
        add_value(PidLangs, Pid, Lang),
        rem_from_list(Pids, Lang, Pid),
        add_to_list(InUse, Lang, Pid),
        true = couch_os_process:prompt(Pid, [<<"reset">>]),
        {reply, Pid, Server};
    _ ->
        {ok, Pid} = new_process(Langs, Lang),
        add_to_list(InUse, Lang, Pid),
        {reply, Pid, Server}
    end;
handle_call({ret_proc, Lang, Pid}, _From, {_, _, Pids, InUse}=Server) ->
    % Along with max process limit, here we should check
    % if we're over the limit and discard when we are.
    add_to_list(Pids, Lang, Pid),
    rem_from_list(InUse, Lang, Pid),
    {reply, true, Server}.

handle_cast(_Whatever, Server) ->
    {noreply, Server}.

handle_info({'EXIT', Pid, Status}, {Langs, PidLangs, Pids, InUse}) ->
    case ets:lookup(PidLangs, Pid) of
    [{Pid, Lang}] ->
        case Status of
        normal -> ok;
        _ -> ?LOG_DEBUG("Linked process died abnromally: ~p (reason: ~p)", [Pid, Status])
        end,
        {ok, {
            Langs,
            rem_value(PidLangs, Pid),
            rem_from_list(Pids, Lang, Pid),
            rem_from_list(InUse, Lang, Pid)
        }};
    [] ->
        ?LOG_DEBUG("Unknown linked process died: ~p (reason: ~p)", [Pid, Status]),
        {ok, {Langs, PidLangs, Pids, InUse}}
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% Private API

new_process(Langs, Lang) ->
    Proc =
    case ets:lookup(Langs, Lang) of
    [{Lang, Command}] ->
        couch_os_process:start_link(Command);
    _ ->
        throw({unknown_query_language, Lang})
    end,
    Proc.

get_os_process(Lang) ->
    gen_server:call(couch_query_servers, {get_proc, Lang}).

ret_os_process(Lang, Pid) ->
    true = gen_server:call(couch_query_servers, {ret_proc, Lang, Pid}),
    ok.

add_value(Tid, Key, Value) ->
    true = ets:insert(Tid, {Key, Value}).

rem_value(Tid, Key) ->
    true = ets:insert(Tid, Key).

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
