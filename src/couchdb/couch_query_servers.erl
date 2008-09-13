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
-export([reduce/3, rereduce/3]).
% -export([test/0]).

-include("couch_db.hrl").

start_link() ->
    gen_server:start_link({local, couch_query_servers}, couch_query_servers, [], []).

stop() ->
    exit(whereis(couch_query_servers), close).

readline(Port) ->
    readline(Port, []).

readline(Port, Acc) ->
    case get(query_server_timeout) of
    undefined ->
        Timeout = list_to_integer(couch_config:get(
            "couchdb", "view_timeout", "5000")),
        put(query_server_timeout, Timeout);
    Timeout -> ok
    end,
    receive
    {Port, {data, {noeol, Data}}} ->
        readline(Port, [Data|Acc]);
    {Port, {data, {eol, Data}}} ->
        lists:reverse(Acc, Data);
    {Port, Err} ->
        catch port_close(Port),
        throw({map_process_error, Err})
    after Timeout ->
        catch port_close(Port),
        throw({map_process_error, "map function timed out"})
    end.

read_json(Port) ->
    Line = readline(Port),
    case ?JSON_DECODE(Line) of
    {[{<<"log">>,Msg}]} when is_binary(Msg) ->
        % we got a message to log. Log it and continue
        ?LOG_INFO("Query Server Log Message: ~s", [Msg]),
        read_json(Port);
    Else ->
        Else
    end.

% send command and get a response.
prompt(Port, Json) ->
    Bin = iolist_to_binary([?JSON_ENCODE(Json) , "\n"]),
    true = port_command(Port, Bin),
    case read_json(Port) of
    {[{<<"error">>, Id}, {<<"reason">>, Reason}]} ->
        throw({list_to_atom(binary_to_list(Id)),Reason});
    {[{<<"reason">>, Reason}, {<<"error">>, Id}]} ->
        throw({list_to_atom(binary_to_list(Id)),Reason});
    Result ->
        Result
    end.


start_doc_map(Lang, Functions) ->
    Port = get_linked_port(Lang),
    % send the functions as json strings
    lists:foreach(fun(FunctionSource) ->
            true = prompt(Port, [<<"add_fun">>, FunctionSource])
        end,
        Functions),
    {ok, {Lang, Port}}.

map_docs({_Lang, Port}, Docs) ->
    % send the documents
    Results = lists:map(
        fun(Doc) ->
            Json = couch_doc:to_json_obj(Doc, []),
            
            FunsResults = prompt(Port, [<<"map_doc">>, Json]),
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
stop_doc_map({Lang, Port}) ->
    return_linked_port(Lang, Port).
    
get_linked_port(Lang) ->
    case gen_server:call(couch_query_servers, {get_port, Lang}) of
    {ok, Port0} ->
        link(Port0),
        true = prompt(Port0, [<<"reset">>]),
        Port0;
    {empty, Cmd} ->
        ?LOG_INFO("Spawning new ~s instance.", [Lang]),
        open_port({spawn, Cmd}, [stream,
                                    {line, 1000},
                                    binary,
                                    exit_status,
                                    hide]);
    Error ->
        throw(Error)
    end.

return_linked_port(Lang, Port) ->
    ok = gen_server:call(couch_query_servers, {return_port, {Lang, Port}}),
    true = unlink(Port),
    ok.

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
    Port = get_linked_port(Lang),
    Grouped = group_reductions_results(ReducedValues),
    Results = lists:zipwith(
        fun(FunSrc, Values) ->
            [true, [Result]] = 
                prompt(Port, [<<"rereduce">>, [FunSrc], Values]),
            Result
        end, RedSrcs, Grouped),
        
    return_linked_port(Lang, Port),
    {ok, Results}.

reduce(_Lang, [], _KVs) ->
    {ok, []};
reduce(Lang, RedSrcs, KVs) ->
    Port = get_linked_port(Lang),
    [true, Results] = prompt(Port, 
            [<<"reduce">>, RedSrcs, KVs]),
    return_linked_port(Lang, Port),
    {ok, Results}.


init([]) ->
    
    % read config and register for configuration changes
    
    % just stop if one of the config settings change. couch_server_sup
    % will restart us and then we will pick up the new settings.
    
    ok = couch_config:register(
        fun("query_servers" ++ _, _) ->
            ?MODULE:stop()
        end),
        
    QueryServers = couch_config:get("query_servers"),
    QueryServers2 = 
        [{list_to_binary(Lang), Path} || {Lang, Path} <- QueryServers],
        
    {ok, {QueryServers2, []}}.

terminate(_Reason, _Server) ->
    ok.


handle_call({get_port, Lang}, {FromPid, _}, {QueryServerList, LangPorts}) ->
    case proplists:get_value(Lang, LangPorts) of
    undefined ->
        case proplists:get_value(Lang, QueryServerList) of
        undefined -> % not a supported language
            {reply, {query_language_unknown, Lang}, {QueryServerList, LangPorts}};
        ServerCmd ->
            {reply, {empty, ServerCmd}, {QueryServerList, LangPorts}}
        end;
    Port ->
        Result =
        case catch port_connect(Port, FromPid) of
        true ->
            true = unlink(Port),
            {ok, Port};
        Error ->
            catch port_close(Port),
            Error
        end,
        {reply, Result, {QueryServerList, LangPorts -- [{Lang,Port}]}}
    end;
handle_call({return_port, {Lang, Port}}, _From, {QueryServerList, LangPorts}) ->
    case catch port_connect(Port, self()) of
    true ->
        {reply, ok, {QueryServerList, [{Lang, Port} | LangPorts]}};
    _ ->
        catch port_close(Port),
        {reply, ok, {QueryServerList, LangPorts}}
    end.

handle_cast(_Whatever, {Cmd, Ports}) ->
    {noreply, {Cmd, Ports}}.

handle_info({Port, {exit_status, Status}}, {QueryServerList, LangPorts}) ->
    case lists:keysearch(Port, 2, LangPorts) of
    {value, {Lang, _}} ->
        case Status of
        0 -> ok;
        _ -> ?LOG_ERROR("Abnormal shutdown of ~s query server process (exit_status: ~w).", [Lang, Status])
        end,
        {noreply, {QueryServerList,  lists:keydelete(Port, 2, LangPorts)}};
    _ ->
        ?LOG_ERROR("Unknown linked port/process crash: ~p", [Port])
    end.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

% test() ->
%     test("../js/js -f main.js").
% 
% test(Cmd) ->
%     start_link(Cmd),
%     {ok, DocMap} = start_doc_map(<<"javascript">>, [<<"function(doc) {if (doc[0] == 'a') return doc[1];}">>]),
%     {ok, Results} = map_docs(DocMap, [#doc{body={"a", "b"}}, #doc{body={"c", "d"}},#doc{body={"a", "c"}}]),
%     io:format("Results: ~w~n", [Results]),
%     stop_doc_map(DocMap),
%     ok.
