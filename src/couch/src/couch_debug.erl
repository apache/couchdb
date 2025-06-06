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

-module(couch_debug).

-export([
    help/0,
    help/1
]).

-export([
    opened_files/0,
    opened_files_by_regexp/1,
    opened_files_contains/1
]).

-export([
    process_name/1,
    get_pid/1,
    link_tree/1,
    link_tree/2,
    mapfold_tree/3,
    map_tree/2,
    fold_tree/3,
    linked_processes_info/2,
    print_linked_processes/1,
    memory_info/1,
    memory_info/2,
    resource_hoggers/2,
    resource_hoggers_snapshot/1,
    analyze_resource_hoggers/2,
    busy/2,
    busy/3,
    restart/1,
    restart_busy/2,
    restart_busy/3,
    restart_busy/4,
    dead_nodes/0,
    dead_nodes/1,
    ping/1,
    ping/2,
    ping_live_cluster_nodes/0,
    ping_live_cluster_nodes/1,
    ping_nodes/1,
    ping_nodes/2,
    node_events/0
]).

-export([
    print_table/2,
    print_report/1,
    print_report_with_info_width/2,
    print_tree/2
]).

-define(PING_TIMEOUT_IN_MS, 60000).

-type throw(_Reason) :: no_return().

-type process_name() :: atom().
-type process() :: process_name() | pid().
-type function_name() :: atom().
-type busy_properties() ::
    heap_size
    | memory
    | message_queue_len
    | reductions
    | total_heap_size.

-spec help() -> [function_name()].

help() ->
    [
        busy,
        opened_files,
        opened_files_by_regexp,
        opened_files_contains,
        process_name,
        get_pid,
        link_tree,
        mapfold_tree,
        fold_tree,
        map_tree,
        linked_processes_info,
        print_linked_processes,
        memory_info,
        resource_hoggers,
        resource_hoggers_snapshot,
        analyze_resource_hoggers,
        print_table,
        print_report,
        print_report_with_info_width,
        print_tree,
        restart,
        restart_busy,
        dead_nodes,
        ping,
        ping_nodes,
        node_events
    ].

-spec help(Function :: function_name()) -> ok.
%% erlfmt-ignore
help(busy) ->
    io:format("
    busy(ProcessList, Threshold)
    busy(ProcessList, Threshold, Property)
    --------------

    Iterate over given list of named processes or pids and returns the ones with
    a Property value greater than provided Threshold.

    If Property is not specified we use message box size

    Properties which can be used are listed below

    - heap_size
    - memory
    - message_queue_len (default)
    - reductions
    - total_heap_size

    ---
    ", []);
help(opened_files) ->
    io:format("
    opened_files()
    --------------

    Returns list of currently opened couch_file files. It iterates through all
    the processes() and returns only those which have the Fd and Path set by
    couch_file in the process dictionary.

    It uses `process_info(Pid, dictionary)` to get info about couch_file properties.
    ---
    ", []);
help(opened_files_by_regexp) ->
    io:format("
    opened_files_by_regexp(FileRegExp)
    ----------------------------------

    Returns list of currently opened couch_file files which name match the provided regular expression.
    It uses `process_info(Pid, dictionary)` to get info about couch_file properties.
    ---
    ", []);
help(opened_files_contains) ->
    io:format("
    opened_files_contains(SubString)
    --------------------------------

    Returns list of currently opened couch_file files whose names contain the provided SubString.
    It uses `process_info(Pid, dictionary)` to get info about couch_file properties.
    ---
    ", []);
help(process_name) ->
    io:format("
    process_name(Pid)
    -----------------

    Uses heuristics to figure out the process name.
    The heuristic is based on the following information about the process:
    - process_info(Pid, registered_name)
    - '$initial_call' key in process dictionary
    - process_info(Pid, initial_call)

    ---
    ", []);
help(get_pid) ->
    io:format("
    get_pid(PidOrName)
    -----------------

    Get the pid for a process name given either a name or pid. When a pid is given, it returns it as is.
    This has the same functionality as whereis/1 except it will not crash when a pid is given.

    ---
    ", []);
help(restart) ->
    io:format("
    restart(ServerName)
    --------------

    Restart a process with given ServerName and wait for
    replacement process to start.
    ---
    ", []);
help(restart_busy) ->
    io:format("
    restart_busy(ProcessList, Thereshold)
    restart_busy(ProcessList, Thereshold, DelayInMsec)
    --------------

    Iterate over given list of named processes and returns the ones with
    a Property value greater than provided Threshold.

    Then it restart the identified processes.

    If Property is not specified we use message box size

    Properties which can be used are listed below

    - heap_size
    - memory
    - message_queue_len (default)
    - reductions
    - total_heap_size

    The restarts happen sequentially with a given DelayInMsec between them.
    If DelayInMsec is not provided the default value is one second.
    The function doesn't proceed to next process until
    the replacement process starts.
    ---
    ", []);
help(link_tree) ->
    io:format("
    link_tree(Pid)
    --------------

    Returns a tree which represents a cluster of linked processes.
    This function receives the initial Pid to start from.
    The function doesn't recurse to pids older than initial one.
    The Pids which are lesser than initial Pid are still shown in the output.
    This is analogue of `link_tree(RootPid, []).`

    link_tree(Pid, Info)
    --------------------

    Returns a tree which represents a cluster of linked processes.
    This function receives the initial Pid to start from.
    The function doesn't recurse to pids older than initial one.
    The Pids which are lesser than initial Pid are still shown in the output.
    The info argument is a list of process_info_item() as documented in
    erlang:process_info/2. We don't do any attempts to prevent dangerous items.
    Be warn that passing some of them such as `messages` for example
    can be dangerous in a very busy system.
    ---
    ", []);
help(mapfold_tree) ->
    io:format("
    mapfold_tree(Tree, Acc, Fun)
    -----------------------

    Traverses all nodes of the tree. It is a combination of a map and fold.
    It calls a user provided callback for every node of the tree.
    `Fun(Key, Value, Pos, Acc) -> {NewValue, NewAcc}`.
    Where:
      - Key of the node (usually Pid of a process)
      - Value of the node (usually information collected by link_tree)
      - Pos - depth from the root of the tree
      - Acc - user's accumulator

    ---
    ", []);
help(map_tree) ->
    io:format("
    map_tree(Tree, Fun)
    -----------------------

    Traverses all nodes of the tree in order to modify them.
    It calls a user provided callback
    `Fun(Key, Value, Pos) -> NewValue`
    Where:
      - Key of the node (usually Pid of a process)
      - Value of the node (usually information collected by link_tree)
      - Pos - depth from the root of the tree

    ---
    ", []);
help(fold_tree) ->
    io:format("
    fold_tree(Tree, Fun)
    Traverses all nodes of the tree in order to collect some aggregated information
    about the tree. It calls a user provided callback
    `Fun(Key, Value, Pos) -> NewValue`
    Where:
      - Key of the node (usually Pid of a process)
      - Value of the node (usually information collected by link_tree)
      - Pos - depth from the root of the tree

    ---
    ", []);
help(linked_processes_info) ->
    io:format("
        linked_processes_info(Pid, Info)
        --------------------------------

        Convenience function which reduces the amount of typing compared to direct
        use of link_tree.
          - Pid: initial Pid to start from
          - Info: a list of process_info_item() as documented
            in erlang:process_info/2.

        ---
    ", []);
help(print_linked_processes) ->
    io:format("
        - print_linked_processes(Pid)
        - print_linked_processes(RegisteredName)
        - print_linked_processes(couch_index_server)

        ---------------------------

        Print cluster of linked processes. This function receives the
        initial Pid to start from. The function doesn't recurse to pids
        older than initial one.

        The output will look like similar to:

            couch_debug:print_linked_processes(whereis(couch_index_server)).
            name                                         | reductions | message_queue_len |  memory
            couch_index_server[<0.288.0>]                |   478240   |         0         |  109696
            couch_index:init/1[<0.3520.22>]              |    4899    |         0         |  109456
                couch_file:init/1[<0.886.22>]            |   11973    |         0         |  67984
                couch_index:init/1[<0.3520.22>]          |    4899    |         0         |  109456

        ---
    ", []);
help(memory_info) ->
    io:format("
        - memory_info(ProcessList)
        - memory_info(ProcessList, InfoKeys)
        - memory_info(Pid, InfoKeys)
        --------------------------------

        Obtains the values for a set of optional InfoKeys for each process in ProcessList.
          - ProcessList: List of processes
          - InfoKeys: List of desired keys to obtain values for. The supported keys are
            [binary, dictionary, heap_size, links, memory, message_queue_len, monitored_by,
            monitors, stack_size, total_heap_size]
          - Pid: Initial Pid to start from

        The output is a list containing tuples of the form {Pid, ProcessName, #{InfoKey: InfoVal}}
        for each process in ProcessList.
    ", []);
help(print_table) ->
    io:format("
        print_table(Rows, TableSpec)
        --------------------------------

        Print table of specifications.
          - Rows: List of {Id, Props} to be printed from the TableSpec
          - TableSpec: List of either {Value} or {Width, Align, Value}
            where Align is either left/center/right.

        ---
    ", []);
help(print_report) ->
    io:format("
        print_report(Report)
        --------------------------------

        Print a report in table form.
          - Report: List of {InfoKey, InfoVal} where each InfoKey is unique
          (unlike print_table/2).

        The output will look similar to:

            |info           |                                                                                               value
            |  btree_size   |                                                                                                  51
            |  def          |                                                                     function(doc){emit(doc.id, 1);}
            |  id_num       |                                                                                                   0
            |  options      |
            |  purge_seq    |                                                                                                   0
            |  reduce_funs  |
            |  update_seq   |                                                                                                   3

        ---
    ", []);
help(print_report_with_info_width) ->
    io:format("
        print_report_with_info_width(Report, Width)
        --------------------------------

        Print a report in table form. Same as print_report/1 but with a custom
        width for the InfoKey column.
          - Report: List of {InfoKey, InfoVal} where each InfoKey is unique
            (unlike print_table/2).
          - Width: Width of InfoKey column in TableSpec. Default is 50.

        ---
    ", []);
help(print_tree) ->
    io:format("
        print_tree(Tree, TableSpec)
        --------------------------------

        Print tree of specifications.
          - Tree: Tree to be printed from the TableSpec
          - TableSpec: List of either {Value} or {Width, Align, Value}
            where Align is either left/center/right.

        ---
    ", []);
help(resource_hoggers) ->
    io:format("
        resource_hoggers(MemoryInfo, InfoKey)
        --------------------------------

        Prints the top processes hogging resources along with the value associated with InfoKey.
          - MemoryInfo: Data map containing values for a set of InfoKeys
            (same structure returned by memory_info)
          - InfoKey: Desired key to obtain value for. The supported keys are
            binary, dictionary, heap_size, links, memory, message_queue_len, monitored_by,
            monitors, stack_size, and total_heap_size

        ---
    ", []);
help(resource_hoggers_snapshot) ->
    io:format("
        resource_hoggers_snapshot(MemoryInfo)
        resource_hoggers_snapshot(PreviousSnapshot)
        --------------------------------

        Prints a snapshot of the top processes hogging resources.
          - MemoryInfo: Data map containing values for a set of InfoKeys
            (same structure returned by memory_info)
          - PreviousSnapshot: Previous snapshot of resource hoggers

        An example workflow is to call `memory_info(Pids)` and pass it as a first snapshot into 
        `resource_hoggers_snapshot/1`. Then, periodically call `resource_hoggers_snapshot/1` and pass in
        the previous snapshot.

        Here is an example use case:
        ```
            S0 = couch_debug:memory_info(erlang:processes()).
            Summary = lists:foldl(fun(I, S) -> 
                timer:sleep(1000), 
                io:format(\"Snapshot ~~p/10~~n\", [I]),
                couch_debug:resource_hoggers_snapshot(S) 
            end, S0, lists:seq(1, 10)).
            couch_debug:analyze_resource_hoggers(Summary, 10).
        ```

        ---
    ", []);
help(analyze_resource_hoggers) ->
    io:format("
        analyze_resource_hoggers(Snapshot, TopN)
        --------------------------------

        Analyzes the TopN processes hogging resources along with the values associated with InfoKeys.
          - Snapshot: Snapshot of resource hoggers
          - TopN: Number of top processes to include in result

        An example workflow is to call `resource_hoggers_snapshot(memory_info(Pids))` and pass this to `analyze_resource_hoggers/2`
        along with the number of top processes to include in result, TopN. See `couch_debug:help(resource_hoggers_snapshot)` for an 
        example and more info.

        ---
    ", []);
help(dead_nodes) ->
    io:format("
        dead_nodes()
        dead_nodes(Timeout)
        --------------------------------

        Get the list of 'dead' nodes, that is node which appears in the
        mem3:nodes() list but are not connected to some nodes of the cluster.

        ---
    ", []);
help(ping) ->
    io:format("
        ping(Node)
        ping(Node, Timeout)
        --------------------------------

        Ping a node and return either a time in microseconds or an error term.

        ---
    ", []);
help(ping_live_cluster_nodes) ->
    io:format("
        ping_live_cluster_nodes()
        ping_live_cluster_nodes(Timeout)
        --------------------------------

        Ping the currently connected cluster nodes. Returns a list of
        {Node, Result} tuples or an empty list.

        ---
    ", []);
help(ping_nodes) ->
    io:format("
        ping_nodes(Nodes)
        ping_nodes(Nodes, Timeout)
        --------------------------------

        Ping the list of nodes. Return a list of {Node, Result} tuples where
        Result is either a time in microseconds or an error term.

        ---
    ", []);
help(node_events) ->
    io:format("
        node_events()
        --------------------------------

        Return the list of nodeup/nodedown events for each node in the cluster.

        ---
    ", []);
help(Unknown) ->
    io:format("Unknown function: `~p`. Please try one of the following:~n", [Unknown]),
    [io:format("    - ~s~n", [Function]) || Function <- help()],
    io:format("    ---~n", []),
    ok.

-spec busy(ProcessList :: [process()], Threshold :: pos_integer()) ->
    [Name :: process_name()].

busy(ProcessList, Threshold) when Threshold > 0 ->
    busy(ProcessList, Threshold, message_queue_len).

-spec busy(
    [process()], Threshold :: pos_integer(), Property :: busy_properties()
) ->
    [Name :: process_name()].

busy(ProcessList, Threshold, Property) when Threshold > 0 ->
    lists:filter(
        fun(Process) ->
            case (catch process_info(get_pid(Process), Property)) of
                {Property, Value} when is_integer(Value) andalso Value > Threshold ->
                    true;
                _ ->
                    false
            end
        end,
        ProcessList
    ).

-spec opened_files() ->
    [{CouchFilePid :: pid(), Fd :: pid() | tuple(), FilePath :: string()}].

opened_files() ->
    lists:filtermap(
        fun(Pid) ->
            case couch_file:process_info(Pid) of
                {Fd, FilePath} -> {true, {Pid, Fd, FilePath}};
                undefined -> false
            end
        end,
        processes()
    ).

-spec opened_files_by_regexp(FileRegExp :: iodata()) ->
    [{CouchFilePid :: pid(), Fd :: pid() | tuple(), FilePath :: string()}].
opened_files_by_regexp(FileRegExp) ->
    {ok, RegExp} = re:compile(FileRegExp),
    lists:filter(
        fun({_Pid, _Fd, Path}) ->
            re:run(Path, RegExp) =/= nomatch
        end,
        couch_debug:opened_files()
    ).

-spec opened_files_contains(FileNameFragment :: iodata()) ->
    [{CouchFilePid :: pid(), Fd :: pid() | tuple(), FilePath :: string()}].
opened_files_contains(FileNameFragment) ->
    lists:filter(
        fun({_Pid, _Fd, Path}) ->
            string:str(Path, FileNameFragment) > 0
        end,
        couch_debug:opened_files()
    ).

process_name(Pid) when is_pid(Pid) ->
    Info = process_info(Pid, [registered_name, dictionary, initial_call]),
    case Info of
        undefined ->
            iolist_to_list(io_lib:format("[~p]", [Pid]));
        [{registered_name, Name} | _] when Name =/= [] ->
            iolist_to_list(io_lib:format("~s[~p]", [Name, Pid]));
        [_, {dictionary, Dict}, {initial_call, MFA}] ->
            {M, F, A} = proplists:get_value('$initial_call', Dict, MFA),
            iolist_to_list(io_lib:format("~p:~p/~p[~p]", [M, F, A, Pid]))
    end;
process_name(Else) ->
    iolist_to_list(io_lib:format("~p", [Else])).

get_pid(Process) when is_pid(Process) ->
    Process;
get_pid(Process) ->
    whereis(Process).

iolist_to_list(List) ->
    binary_to_list(iolist_to_binary(List)).

link_tree(RootPid) ->
    link_tree(RootPid, []).

link_tree(RootPid, Info) ->
    link_tree(RootPid, Info, fun(_, Props) -> Props end).

link_tree(RootPid, Info, Fun) ->
    {_, Result} = link_tree(
        RootPid, [links | Info], gb_trees:empty(), 0, [RootPid], Fun
    ),
    Result.

link_tree(RootPid, Info, Visited0, Pos, [Pid | Rest], Fun) ->
    case gb_trees:lookup(Pid, Visited0) of
        {value, Props} ->
            {Visited0, [{Pos, {Pid, Fun(Pid, Props), []}}]};
        none when RootPid =< Pid ->
            Props = info(Pid, Info),
            Visited1 = gb_trees:insert(Pid, Props, Visited0),
            {links, Children} = lists:keyfind(links, 1, Props),
            {Visited2, NewTree} = link_tree(
                RootPid, Info, Visited1, Pos + 1, Children, Fun
            ),
            {Visited3, Result} = link_tree(
                RootPid, Info, Visited2, Pos, Rest, Fun
            ),
            {Visited3, [{Pos, {Pid, Fun(Pid, Props), NewTree}}] ++ Result};
        none ->
            Props = info(Pid, Info),
            Visited1 = gb_trees:insert(Pid, Props, Visited0),
            {Visited2, Result} = link_tree(
                RootPid, Info, Visited1, Pos, Rest, Fun
            ),
            {Visited2, [{Pos, {Pid, Fun(Pid, Props), []}}] ++ Result}
    end;
link_tree(_RootPid, _Info, Visited, _Pos, [], _Fun) ->
    {Visited, []}.

info(Pid, Info) when is_pid(Pid) ->
    ValidProps = [
        backtrace,
        binary,
        catchlevel,
        current_function,
        current_location,
        current_stacktrace,
        dictionary,
        error_handler,
        garbage_collection,
        garbage_collection_info,
        group_leader,
        heap_size,
        initial_call,
        links,
        last_calls,
        memory,
        message_queue_len,
        messages,
        min_heap_size,
        min_bin_vheap_size,
        monitored_by,
        monitors,
        message_queue_data,
        priority,
        reductions,
        registered_name,
        sequential_trace_token,
        stack_size,
        status,
        suspending,
        total_heap_size,
        trace,
        trap_exit
    ],
    Validated = lists:filter(fun(P) -> lists:member(P, ValidProps) end, Info),
    process_info(Pid, lists:usort(Validated));
info(Port, Info) when is_port(Port) ->
    ValidProps = [
        registered_name,
        id,
        connected,
        links,
        name,
        input,
        output,
        os_pid
    ],
    Validated = lists:filter(fun(P) -> lists:member(P, ValidProps) end, Info),
    port_info(Port, lists:usort(Validated)).

port_info(Port, Items) ->
    lists:foldl(
        fun(Item, Acc) ->
            case (catch erlang:port_info(Port, Item)) of
                {Item, _Value} = Info -> [Info | Acc];
                _Else -> Acc
            end
        end,
        [],
        Items
    ).

mapfold_tree([], Acc, _Fun) ->
    {[], Acc};
mapfold_tree([{Pos, {Key, Value0, SubTree0}} | Rest0], Acc0, Fun) ->
    {Value1, Acc1} = Fun(Key, Value0, Pos, Acc0),
    {SubTree1, Acc2} = mapfold_tree(SubTree0, Acc1, Fun),
    {Rest1, Acc3} = mapfold_tree(Rest0, Acc2, Fun),
    {[{Pos, {Key, Value1, SubTree1}} | Rest1], Acc3}.

map_tree(Tree, Fun) ->
    {Result, _} = mapfold_tree(Tree, nil, fun(Key, Value, Pos, Acc) ->
        {Fun(Key, Value, Pos), Acc}
    end),
    Result.

fold_tree(Tree, Acc, Fun) ->
    {_, Result} = mapfold_tree(Tree, Acc, fun(Key, Value, Pos, AccIn) ->
        {Value, Fun(Key, Value, Pos, AccIn)}
    end),
    Result.

linked_processes_info(Pid, Info) ->
    link_tree(Pid, Info, fun(P, Props) -> {process_name(P), Props} end).

print_linked_processes(couch_index_server) ->
    print_couch_index_server_processes();
print_linked_processes(Name) when is_atom(Name) ->
    case whereis(Name) of
        undefined -> {error, {unknown, Name}};
        Pid -> print_linked_processes(Pid)
    end;
print_linked_processes(Pid) when is_pid(Pid) ->
    Info = [reductions, message_queue_len, memory],
    TableSpec = [
        {50, left, name},
        {12, centre, reductions},
        {19, centre, message_queue_len},
        {10, centre, memory}
    ],
    Tree = linked_processes_info(Pid, Info),
    print_tree(Tree, TableSpec).

memory_info(ProcessList) ->
    InfoKeys = [
        binary,
        dictionary,
        heap_size,
        links,
        memory,
        message_queue_len,
        monitored_by,
        monitors,
        stack_size,
        total_heap_size
    ],
    memory_info(ProcessList, InfoKeys).

memory_info(ProcessList, InfoKeys) when is_list(ProcessList) ->
    lists:map(
        fun(Process) ->
            memory_info(Process, InfoKeys)
        end,
        ProcessList
    );
memory_info(Pid, InfoKeys) ->
    case process_info(Pid, InfoKeys) of
        undefined ->
            {Pid, undefined, undefined};
        Values ->
            DataMap = maps:from_list(
                lists:map(
                    fun({K, _} = I) ->
                        {K, info_size(I)}
                    end,
                    Values
                )
            ),
            {Pid, process_name(Pid), DataMap}
    end.

info_size(InfoKV) ->
    case InfoKV of
        {monitors, L} -> length(L);
        {monitored_by, L} -> length(L);
        {links, L} -> length(L);
        {dictionary, L} -> length(L);
        {binary, BinInfos} -> lists:sum([S || {_, S, _} <- BinInfos]);
        {_, V} -> V
    end.

resource_hoggers(MemoryInfo, InfoKey) ->
    KeyFun = fun
        ({_Pid, _Id, undefined}) -> undefined;
        ({_Pid, Id, DataMap}) -> {Id, [{InfoKey, maps:get(InfoKey, DataMap)}]}
    end,
    resource_hoggers(MemoryInfo, InfoKey, KeyFun).

resource_hoggers(MemoryInfo, InfoKey, KeyFun) ->
    HoggersData = resource_hoggers_data(MemoryInfo, InfoKey, KeyFun),
    TableSpec = [
        {50, centre, id},
        {20, centre, InfoKey}
    ],
    print_table(HoggersData, TableSpec).

resource_hoggers_data(MemoryInfo, InfoKey, KeyFun) when is_atom(InfoKey) ->
    resource_hoggers_data(MemoryInfo, InfoKey, KeyFun, 20).

resource_hoggers_data(MemoryInfo, InfoKey, KeyFun, N) when is_atom(InfoKey) and is_integer(N) ->
    SortedTuples = resource_hoggers_data(MemoryInfo, InfoKey, KeyFun, undefined),
    {TopN, _} = lists:split(N, SortedTuples),
    TopN;
resource_hoggers_data(MemoryInfo, InfoKey, KeyFun, undefined) when is_atom(InfoKey) ->
    Tuples = lists:filtermap(
        fun(Tuple) ->
            case KeyFun(Tuple) of
                undefined ->
                    false;
                Value ->
                    {true, Value}
            end
        end,
        MemoryInfo
    ),
    lists:reverse(
        lists:sort(
            fun({_, A}, {_, B}) ->
                lists:keyfind(InfoKey, 1, A) < lists:keyfind(InfoKey, 1, B)
            end,
            Tuples
        )
    ).

resource_hoggers_snapshot({N, MemoryInfo, InfoKeys} = _Snapshot) ->
    Data = lists:filtermap(
        fun({Pid, Id, Data}) ->
            case memory_info(Pid, InfoKeys) of
                {Pid, undefined, undefined} ->
                    false;
                {_, _, DataMap} ->
                    {true, {Pid, Id, update_delta(Data, DataMap)}}
            end
        end,
        MemoryInfo
    ),
    {N + 1, Data, InfoKeys};
resource_hoggers_snapshot([]) ->
    [];
resource_hoggers_snapshot([{_Pid, _Id, Data} | _] = MemoryInfo) ->
    resource_hoggers_snapshot({0, MemoryInfo, maps:keys(Data)}).

update_delta({_, InitialDataMap}, DataMap) ->
    update_delta(InitialDataMap, DataMap);
update_delta(InitialDataMap, DataMap) ->
    Delta = maps:fold(
        fun(Key, Value, AccIn) ->
            maps:put(Key, maps:get(Key, DataMap, Value) - Value, AccIn)
        end,
        maps:new(),
        InitialDataMap
    ),
    {Delta, InitialDataMap}.

analyze_resource_hoggers({N, Data, InfoKeys}, TopN) ->
    io:format("Number of snapshots: ~p~n", [N]),
    lists:map(
        fun(InfoKey) ->
            KeyFun = fun
                ({_Pid, _Id, undefined}) ->
                    undefined;
                ({_Pid, Id, {Delta, DataMap}}) ->
                    {Id, [
                        {InfoKey, maps:get(InfoKey, DataMap)},
                        {delta, maps:get(InfoKey, Delta)}
                    ]}
            end,
            io:format("Top ~p by change in ~p~n", [TopN, InfoKey]),
            HoggersData = resource_hoggers_data(Data, delta, KeyFun, TopN),
            TableSpec = [
                {50, centre, id},
                {20, right, InfoKey},
                {20, right, delta}
            ],
            print_table(HoggersData, TableSpec)
        end,
        InfoKeys
    ).

id("couch_file:init" ++ _, Pid, _Props) ->
    case couch_file:process_info(Pid) of
        {{file_descriptor, prim_file, {Port, Fd}}, FilePath} ->
            term2str([
                term2str(Fd),
                ":",
                term2str(Port),
                ":",
                shorten_path(FilePath)
            ]);
        undefined ->
            ""
    end;
id(_IdStr, _Pid, _Props) ->
    "".

print_couch_index_server_processes() ->
    Info = [reductions, message_queue_len, memory],
    Trees = lists:map(
        fun(Name) ->
            link_tree(whereis(Name), Info, fun(P, Props) ->
                IdStr = process_name(P),
                {IdStr, [{id, id(IdStr, P, Props)} | Props]}
            end)
        end,
        couch_index_server:names()
    ),
    TableSpec = [
        {50, left, name},
        {12, centre, reductions},
        {19, centre, message_queue_len},
        {14, centre, memory},
        {id}
    ],
    print_trees(Trees, TableSpec).

shorten_path(Path) ->
    ViewDir = list_to_binary(config:get("couchdb", "view_index_dir")),
    DatabaseDir = list_to_binary(config:get("couchdb", "database_dir")),
    File = list_to_binary(Path),
    Len = max(
        binary:longest_common_prefix([File, DatabaseDir]),
        binary:longest_common_prefix([File, ViewDir])
    ),
    <<_:Len/binary, Rest/binary>> = File,
    binary_to_list(Rest).

-spec restart(Name :: process_name()) ->
    Pid :: pid() | timeout.

restart(Name) ->
    Res = test_util:with_process_restart(Name, fun() ->
        exit(whereis(Name), kill)
    end),
    case Res of
        {Pid, true} ->
            Pid;
        timeout ->
            timeout
    end.

-spec restart_busy(ProcessList :: [process_name()], Threshold :: pos_integer()) ->
    throw({timeout, Name :: process_name()}).

restart_busy(ProcessList, Threshold) ->
    restart_busy(ProcessList, Threshold, 1000).

-spec restart_busy(
    ProcessList :: [process_name()], Threshold :: pos_integer(), DelayInMsec :: pos_integer()
) ->
    throw({timeout, Name :: process_name()}) | ok.

restart_busy(ProcessList, Threshold, DelayInMsec) ->
    restart_busy(ProcessList, Threshold, DelayInMsec, message_queue_len).

-spec restart_busy(
    ProcessList :: [process_name()],
    Threshold :: pos_integer(),
    DelayInMsec :: pos_integer(),
    Property :: busy_properties()
) ->
    throw({timeout, Name :: process_name()}) | ok.

restart_busy(ProcessList, Threshold, DelayInMsec, Property) when
    Threshold > 0 andalso DelayInMsec > 0
->
    lists:foreach(
        fun(Name) ->
            case restart(Name) of
                timeout ->
                    throw({timeout, Name});
                _ ->
                    timer:sleep(DelayInMsec)
            end
        end,
        busy(ProcessList, Threshold, Property)
    ).

dead_nodes() ->
    mem3:dead_nodes().

dead_nodes(Timeout) ->
    mem3:dead_nodes(Timeout).

ping(Node) ->
    mem3:ping(Node).

ping(Node, Timeout) ->
    mem3:ping(Node, Timeout).

ping_live_cluster_nodes() ->
    mem3:ping_nodes().

ping_live_cluster_nodes(Timeout) ->
    mem3:ping_nodes(Timeout).

ping_nodes(Nodes) ->
    mem3:ping_nodes(Nodes, ?PING_TIMEOUT_IN_MS).

ping_nodes(Nodes, Timeout) ->
    mem3:ping_nodes(Nodes, Timeout).

node_events() ->
    mem3_distribution:events().

%% Pretty print functions

%% Limitations:
%%   - The first column has to be specified as {Width, left, Something}
%% The TableSpec is a list of either:
%%   - {Value}
%%   - {Width, Align, Value}
%% Align is one of the following:
%%  - left
%%  - centre
%%  - right
print_table(Rows, TableSpec) ->
    io:format("~s~n", [format(TableSpec)]),
    lists:foreach(
        fun({Id, Props}) ->
            io:format("~s~n", [table_row(Id, 2, Props, TableSpec)])
        end,
        Rows
    ),
    io:format("~n", []),
    ok.

print_report(Report) ->
    print_report_with_info_width(Report, 50).

print_report_with_info_width(Report, Width) ->
    TableSpec = [
        {Width, left, info},
        {100, right, value}
    ],
    io:format("~s~n", [format(TableSpec)]),
    lists:map(
        fun({InfoKey, Value}) ->
            TableSpec1 = [
                {Width, left, info},
                {100, right, InfoKey}
            ],
            io:format("~s~n", [table_row(InfoKey, 2, [{InfoKey, Value}], TableSpec1)])
        end,
        Report
    ).

print_tree(Tree, TableSpec) ->
    io:format("~s~n", [format(TableSpec)]),
    map_tree(Tree, fun(_, {Id, Props}, Pos) ->
        io:format("~s~n", [table_row(Id, Pos * 2, Props, TableSpec)])
    end),
    ok.

print_trees(Trees, TableSpec) ->
    io:format("~s~n", [format(TableSpec)]),
    io:format("~s~n", [separator(TableSpec)]),
    lists:foreach(
        fun(Tree) ->
            map_tree(Tree, fun(_, {Id, Props}, Pos) ->
                io:format("~s~n", [table_row(Id, Pos * 2, Props, TableSpec)])
            end),
            io:format("~s~n", [space(TableSpec)])
        end,
        Trees
    ),
    ok.

format(Spec) ->
    Fields = [format_value(Format) || Format <- Spec],
    [$| | string:join(Fields, "|")].

fill(Spec, [Char]) ->
    fill(Spec, Char);
fill(Spec, Char) when is_integer(Char) ->
    Fields = [format_value(Format) || Format <- Spec],
    Sizes = [length(F) || F <- Fields],
    [$| | [string:join([string:chars(Char, F) || F <- Sizes], "|")]].

space(Spec) ->
    fill(Spec, " ").

separator(Spec) ->
    fill(Spec, "-").

format_value({Value}) -> term2str(Value);
format_value({Width, Align, Value}) -> string:Align(term2str(Value), Width).

bind_value({K}, Props) when is_list(Props) ->
    {element(2, lists:keyfind(K, 1, Props))};
bind_value({Width, Align, K}, Props) when is_list(Props) ->
    {Width, Align, element(2, lists:keyfind(K, 1, Props))}.

term2str(Atom) when is_atom(Atom) -> atom_to_list(Atom);
term2str(Binary) when is_binary(Binary) -> binary_to_list(Binary);
term2str(Integer) when is_integer(Integer) -> integer_to_list(Integer);
term2str(Float) when is_float(Float) -> float_to_list(Float);
term2str(String) when is_list(String) -> lists:flatten(String);
term2str(Term) -> iolist_to_list(io_lib:format("~p", [Term])).

table_row(Key, Indent, Props, [{KeyWidth, Align, _} | Spec]) ->
    Values = [bind_value(Format, Props) || Format <- Spec],
    KeyStr = string:Align(term2str(Key), KeyWidth - Indent),
    [$|, string:copies(" ", Indent), KeyStr | format(Values)].

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

random_processes(Depth) ->
    random_processes([], Depth).

random_processes(Pids, 0) ->
    lists:usort(Pids);
random_processes(Acc, Depth) ->
    Caller = self(),
    Ref = make_ref(),
    Pid =
        case oneof([spawn_link, open_port]) of
            spawn_monitor ->
                {P, _} = spawn_monitor(fun() ->
                    Caller ! {Ref, random_processes(Depth - 1)},
                    receive
                        looper -> ok
                    end
                end),
                P;
            spawn ->
                spawn(fun() ->
                    Caller ! {Ref, random_processes(Depth - 1)},
                    receive
                        looper -> ok
                    end
                end);
            spawn_link ->
                spawn_link(fun() ->
                    Caller ! {Ref, random_processes(Depth - 1)},
                    receive
                        looper -> ok
                    end
                end);
            open_port ->
                spawn_link(fun() ->
                    Port = erlang:open_port({spawn, "sleep 10"}, [hide]),
                    true = erlang:link(Port),
                    Caller ! {Ref, random_processes(Depth - 1)},
                    receive
                        looper -> ok
                    end
                end)
        end,
    receive
        {Ref, Pids} -> random_processes([Pid | Pids] ++ Acc, Depth - 1)
    end.

oneof(Options) ->
    lists:nth(rand:uniform(length(Options)), Options).

tree() ->
    [InitialPid | _] = Processes = random_processes(5),
    {InitialPid, Processes, link_tree(InitialPid)}.

setup() ->
    tree().

teardown({_InitialPid, Processes, _Tree}) ->
    [
        begin
            (catch unlink(Pid)),
            exit(Pid, kill)
        end
     || Pid <- Processes
    ].

link_tree_test_() ->
    {
        "link_tree tests",
        {
            foreach,
            fun setup/0,
            fun teardown/1,
            [
                fun should_have_same_shape/1,
                fun should_include_extra_info/1
            ]
        }
    }.

should_have_same_shape({InitialPid, _Processes, Tree}) ->
    ?_test(begin
        InfoTree = linked_processes_info(InitialPid, []),
        ?assert(is_equal(InfoTree, Tree)),
        ok
    end).

should_include_extra_info({InitialPid, _Processes, _Tree}) ->
    Info = [reductions, message_queue_len, memory],
    ?_test(begin
        InfoTree = linked_processes_info(InitialPid, Info),
        map_tree(InfoTree, fun(Key, {_Id, Props}, _Pos) ->
            case Key of
                Pid when is_pid(Pid) ->
                    ?assert(lists:keymember(reductions, 1, Props)),
                    ?assert(lists:keymember(message_queue_len, 1, Props)),
                    ?assert(lists:keymember(memory, 1, Props));
                _Port ->
                    ok
            end,
            Props
        end),
        ok
    end).

is_equal([], []) ->
    true;
is_equal([{Pos, {Pid, _, A}} | RestA], [{Pos, {Pid, _, B}} | RestB]) ->
    case is_equal(RestA, RestB) of
        false -> false;
        true -> is_equal(A, B)
    end.

-endif.
