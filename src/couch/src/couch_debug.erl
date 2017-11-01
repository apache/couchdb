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
    link_tree/1,
    link_tree/2,
    mapfold_tree/3,
    map_tree/2,
    fold_tree/3,
    linked_processes_info/2,
    print_linked_processes/1
]).

help() ->
    [
        opened_files,
        opened_files_by_regexp,
        opened_files_contains,
        process_name,
        link_tree,
        mapfold,
        map,
        fold,
        linked_processes_info,
        print_linked_processes
    ].

-spec help(Function :: atom()) -> ok.
help(opened_files) ->
    io:format("
    opened_files()
    --------------

    Returns list of currently opened files
    It iterates through `erlang:ports` and filters out all ports which are not efile.
    It uses `process_info(Pid, dictionary)` to get info about couch_file properties.
    ---
    ", []);
help(opened_files_by_regexp) ->
    io:format("
    opened_files_by_regexp(FileRegExp)
    ----------------------------------

    Returns list of currently opened files which name match the provided regular expression.
    It iterates through `erlang:ports()` and filter out all ports which are not efile.
    It uses `process_info(Pid, dictionary)` to get info about couch_file properties.
    ---
    ", []);
help(opened_files_contains) ->
    io:format("
    opened_files_contains(SubString)
    --------------------------------

    Returns list of currently opened files whose names contain the provided SubString.
    It iterates through `erlang:ports()` and filters out all ports which are not efile.
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
      - Key of the node (usualy Pid of a process)
      - Value of the node (usualy information collected by link_tree)
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
      - Key of the node (usualy Pid of a process)
      - Value of the node (usualy information collected by link_tree)
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
      - Key of the node (usualy Pid of a process)
      - Value of the node (usualy information collected by link_tree)
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
        older than initial one. The output would look like similar to:
        ```
couch_debug:print_linked_processes(whereis(couch_index_server)).
name                                         | reductions | message_queue_len |  memory
couch_index_server[<0.288.0>]                |   478240   |         0         |  109696
  couch_index:init/1[<0.3520.22>]            |    4899    |         0         |  109456
    couch_file:init/1[<0.886.22>]            |   11973    |         0         |  67984
      couch_index:init/1[<0.3520.22>]        |    4899    |         0         |  109456
        ```

        ---
    ", []);
help(Unknown) ->
    io:format("Unknown function: `~p`. Please try one of the following:~n", [Unknown]),
    [io:format("    - ~s~n", [Function]) || Function <- help()],
    io:format("    ---~n", []),
    ok.

-spec opened_files() ->
    [{port(), CouchFilePid :: pid(), Fd :: pid() | tuple(), FilePath :: string()}].

opened_files() ->
    Info = [couch_file_port_info(Port)
        || Port <- erlang:ports(),
            {name, "efile"} =:= erlang:port_info(Port, name)],
    [I || I <- Info, is_tuple(I)].

couch_file_port_info(Port) ->
    {connected, Pid} = erlang:port_info(Port, connected),
    case couch_file:process_info(Pid) of
        {Fd, FilePath} ->
            {Port, Pid, Fd, FilePath};
        undefined ->
            undefined
    end.

-spec opened_files_by_regexp(FileRegExp :: iodata()) ->
    [{port(), CouchFilePid :: pid(), Fd :: pid() | tuple(), FilePath :: string()}].
opened_files_by_regexp(FileRegExp) ->
    {ok, RegExp} = re:compile(FileRegExp),
    lists:filter(fun({_Port, _Pid, _Fd, Path}) ->
        re:run(Path, RegExp) =/= nomatch
    end, couch_debug:opened_files()).

-spec opened_files_contains(FileNameFragment :: iodata()) ->
    [{port(), CouchFilePid :: pid(), Fd :: pid() | tuple(), FilePath :: string()}].
opened_files_contains(FileNameFragment) ->
    lists:filter(fun({_Port, _Pid, _Fd, Path}) ->
        string:str(Path, FileNameFragment) > 0
    end, couch_debug:opened_files()).

process_name(Pid) when is_pid(Pid) ->
    case process_info(Pid, registered_name) of
        {registered_name, Name} ->
            iolist_to_list(io_lib:format("~s[~p]", [Name, Pid]));
        _ ->
            {dictionary, Dict} = process_info(Pid, dictionary),
            case proplists:get_value('$initial_call', Dict) of
                undefined ->
                    {initial_call, {M, F, A}} = process_info(Pid, initial_call),
                    iolist_to_list(io_lib:format("~p:~p/~p[~p]", [M, F, A, Pid]));
                {M, F, A} ->
                    iolist_to_list(io_lib:format("~p:~p/~p[~p]", [M, F, A, Pid]))
            end
    end;
process_name(Else) ->
    iolist_to_list(io_lib:format("~p", [Else])).

iolist_to_list(List) ->
    binary_to_list(iolist_to_binary(List)).

link_tree(RootPid) ->
    link_tree(RootPid, []).

link_tree(RootPid, Info) ->
    link_tree(RootPid, Info, fun(_, Props) -> Props end).

link_tree(RootPid, Info, Fun) ->
    {_, Result} = link_tree(
        RootPid, [links | Info], gb_trees:empty(), 0, [RootPid], Fun),
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
                RootPid, Info, Visited1, Pos + 1, Children, Fun),
            {Visited3, Result} = link_tree(
                RootPid, Info, Visited2, Pos, Rest, Fun),
            {Visited3, [{Pos, {Pid, Fun(Pid, Props), NewTree}}]  ++ Result};
        none ->
            Props = info(Pid, Info),
            Visited1 = gb_trees:insert(Pid, Props, Visited0),
            {Visited2, Result} = link_tree(
                RootPid, Info, Visited1, Pos, Rest, Fun),
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
    lists:foldl(fun(Item, Acc) ->
        case (catch erlang:port_info(Port, Item)) of
            {Item, _Value} = Info -> [Info | Acc];
            _Else -> Acc
        end
    end, [], Items).

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
        {50, left, name}, {12, centre, reductions},
        {19, centre, message_queue_len}, {10, centre, memory}
    ],
    Tree = linked_processes_info(Pid, Info),
    print_tree(Tree, TableSpec).

id("couch_file:init" ++ _, Pid, _Props) ->
    case couch_file:process_info(Pid) of
        {{file_descriptor, prim_file, {Port, Fd}}, FilePath} ->
            term2str([
                term2str(Fd), ":",
                term2str(Port), ":",
                shorten_path(FilePath)]);
        undefined ->
            ""
    end;
id(_IdStr, _Pid, _Props) ->
    "".

print_couch_index_server_processes() ->
    Info = [reductions, message_queue_len, memory],
    TableSpec = [
        {50, left, name}, {12, centre, reductions},
        {19, centre, message_queue_len}, {14, centre, memory}, {id}
    ],

    Tree = link_tree(whereis(couch_index_server), Info, fun(P, Props) ->
        IdStr = process_name(P),
        {IdStr, [{id, id(IdStr, P, Props)} | Props]}
    end),
    print_tree(Tree, TableSpec).

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

%% Pretty print functions

%% Limmitations:
%%   - The first column has to be specified as {Width, left, Something}
%% The TableSpec is a list of either:
%%   - {Value}
%%   - {Width, Align, Value}
%% Align is one of the following:
%%  - left
%%  - centre
%%  - right
print_tree(Tree, TableSpec) ->
    io:format("~s~n", [format(TableSpec)]),
    map_tree(Tree, fun(_, {Id, Props}, Pos) ->
        io:format("~s~n", [table_row(Id, Pos * 2, Props, TableSpec)])
    end),
    ok.

format(Spec) ->
    Fields = [format_value(Format) || Format <- Spec],
    string:join(Fields, "|").

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
    [string:copies(" ", Indent), KeyStr, "|" | format(Values)].

-ifdef(TEST).
-include_lib("couch/include/couch_eunit.hrl").

random_processes(Depth) ->
    random_processes([], Depth).

random_processes(Pids, 0) ->
    lists:usort(Pids);
random_processes(Acc, Depth) ->
    Caller = self(),
    Ref = make_ref(),
    Pid = case oneof([spawn_link, open_port]) of
        spawn_monitor ->
            {P, _} = spawn_monitor(fun() ->
                Caller ! {Ref, random_processes(Depth - 1)},
                receive looper -> ok end
            end),
            P;
        spawn ->
            spawn(fun() ->
                Caller ! {Ref, random_processes(Depth - 1)},
                receive looper -> ok end
            end);
        spawn_link ->
            spawn_link(fun() ->
                Caller ! {Ref, random_processes(Depth - 1)},
                receive looper -> ok end
            end);
        open_port ->
            spawn_link(fun() ->
                Port = erlang:open_port({spawn, "sleep 10"}, []),
                true = erlang:link(Port),
                Caller ! {Ref, random_processes(Depth - 1)},
                receive looper -> ok end
            end)
    end,
    receive
        {Ref, Pids} -> random_processes([Pid | Pids] ++ Acc, Depth - 1)
    end.

oneof(Options) ->
    lists:nth(couch_rand:uniform(length(Options)), Options).


tree() ->
    [InitialPid | _] = Processes = random_processes(5),
    {InitialPid, Processes, link_tree(InitialPid)}.

setup() ->
    tree().

teardown({_InitialPid, Processes, _Tree}) ->
    [exit(Pid, normal) || Pid <- Processes].

link_tree_test_() ->
    {
        "link_tree tests",
        {
            foreach,
            fun setup/0, fun teardown/1,
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
                Port ->
                    ok
            end,
            Props
         end),
         ok
    end).

is_equal([], []) -> true;
is_equal([{Pos, {Pid, _, A}} | RestA], [{Pos, {Pid, _, B}} | RestB]) ->
    case is_equal(RestA, RestB) of
        false -> false;
        true -> is_equal(A, B)
    end.

-endif.
