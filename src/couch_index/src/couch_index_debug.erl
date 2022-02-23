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

-module(couch_index_debug).

-export([
    help/0,
    help/1
]).

-export([
    names/0,
    print_linked_processes/0,
    busy/1,
    busy/2,
    restart_busy/1,
    restart_busy/2,
    restart_busy/3
]).

-type throw(_Reason) :: no_return().

-type process_name() :: atom().
-type function_name() :: atom().

help() ->
    [
        %% list of provided commands
        names,
        print_linked_processes,
        busy,
        restart_busy
    ].

-spec help(Function :: function_name()) -> ok.
%% erlfmt-ignore
help(names) ->
    io:format("
    names()
    --------------

    Returns list of named processes which constitutes
    a sharded couch_index_server
    ---
    ", []);
help(print_linked_processes) ->
    io:format("
    print_linked_processes()
    --------------

    Print cluster of linked processes. The output would look like similar to:

    |name                                              | reductions | message_queue_len |    memory    |id
    |--------------------------------------------------|------------|-------------------|--------------|--
    |index_server_1[<0.320.0>]                         |    1115    |         0         |    17000     |
    |  couch_secondary_services[<0.312.0>]             |   93258    |         0         |    68600     |
    |  couch_event_listener:do_init/3[<0.323.0>]       |    195     |         0         |     2856     |
    |    index_server_1[<0.320.0>]                     |    1115    |         0         |    17000     |
    |                                                  |            |                   |              |
    |index_server_2[<0.324.0>]                         |    278     |         0         |     6088     |
    |  couch_secondary_services[<0.312.0>]             |   93260    |         0         |    68600     |
    |  couch_event_listener:do_init/3[<0.326.0>]       |    161     |         0         |     2856     |
    |    index_server_2[<0.324.0>]                     |    278     |         0         |     6088     |
    ---
    ", []);
help(busy) ->
    io:format("
    busy(Thereshold)
    busy(Thereshold, Property)
    --------------

    Finds list of couch_index_server processes and returns the ones with
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
help(restart_busy) ->
    io:format("
    restart_busy(Thereshold)
    restart_busy(Thereshold, DelayInMsec)
    restart_busy(Thereshold, DelayInMsec, Property)
    --------------

    Finds list of couch_index_server processes and returns the ones with
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
    The function doesn't proceed to next server until the replacement server
    process starts.
    ---
    ", []);
help(Unknown) ->
    io:format("Unknown function: `~p`. Please try one of the following:~n", [Unknown]),
    [io:format("    - ~s~n", [Function]) || Function <- help()],
    io:format("    ---~n", []),
    ok.

-spec names() -> [process_name()].

names() ->
    couch_index_server:names().

-spec print_linked_processes() -> ok.

print_linked_processes() ->
    couch_debug:print_linked_processes(couch_index_server).

-spec busy(Thershold :: pos_integer()) ->
    [Name :: process_name()].

busy(Threshold) when Threshold > 0 ->
    couch_debug:busy(names(), Threshold).

-spec busy(Thershold :: pos_integer(), Property :: couch_debug:busy_properties()) ->
    [Name :: process_name()].

busy(Threshold, Property) when Threshold > 0 ->
    couch_debug:busy(names(), Threshold, Property).

-spec restart_busy(Threshold :: pos_integer()) ->
    throw({timeout, Name :: process_name()}).

restart_busy(Threshold) ->
    couch_debug:restart_busy(names(), Threshold, 1000).

-spec restart_busy(Thershold :: pos_integer(), DelayInMsec :: pos_integer()) ->
    throw({timeout, Name :: process_name()}).

restart_busy(Threshold, DelayInMsec) ->
    couch_debug:restart_busy(names(), Threshold, DelayInMsec).

-spec restart_busy(
    Thershold :: pos_integer(),
    DelayInMsec :: pos_integer(),
    Property :: couch_debug:busy_properties()
) ->
    throw({timeout, Name :: process_name()}).

restart_busy(Threshold, DelayInMsec, Property) ->
    couch_debug:restart_busy(names(), Threshold, DelayInMsec, Property).
