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

-module(ken_server_test).

-include_lib("eunit/include/eunit.hrl").

%% hardcoded defaults: limit: 20; batch: 1; delay: 5000; prune: 60000
default_test_() ->
    {inorder,
        {setup, fun setup_default/0, fun teardown/1, [
            set_builder("returns default", set_limit, 12, 20),
            set_builder("keeps set", set_limit, 6, 12),
            set_builder("returns default", set_batch_size, 3, 1),
            set_builder("keeps set", set_batch_size, 6, 3),
            set_builder("returns default", set_delay, 7000, 5000),
            set_builder("keeps set", set_delay, 10000, 7000),
            set_builder("returns default", set_prune_interval, 70000, 60000),
            set_builder("keeps set", set_prune_interval, 80000, 70000)
        ]}}.

exception_test_() ->
    {inorder,
        {foreach, fun setup_default/0, fun teardown/1, [
            exception_builder("exception on zero", set_limit, 0),
            exception_builder("exception on negative", set_limit, -12),
            exception_builder("exception on zero", set_batch_size, 0),
            exception_builder("exception on negative", set_batch_size, -12),
            set_builder("no exception on zero", set_delay, 0, 5000),
            exception_builder("exception on negative", set_delay, -12),
            exception_builder("exception on zero", set_prune_interval, 0),
            exception_builder("exception on negative", set_prune_interval, -12)
        ]}}.

config_test_() ->
    {inorder,
        {setup, fun setup_config/0, fun teardown/1, [
            set_builder("reads config", set_limit, 24, 42),
            set_builder("keeps set", set_limit, 6, 24)
        ]}}.

setup_default() ->
    {ok, EventPid} = start_server(couch_event_server),
    {ok, CfgPid} = start_server(config),
    {ok, KenPid} = start_server(ken_server),
    [{ken_pid, KenPid}, {cfg_pid, CfgPid}, {event_pid, EventPid}].

setup_config() ->
    {ok, Pwd} = file:get_cwd(),
    Config = filename:join([Pwd, "..", "test", "config.ini"]),
    {ok, EventPid} = start_server(couch_event_server),
    {ok, CfgPid} = start_server(config, [[Config]]),
    {ok, KenPid} = start_server(ken_server),
    [{ken_pid, KenPid}, {cfg_pid, CfgPid}, {event_pid, EventPid}].

teardown(Cfg) ->
    ok = stop_server(event_pid, Cfg),
    ok = stop_server(cfg_pid, Cfg),
    ok = stop_server(ken_pid, Cfg).

exception_builder(Desc, F, Val) ->
    D = atom_to_list(F) ++ " " ++ Desc,
    {D, ?_assertException(error, function_clause, ken_server:F(Val))}.

set_builder(Desc, F, In, Out) ->
    D = atom_to_list(F) ++ " " ++ Desc,
    {D, ?_assertEqual(Out, ken_server:F(In))}.

start_server(Module) ->
    start_server(Module, []).

start_server(Module, Config) ->
    gen_server:start({local, Module}, Module, Config, []).

stop_server(Key, Cfg) ->
    {Key, Pid} = lists:keyfind(Key, 1, Cfg),
    MRef = erlang:monitor(process, Pid),
    true = exit(Pid, kill),
    receive
        {'DOWN', MRef, _, _, _} -> ok
    end.
