%% -------------------------------------------------------------------
%%
%% riaknostic - automated diagnostic tools for Riak
%%
%% Copyright (c) 2011 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc Diagnostic that detects the existence of Erlang-generated
%% crash dumps. It will also check whether the location that the crash
%% dump is written to has correct permissions.
-module(riaknostic_check_dumps).
-behaviour(riaknostic_check).

-include_lib("kernel/include/file.hrl").

-export([description/0,
         valid/0,
         check/0,
         format/1]).

-spec description() -> string().
description() ->
    "Find crash dumps".

-spec valid() -> true.
valid() ->
    true.

-spec check() -> [{lager:log_level(), term()}].
check() ->
    CrashDumpConfig = riaknostic_config:get_vm_env("ERL_CRASH_DUMP"),
    {DumpDir, DumpFile} = case CrashDumpConfig of
                              undefined ->
                                  Cwd = riaknostic_config:base_dir(),
                                  {Cwd, filename:absname([Cwd, "erl_crash.dump"])};
                              File ->
                                  AbsFile = filename:absname(File, riaknostic_config:base_dir()),
                                  {filename:dirname(AbsFile), AbsFile}
                          end,
    Messages = case file:read_file_info(DumpDir) of
                   {error, enoent} ->
                       [{error, {enoent, DumpDir}}];
                   {error, _} ->
                       [{error, {eacces, DumpDir}}];
                   {ok, #file_info{access=Access}} when Access =/= read_write ->
                       [{error, {eacces, DumpDir}}];
                   _ ->
                       []
               end,
    case filelib:is_file(DumpFile) of
        true ->
            [{warning, {crash_dump, DumpFile}}|Messages];
        _ ->
            Messages
    end.

-spec format(term()) -> {io:format(), [term()]}.
format({eacces, Dir}) ->
    {"Crash dump directory ~s is not writeable by Riak. Please set -env ERL_CRASH_DUMP <dir>/erl_crash.dump in vm.args to a writeable path.", [Dir]};
format({enoent, Dir}) ->
    {"Crash dump directory ~s does not exist. Please set -env ERL_CRASH_DUMP <dir>/erl_crash.dump in vm.args to a writeable path.", [Dir]};
format({crash_dump, File}) ->
    {ok, #file_info{mtime=MTime}} = file:read_file_info(File),
    {"Riak crashed at ~s, leaving crash dump in ~s. Please inspect or remove the file.", [httpd_util:rfc1123_date(MTime), File]}.
