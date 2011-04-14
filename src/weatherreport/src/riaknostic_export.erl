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

%% @doc Gather and export system stats and other 
%% diagnostic output.
%% @end

-module(riaknostic_export).
-export([export/0]).

%% @doc wrapper for all the moving parts of the export function
%% XXX/evan terrible first draft error-handling
-spec export() -> ok | {error, string()}.
export() ->
    TmpDir = prep_tmp_dir(),
    CmdList = get_cmd_list(),
    FileList = get_file_list(),
    copy_to_dir(FileList, TmpDir),
    Outputs = run_commands(CmdList),
    write_to_file(Outputs, TmpDir),
    package_files(TmpDir),
    cleanup_tmp_dir(TmpDir).

get_cmd_list() ->
    List = [ 
             {"df", "df"}
           ], 
    Stats = [
             {"iostat", "iostat 1 4"},
             {"vmstat", "vmstat 1 4"},
             {"sysctl-net", "sysctl net"}
            ],             
    PerOS = case os:type() of 
                {unix, linux}  -> 
                    [
                     {"swappiness", "sysctl vm.swappiness"}
                    ] ++ Stats;
                {unix, darwin} -> 
                    [
                    ];  % unsupported for production
                {unix, freebsd} ->
                    [
                    ] ++ Stats;
                {unix, openbsd} ->
                    [
                    ] ++ Stats;
                {unix, sunos} ->
                    [
                    ];
                _ -> [] %maybe explicitly error here?
            end,
    List ++ PerOS.

get_file_list() ->
    List = [
            "/etc/hosts", % might want to omit
            "/etc/hostname",
            "/etc/riak/", %TODO get from ENV variable
            "/etc/fstab", 
            "/var/log/riak/" %TODO get from ENV variable
           ],
    PerOS = case os:type() of 
                {unix, linux} ->
                    [ 
                      "/etc/fstab"
                    ];
                {unix, darwin} -> []; % unsupported for production
                {unix, freebsd} ->
                    [
                    ];
                {unix, openbsd} ->
                    [
                    ];
                {unix, sunos} ->
                    [
                    ]; 
                _ -> [] %maybe explicitly error here?
            end,
    List ++ PerOS.

run_commands(CmdList) ->
    [{Name, run_command(Cmd)} || 
        {Name, Cmd} <- CmdList].

run_command(Cmd) ->
    try
        riaknostic_util:run_command(Cmd)
    catch 
        Error:Reason ->
            io:format("when running ~s got ~s:~s~n ~w~n", 
                      [Cmd, Error, Reason, erlang:get_stacktrace()]),
            "got error\n"
    end.
            

copy_to_dir([], _Dir) ->
    ok;
copy_to_dir(FileList, Dir) ->
    [FileName|Tail] = FileList,
    case lists:last(FileName) of 
        $/ -> 
            %%add files in this dir to tail
            {ok, Files} = file:list_dir(FileName),
            copy_to_dir([FileName ++ File || File <- Files] ++ Tail, 
                        Dir);
        _ -> 
            Res = file:copy(FileName, 
                            Dir ++ filename:basename(FileName)),
            case Res of
                {ok, _} -> ok;
                {error, Reason} ->
                    %TODO: Something better than a hard stop
                    io:format("Couldn't copy ~s. Error: ~s", 
                              [FileName, Reason])
            end,
            copy_to_dir(Tail, Dir)
    end.

write_to_file([], _Dir) -> 
    ok;
write_to_file(Outputs, Dir) ->
    [H|T] = Outputs,
    {Filename, Output} = H,
    file:write_file(Dir ++ "/" ++ Filename, Output),
    write_to_file(T, Dir).

package_files(Dir) -> 
    % gathered everything, now package it;
    {ok, NameList} = file:list_dir(Dir),
    FileList = ["export/" ++ File || File <- NameList],
    PrefLen = string:len(Dir) - string:len("export/"),
    Prefix = string:sub_string(Dir, 1, PrefLen),
    {ok, Cwd} = file:get_cwd(),
    io:format("Writing export file: ~s~n", 
              [Cwd ++ "/export.zip"]),
    zip:zip("export.zip", FileList, [{cwd, Prefix}]).

prep_tmp_dir() ->
    % this may not be a good idea
    {A, B, C} = now(),
    {ok, Cwd} = file:get_cwd(),
    DirPrefix = Cwd ++ "/export" ++ integer_to_list(A+B+C),
    file:make_dir(DirPrefix),
    DirName = DirPrefix ++ "/export/",
    file:make_dir(DirName),
    DirName.

cleanup_tmp_dir(DirName) ->
    {ok, FileNames} = file:list_dir(DirName),
    lists:map(fun(File) -> file:delete(DirName ++ File) end, 
              FileNames),
    ok = file:del_dir(DirName),
    PrefLen = string:len(DirName) - string:len("export/"),
    Prefix = string:sub_string(DirName, 1, PrefLen),
    ok = file:del_dir(Prefix).


