%% -------------------------------------------------------------------
%%
%% derived from riaknostic - automated diagnostic tools for Riak
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
%%
%% File renamed from riaknostic_check_disk.erl to
%% weatherreport_check_disk.erl and modified to work with Apache
%% CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% -------------------------------------------------------------------

%% @doc Diagnostic that checks permissions on data directories and
%% whether noatime is set.  It will only check data directories of
%% known storage backends.
-module(weatherreport_check_disk).
-behaviour(weatherreport_check).

%% The file that we will attempt to create and read under each data directory.
-define(TEST_FILE, "weatherreport.tmp").

%% A dependent chain of permissions checking functions.
-define(CHECKPERMFUNS, [
    fun check_is_dir/1,
    fun check_is_writeable/1,
    fun check_is_readable/1,
    fun check_is_file_readable/1,
    fun check_atime/1
]).

-include_lib("kernel/include/file.hrl").

-export([
    description/0,
    valid/0,
    check/1,
    format/1
]).

-spec description() -> string().
description() ->
    "Data directory permissions and atime".

-spec valid() -> true.
valid() ->
    true.

-spec check(list()) -> [{atom(), term()}].
check(_Opts) ->
    DataDirs = weatherreport_config:data_directories(),
    %% Add additional disk checks in the function below
    lists:flatmap(
        fun(Dir) ->
            check_directory_permissions(Dir)
        end,
        DataDirs
    ).

-spec format(term()) -> {io:format(), [term()]}.
format({disk_full, DataDir}) ->
    {
        "Disk containing data directory ~s is full! "
        "Please check that it is set to the correct location and that there are not "
        "other files using up space intended for Riak.",
        [DataDir]
    };
format({no_data_dir, DataDir}) ->
    {"Data directory ~s does not exist. Please create it.", [DataDir]};
format({no_write, DataDir}) ->
    User = weatherreport_config:user(),
    {"No write access to data directory ~s. Please make it writeable by the '~s' user.", [
        DataDir, User
    ]};
format({no_read, DataDir}) ->
    User = weatherreport_config:user(),
    {"No read access to data directory ~s. Please make it readable by the '~s' user.", [
        DataDir, User
    ]};
format({write_check, File}) ->
    {"Write-test file ~s is a directory! Please remove it so this test can continue.", [File]};
format({atime, Dir}) ->
    {
        "Data directory ~s is not mounted with 'noatime'. "
        "Please remount its disk with the 'noatime' flag to improve performance.",
        [Dir]
    }.

%%% Private functions

check_directory_permissions(Directory) ->
    check_directory(Directory, ?CHECKPERMFUNS).

%% Run a list of check functions against the given directory,
%% returning the first non-ok result.
check_directory(_, []) ->
    [];
check_directory(Directory, [Check | Checks]) ->
    case Check(Directory) of
        ok ->
            check_directory(Directory, Checks);
        Message ->
            [Message]
    end.

%% Check if the path is actually a directory
check_is_dir(Directory) ->
    case filelib:is_dir(Directory) of
        true ->
            ok;
        _ ->
            {error, {no_data_dir, Directory}}
    end.

%% Check if the directory is writeable
check_is_writeable(Directory) ->
    File = filename:join([Directory, ?TEST_FILE]),
    case file:write_file(File, <<"ok">>) of
        ok ->
            ok;
        {error, Error} when Error == enoent orelse Error == eacces ->
            {error, {no_write, Directory}};
        {error, enospc} ->
            {critical, {disk_full, Directory}};
        {error, eisdir} ->
            {error, {write_check, File}}
    end.

%% Check if the directory is readable
check_is_readable(Directory) ->
    case file:read_file_info(Directory) of
        {ok, #file_info{access = Access}} when
            Access == read orelse
                Access == read_write
        ->
            ok;
        {error, eacces} ->
            {error, {no_read, Directory}};
        {error, Error} when
            Error == enoent orelse
                Error == enotdir
        ->
            {error, {no_data_dir, Directory}};
        _ ->
            {error, {no_read, Directory}}
    end.

%% Check if the file we created is readable
check_is_file_readable(Directory) ->
    File = filename:join([Directory, ?TEST_FILE]),
    case file:read_file(File) of
        {error, Error} when
            Error == eacces orelse
                Error == enotdir
        ->
            {error, {no_read, Directory}};
        {error, enoent} ->
            {error, {write_check, File}};
        _ ->
            ok
    end.

%% Check if the directory is mounted with 'noatime'
check_atime(Directory) ->
    File = filename:join([Directory, ?TEST_FILE]),
    weatherreport_util:run_command("touch -at 201401010000.00 " ++ File),
    {ok, FileInfo1} = file:read_file_info(File),
    {ok, S} = file:open(File, [read]),
    io:get_line(S, ''),
    file:close(S),
    {ok, FileInfo2} = file:read_file_info(File),
    file:delete(File),
    case (FileInfo1#file_info.atime =/= FileInfo2#file_info.atime) of
        true ->
            {notice, {atime, Directory}};
        _ ->
            ok
    end.
