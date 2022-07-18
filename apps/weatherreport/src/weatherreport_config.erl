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
%% File renamed from riaknostic_config.erl to weatherreport_config.erl
%% Copyright (c) 2014 Cloudant
%%
%% -------------------------------------------------------------------

%% @doc Provides convenient access to configuration values.  When
%% the {@link weatherreport. weatherreport} module calls {@link
%% prepare/0. prepare/0}, CouchDB's <code>default.ini</code>,
%% <code>local.ini</code> and <code>vm.args</code> files will be
%% parsed and memoized.
%% @end

-module(weatherreport_config).

-export([
    prepare/0,
    data_directories/0,
    get_vm_env/1,
    etc_dir/0,
    timeout/0,
    node_name/0,
    cookie/0,
    user/0
]).

%% @doc Prepares appropriate configuration to the weatherreport script
%%      can run.  This is called by the weaterreport module and you do
%%      not need to invoke it.
-spec prepare() -> ok | {error, iodata()}.
prepare() ->
    prepare([fun load_app_config/0, fun load_vm_args/0]).

prepare([]) ->
    ok;
prepare([Fun | T]) ->
    case Fun() of
        {error, Reason} ->
            {error, Reason};
        _ ->
            prepare(T)
    end.

%% @doc Determines where CouchDB is configured to store data. Returns a
%%      list of paths to directories defined by storage backends.
-spec data_directories() -> [file:filename()].
data_directories() ->
    [config:get("couchdb", "view_index_dir"), config:get("couchdb", "database_dir")].

%% @doc Get an -env flag out of the vm.args file.
-spec get_vm_env(string()) -> string() | undefined.
get_vm_env(Key) ->
    case application:get_env(weatherreport, vm_env) of
        undefined ->
            undefined;
        {ok, PList} ->
            proplists:get_value(Key, PList)
    end.

%% @doc Determines the user/uid that the script is running as.
-spec user() -> string().
user() ->
    case weatherreport_util:run_command("whoami") of
        [] ->
            undefined;
        Resp ->
            [_Newline | Resp1] = lists:reverse(Resp),
            lists:reverse(Resp1)
    end.

%% @doc The specified timeout value for diagnostic checks run via RPC
-spec timeout() -> integer().
timeout() ->
    case application:get_env(weatherreport, timeout) of
        {ok, Timeout} ->
            Timeout;
        _ ->
            300000
    end.

%% @doc The CouchDB configuration directory.
-spec etc_dir() -> file:filename().
etc_dir() ->
    case application:get_env(weatherreport, etc) of
        undefined ->
            ExecDir = filename:absname(filename:dirname(escript:script_name())),
            filename:join(ExecDir, "../etc");
        {ok, Path} ->
            filename:absname(Path, "/")
    end.

%% @doc The local node name. Includes whether the node uses short
%% or long nodenames for distributed Erlang.
-spec node_name() -> {shortnames | longnames, Name :: string()}.
node_name() ->
    case application:get_env(weatherreport, node_name) of
        undefined ->
            undefined;
        {ok, Node} ->
            Node
    end.

%% @doc The node's distributed Erlang cookie.
-spec cookie() -> atom().
cookie() ->
    case application:get_env(weatherreport, cookie) of
        undefined ->
            undefined;
        {ok, Cookie} ->
            list_to_atom(Cookie)
    end.

load_app_config() ->
    Etc = ?MODULE:etc_dir(),
    IniFiles = [
        filename:join(Etc, "default.ini"),
        filename:join(Etc, "local.ini")
    ],
    weatherreport_log:log(node(), debug, "Reading config from files: ~p", [IniFiles]),
    {ok, _Pid} = config:start_link(IniFiles),
    weatherreport_log:log(node(), debug, "Local node config: ~p~n", [config:all()]).

load_vm_args() ->
    VmArgs =
        case init:get_argument(vm_args) of
            {ok, [[X]]} ->
                X;
            _ ->
                %% This is a backup. If for some reason -vm_args isn't specified
                %% then assume it lives in the same dir as app.config
                filename:absname("./vm.args", ?MODULE:etc_dir())
        end,

    case file:read_file(VmArgs) of
        {error, Reason} ->
            {error, io_lib:format("Could not read ~s, received error ~w!", [VmArgs, Reason])};
        {ok, Binary} ->
            load_vm_args(Binary)
    end.

load_vm_args(Bin) when is_binary(Bin) ->
    load_vm_args(re:split(Bin, "\s*\r?\n\s*", [{return, list}, trim]));
load_vm_args([]) ->
    ok;
load_vm_args([[$# | _] | T]) ->
    load_vm_args(T);
load_vm_args(["" | T]) ->
    load_vm_args(T);
load_vm_args(["-sname " ++ NodeName | T]) ->
    application:set_env(weatherreport, node_name, {shortnames, string:strip(NodeName)}),
    load_vm_args(T);
load_vm_args(["-name " ++ NodeName | T]) ->
    application:set_env(weatherreport, node_name, {longnames, string:strip(NodeName)}),
    load_vm_args(T);
load_vm_args(["-setcookie " ++ Cookie | T]) ->
    application:set_env(weatherreport, cookie, string:strip(Cookie)),
    load_vm_args(T);
load_vm_args(["-env " ++ Env | T]) ->
    [Key, Value] = re:split(Env, "\s+", [{return, list}, trim]),
    add_or_insert_env(vm_env, {Key, Value}),
    load_vm_args(T);
load_vm_args([[$+ | EmuFlags] | T]) ->
    [Flag | Rest] = re:split(EmuFlags, "\s+", [{return, list}, trim]),
    add_or_insert_env(emu_flags, {[$+ | Flag], Rest}),
    load_vm_args(T);
load_vm_args([[$- | InitFlags] | T]) ->
    [Flag | Rest] = re:split(InitFlags, "\s+", [{return, list}, trim]),
    add_or_insert_env(init_flags, {[$- | Flag], Rest}),
    load_vm_args(T);
load_vm_args([Line | _]) ->
    {error, io_lib:format("Erroneous line in vm.args: ~s", [Line])}.

add_or_insert_env(Key, Value) ->
    case application:get_env(weatherreport, Key) of
        undefined ->
            application:set_env(weatherreport, Key, [Value]);
        {ok, List} ->
            application:set_env(weatherreport, Key, [Value | List])
    end.
