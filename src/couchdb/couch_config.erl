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

%% @doc Reads CouchDB's ini file and gets queried for configuration parameters.
%%      This module is initialized with a list of ini files that it 
%%      consecutively reads Key/Value pairs from and saves them in an ets 
%%      table. If more an one ini file is specified, the last one is used to 
%%      write changes that are made with store/2 back to that ini file.

-module(couch_config).
-include("couch_db.hrl").

-behaviour(gen_server).
-export([start_link/1, init/1,
    handle_call/3, handle_cast/2, handle_info/2, 
    terminate/2, code_change/3]).
-export([store/2, register/1, register/2, 
    get/1, get/2,
    lookup_match/1, lookup_match/2,
    all/0, unset/1, load_ini_file/1]).
    
-record(config,
    {notify_funs=[],
    writeback_filename=""
    }).

%% Public API %%

%% @type etstable() = integer().

start_link(IniFiles) -> gen_server:start_link({local, ?MODULE}, ?MODULE, IniFiles, []).  

%% @spec store(Key::any(), Value::any()) -> {ok, Tab::etsatable()}
%% @doc Public API function that triggers storage of a Key/Value pair into the
%%      local ets table and writes it to the storage ini file.
store(Key, Value) -> gen_server:call(?MODULE, {store, [{Key, Value}]}).

%% @spec get(Key::any()) -> Value::any() | undefined
%% @doc Returns the value that is stored under key::any() or undefined::atom() if no
%%      such Key exists.
get(Key) ->
    ?MODULE:get(Key, undefined).

%% @spec get(Key::any(), Default::any()) -> Value::any() | Default
%% @doc Returns the value that is stored under key::any() or Default::any() if
%%      no such Key exists.
get(Key, Default) ->
    fix_lookup_result(ets:lookup(?MODULE, Key), Default).

%% @spec lookup_match(Key::any()) -> Value::any() | undefined:atom()
%% @doc Lets you look for a Key's Value specifying a pattern that gets passed 
%%      to ets::match(). Returns undefined::atom() if no Key is found.
lookup_match(Key) -> gen_server:call(?MODULE, {lookup_match, Key}).

%% @spec lookup_match(Key::any(), Default::any()) -> Value::any() | Default
%% @doc Lets you look for a Key's Value specifying a pattern that gets passed 
%%      to ets::match(). Returns Default::any() if no Key is found
lookup_match(Key, Default) -> gen_server:call(?MODULE, {lookup_match, Key, Default}).

all() -> gen_server:call(?MODULE, all).

register(Fun) -> gen_server:call(?MODULE, {register, Fun, self()}).


register(Fun, Pid) -> gen_server:call(?MODULE, {register, Fun, Pid}).

%% @spec unset(Key::any) -> ok
%% @doc Public API call to remove the configuration entry from the internal 
%%      ets table. This change is _not_ written to the storage ini file.
unset(Key) -> gen_server:call(?MODULE, {unset, Key}).

%% Private API %%

%% @spec init(List::list([])) -> {ok, Tab::etsatable()}
%% @doc Creates a new ets table of the type "set".
init(IniFiles) ->
    ets:new(?MODULE, [named_table, set, protected]),
    [ok = load_ini_file(IniFile) || IniFile <- IniFiles],
    {ok, #config{writeback_filename=lists:last(IniFiles)}}.

%% @doc see store/2
handle_call({store, KVs}, _From, Config) ->
    [ok = insert_and_commit(Config, KV) || KV <- KVs],
    {reply, ok, Config};


%% @doc See init_value/2
handle_call({init_value, Key, Value}, _From, Config) ->
    Reply = ets:insert(?MODULE, {Key, Value}),
    {reply, Reply, Config};

%% @doc See unset/1
handle_call({unset, Key}, _From, Config) ->
    ets:delete(?MODULE, Key),
    {reply, ok, Config};


%% @doc See lookup_match/2
handle_call({lookup_match, Key, Default}, _From, Config) ->
    {reply, fix_lookup_result(ets:match(?MODULE, Key), Default), Config};

handle_call(all, _From, Config) ->
    {reply, lists:sort(ets:tab2list(?MODULE)), Config};

%% @doc See register/2
handle_call({register, Fun, Pid}, _From, #config{notify_funs=PidFuns}=Config) ->
    erlang:monitor(process, Pid),
    {reply, ok, Config#config{notify_funs=[{Pid, Fun}|PidFuns]}}.
    

fix_lookup_result([{_Key, Value}], _Default) ->
    Value;
fix_lookup_result([], Default) ->
    Default;
fix_lookup_result(Values, _Default) ->
    [list_to_tuple(Value) || Value <- Values].

%% @spec insert_and_commit(Tab::etstable(), Config::any()) -> ok
%% @doc Inserts a Key/Value pair into the ets table, writes it to the storage 
%%      ini file and calls all registered callback functions for Key.
insert_and_commit(Config, KV) ->
    true = ets:insert(?MODULE, KV),
    % notify funs
    %[catch Fun(KV) || {_Pid, Fun} <- Config#config.notify_funs],
    couch_config_writer:save_to_file(KV, Config#config.writeback_filename).

%% @spec load_ini_file(IniFile::filename()) -> ok
%% @doc Parses an ini file and stores Key/Value Pairs into the ets table.
load_ini_file(IniFile) ->
    IniFilename = couch_util:abs_pathname(IniFile),
    IniBin =
    case file:read_file(IniFilename) of
        {ok, IniBin0} ->
           IniBin0;
        {error, enoent} ->
           Msg = io_lib:format("Couldn't find server configuration file ~s.", [IniFilename]),
           io:format("~s~n", [Msg]),
           throw({startup_error, Msg})
    end,
    
    {ok, Lines} = regexp:split(binary_to_list(IniBin), "\r\n|\n|\r|\032"),
    {_, ParsedIniValues} =
    lists:foldl(fun(Line, {AccSectionName, AccValues}) ->
            case string:strip(Line) of
            "[" ++ Rest ->
                case regexp:split(Rest, "\\]") of
                {ok, [NewSectionName, ""]} ->
                    {NewSectionName, AccValues};
                _Else -> % end bracket not at end, ignore this line
                    {AccSectionName, AccValues}
                end;
            ";" ++ _Comment ->
                {AccSectionName, AccValues};
            Line2 ->
                case regexp:split(Line2, "=") of
                {ok, [_SingleElement]} -> % no "=" found, ignore this line
                    {AccSectionName, AccValues};
                {ok, [""|_LineValues]} -> % line begins with "=", ignore
                    {AccSectionName, AccValues};
                {ok, [ValueName|LineValues]} -> % yeehaw, got a line!
                    RemainingLine = couch_util:implode(LineValues, "="),
                    {ok, [LineValue | _Rest]} = regexp:split(RemainingLine, " ;|\t;"), % removes comments
                    {AccSectionName, [{{AccSectionName, ValueName}, LineValue} | AccValues]}
                end
            end
        end, {"", []}, Lines),
        
        [ets:insert(?MODULE, {Key, Value}) || {Key, Value} <- ParsedIniValues],
    ok.

% Unused gen_server behaviour API functions that we need to declare.

%% @doc Unused
handle_cast(foo, State) -> {noreply, State}.


handle_info({'DOWN', _, _, DownPid, _}, #config{notify_funs=PidFuns}=Config) ->
    % remove any funs registered by the downed process
    FilteredPidFuns = [{Pid,Fun} || {Pid,Fun} <- PidFuns, Pid /= DownPid],
    {noreply, Config#config{notify_funs=FilteredPidFuns}}.

%% @doc Unused
terminate(_Reason, _State) -> ok.

%% @doc Unused
code_change(_OldVersion, State, _Extra) -> {ok, State}.