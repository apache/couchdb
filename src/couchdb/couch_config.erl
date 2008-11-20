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
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2,
        terminate/2, code_change/3]).
-export([all/0, get/1, get/2, get/3, delete/2, set/3, set/4, register/1,
        register/2, load_ini_file/1]).

-record(config,
    {notify_funs=[],
    write_filename=""
    }).

%% Public API %%

%% @type etstable() = integer().

start_link(IniFiles) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, IniFiles, []).

all() ->
    lists:sort(ets:tab2list(?MODULE)).


get(Section) when is_binary(Section) ->
    ?MODULE:get(?b2l(Section));
get(Section) ->
    Matches = ets:match(?MODULE, {{Section, '$1'}, '$2'}),
    [{Key, Value} || [Key, Value] <- Matches].


get(Section, Key) ->
    ?MODULE:get(Section, Key, undefined).
    
get(Section, Key, Default) when is_binary(Section) and is_binary(Key) ->
    ?MODULE:get(?b2l(Section), ?b2l(Key), Default);
get(Section, Key, Default) ->
    case ets:lookup(?MODULE, {Section, Key}) of
    [] -> Default;
    [{_,Result}] -> Result
    end.

set(Section, Key, Value) ->
    set(Section, Key, Value, true).

set(Section, Key, Value, Persist) when is_binary(Section) and is_binary(Key)  ->
    set(?b2l(Section), ?b2l(Key), Value, Persist);
set(Section, Key, Value, Persist) ->
    gen_server:call(?MODULE, {set, [{{Section, Key}, Value}], Persist}).

delete(Section, Key) ->
    set(Section, Key, "").

register(Fun) ->
    ?MODULE:register(Fun, self()).

register(Fun, Pid) ->
    gen_server:call(?MODULE, {register, Fun, Pid}).

%% Private API %%

%% @spec init(List::list([])) -> {ok, Tab::etsatable()}
%% @doc Creates a new ets table of the type "set".
init(IniFiles) ->
    ets:new(?MODULE, [named_table, set, protected]),
    [ok = load_ini_file(IniFile) || IniFile <- IniFiles],
    {ok, #config{write_filename=lists:last(IniFiles)}}.

handle_call({set, KVs, Persist}, _From, Config) ->
    lists:map(
        fun({{Section, Key}, Value}=KV) ->
            true = ets:insert(?MODULE, KV),
            if Persist ->
                ok = couch_config_writer:save_to_file(KV,
                        Config#config.write_filename);
            true -> ok
            end,
            [catch F(Section, Key, Value)
                    || {_Pid, F} <- Config#config.notify_funs]
        end, KVs),
    {reply, ok, Config};

handle_call({register, Fun, Pid}, _From, #config{notify_funs=PidFuns}=Config) ->
    erlang:monitor(process, Pid),
    % convert 1 and 2 arity to 3 arity
    Fun2 = 
    if is_function(Fun, 1) ->
        fun(Section, _Key, _Value) -> Fun(Section) end;
    is_function(Fun, 2) ->
        fun(Section, Key, _Value) -> Fun(Section, Key) end;
    is_function(Fun, 3) ->
        Fun
    end,
    {reply, ok, Config#config{notify_funs=[{Pid, Fun2}|PidFuns]}}.
    
    

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
           ?LOG_ERROR("~s~n", [Msg]),
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
                case regexp:split(Line2, "\s?=\s?") of
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
