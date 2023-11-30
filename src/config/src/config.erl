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

% Reads CouchDB's ini file and gets queried for configuration parameters.
% This module is initialized with a list of ini files that it consecutively
% reads Key/Value pairs from and saves them in an ets table. If more an one
% ini file is specified, the last one is used to write changes that are made
% with store/2 back to that ini file.

-module(config).
-behaviour(gen_server).

-export([start_link/1, stop/0, reload/0]).

-export([all/0]).
-export([get/1, get/2, get/3]).
-export([set/3, set/4, set/5]).
-export([delete/2, delete/3, delete/4]).

-export([get_integer/3, set_integer/3, set_integer/4]).
-export([get_float/3, set_float/3, set_float/4]).
-export([get_boolean/3, set_boolean/3, set_boolean/4]).

-export([features/0, enable_feature/1, disable_feature/1, is_enabled/1]).

-export([listen_for_changes/2]).
-export([subscribe_for_changes/1]).

-export([init/1]).
-export([handle_call/3, handle_cast/2, handle_info/2]).

-export([is_sensitive/2]).

-define(FEATURES, features).

-define(TIMEOUT, 30000).
-define(INVALID_SECTION, <<"Invalid configuration section">>).
-define(INVALID_KEY, <<"Invalid configuration key">>).
-define(INVALID_VALUE, <<"Invalid configuration value">>).

-include("config.hrl").

-record(config, {
    ini_files = undefined,
    write_filename = undefined
}).

start_link(IniFiles) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, IniFiles, []).

stop() ->
    gen_server:cast(?MODULE, stop).

reload() ->
    gen_server:call(?MODULE, reload, ?TIMEOUT).

all() ->
    lists:sort(gen_server:call(?MODULE, all, infinity)).

get_integer(Section, Key, Default) when is_integer(Default) ->
    try
        to_integer(get(Section, Key, Default))
    catch
        error:badarg ->
            Default
    end.

set_integer(Section, Key, Value) ->
    set_integer(Section, Key, Value, true).

set_integer(Section, Key, Value, Persist) when is_integer(Value) ->
    set(Section, Key, integer_to_list(Value), Persist);
set_integer(_, _, _, _) ->
    error(badarg).

to_integer(List) when is_list(List) ->
    list_to_integer(List);
to_integer(Int) when is_integer(Int) ->
    Int;
to_integer(Bin) when is_binary(Bin) ->
    list_to_integer(binary_to_list(Bin)).

get_float(Section, Key, Default) when is_float(Default) ->
    try
        to_float(get(Section, Key, Default))
    catch
        error:badarg ->
            Default
    end.

set_float(Section, Key, Value) ->
    set_float(Section, Key, Value, true).

set_float(Section, Key, Value, Persist) when is_float(Value) ->
    set(Section, Key, float_to_list(Value), Persist);
set_float(_, _, _, _) ->
    error(badarg).

to_float(List) when is_list(List) ->
    list_to_float(List);
to_float(Float) when is_float(Float) ->
    Float;
to_float(Int) when is_integer(Int) ->
    list_to_float(integer_to_list(Int) ++ ".0");
to_float(Bin) when is_binary(Bin) ->
    list_to_float(binary_to_list(Bin)).

get_boolean(Section, Key, Default) when is_boolean(Default) ->
    try
        to_boolean(get(Section, Key, Default))
    catch
        error:badarg ->
            Default
    end.

set_boolean(Section, Key, Value) ->
    set_boolean(Section, Key, Value, true).

set_boolean(Section, Key, true, Persist) ->
    set(Section, Key, "true", Persist);
set_boolean(Section, Key, false, Persist) ->
    set(Section, Key, "false", Persist);
set_boolean(_, _, _, _) ->
    error(badarg).

to_boolean(List) when is_list(List) ->
    case list_to_existing_atom(List) of
        true ->
            true;
        false ->
            false;
        _ ->
            error(badarg)
    end;
to_boolean(Bool) when is_boolean(Bool) ->
    Bool.

get(Section) when is_binary(Section) ->
    ?MODULE:get(binary_to_list(Section));
get(Section) when is_list(Section) ->
    Matches = ets:match(?MODULE, {{Section, '$1'}, '$2'}),
    [{Key, Value} || [Key, Value] <- Matches].

get(Section, Key) ->
    ?MODULE:get(Section, Key, undefined).

get(Section, Key, Default) when is_binary(Section) and is_binary(Key) ->
    ?MODULE:get(binary_to_list(Section), binary_to_list(Key), Default);
get(Section, Key, Default) when is_list(Section), is_list(Key) ->
    case ets:lookup(?MODULE, {Section, Key}) of
        [] when Default == undefined -> Default;
        [] when is_boolean(Default) -> Default;
        [] when is_float(Default) -> Default;
        [] when is_integer(Default) -> Default;
        [] when is_list(Default) -> Default;
        [] when is_atom(Default) -> Default;
        [] -> error(badarg);
        [{_, Match}] -> Match
    end.

set(Section, Key, Value) ->
    ?MODULE:set(Section, Key, Value, true, nil).

set(Sec, Key, Val, Opts) when is_binary(Sec) and is_binary(Key) ->
    ?MODULE:set(binary_to_list(Sec), binary_to_list(Key), Val, Opts);
set(Section, Key, Value, Persist) when is_boolean(Persist) ->
    ?MODULE:set(Section, Key, Value, #{persist => Persist});
set(Section, Key, Value, #{} = Opts) when
    is_list(Section), is_list(Key), is_list(Value)
->
    gen_server:call(?MODULE, {set, Section, Key, Value, Opts}, ?TIMEOUT);
set(Section, Key, Value, Reason) when
    is_list(Section), is_list(Key), is_list(Value)
->
    ?MODULE:set(Section, Key, Value, #{persist => true, reason => Reason});
set(_Sec, _Key, _Val, _Options) ->
    error(badarg).

set(Section, Key, Value, Persist, Reason) when
    is_list(Section), is_list(Key), is_list(Value)
->
    ?MODULE:set(Section, Key, Value, #{persist => Persist, reason => Reason}).

delete(Section, Key) when is_binary(Section) and is_binary(Key) ->
    delete(binary_to_list(Section), binary_to_list(Key));
delete(Section, Key) ->
    delete(Section, Key, true, nil).

delete(Section, Key, Persist) when is_boolean(Persist) ->
    delete(Section, Key, Persist, nil);
delete(Section, Key, Reason) ->
    delete(Section, Key, true, Reason).

delete(Sec, Key, Persist, Reason) when is_binary(Sec) and is_binary(Key) ->
    delete(binary_to_list(Sec), binary_to_list(Key), Persist, Reason);
delete(Section, Key, Persist, Reason) when is_list(Section), is_list(Key) ->
    gen_server:call(
        ?MODULE,
        {delete, Section, Key, Persist, Reason},
        ?TIMEOUT
    ).

features() ->
    Map = persistent_term:get({?MODULE, ?FEATURES}, #{}),
    lists:sort(maps:keys(Map)).

enable_feature(Feature) when is_atom(Feature) ->
    Map = persistent_term:get({?MODULE, ?FEATURES}, #{}),
    Map1 = Map#{Feature => true},
    persistent_term:put({?MODULE, ?FEATURES}, Map1).

disable_feature(Feature) when is_atom(Feature) ->
    Map = persistent_term:get({?MODULE, ?FEATURES}, #{}),
    Map1 = maps:remove(Feature, Map),
    persistent_term:put({?MODULE, ?FEATURES}, Map1).

is_enabled(Feature) when is_atom(Feature) ->
    Map = persistent_term:get({?MODULE, ?FEATURES}, #{}),
    maps:get(Feature, Map, false).

% Some features like FIPS mode must be enabled earlier before couch, couch_epi
% start up
%
enable_early_features() ->
    % Mark FIPS if enabled
    case crypto:info_fips() == enabled of
        true ->
            enable_feature(fips);
        false ->
            ok
    end.

listen_for_changes(CallbackModule, InitialState) ->
    config_listener_mon:subscribe(CallbackModule, InitialState).

subscribe_for_changes(Subscription) ->
    config_notifier:subscribe(Subscription).

init(IniFiles) ->
    enable_early_features(),
    ets:new(?MODULE, [named_table, set, protected, {read_concurrency, true}]),
    maps:foreach(
        fun(K, V) ->
            true = ets:insert(?MODULE, {K, V})
        end,
        ini_map(IniFiles)
    ),
    WriteFile =
        case IniFiles of
            [_ | _] -> lists:last(IniFiles);
            _ -> undefined
        end,
    debug_config(),
    {ok, #config{ini_files = IniFiles, write_filename = WriteFile}}.

handle_call(all, _From, Config) ->
    Resp = lists:sort((ets:tab2list(?MODULE))),
    {reply, Resp, Config};
handle_call({set, Sec, Key, Val, Opts}, _From, Config) ->
    Persist = maps:get(persist, Opts, true),
    Reason = maps:get(reason, Opts, nil),
    LogVal = maybe_conceal(Val, is_sensitive(Sec, Key)),
    case validate_config_update(Sec, Key, Val) of
        {error, ValidationError} ->
            couch_log:error(
                "~p: [~s] ~s = ~s rejected for reason ~p",
                [?MODULE, Sec, Key, LogVal, Reason]
            ),
            {reply, {error, ValidationError}, Config};
        ok ->
            true = ets:insert(?MODULE, {{Sec, Key}, Val}),
            couch_log:notice(
                "~p: [~s] ~s set to ~s for reason ~p",
                [?MODULE, Sec, Key, LogVal, Reason]
            ),
            ConfigWriteReturn =
                case {Persist, Config#config.write_filename} of
                    {true, undefined} ->
                        ok;
                    {true, FileName} ->
                        config_writer:save_to_file({{Sec, Key}, Val}, FileName);
                    _ ->
                        ok
                end,
            case ConfigWriteReturn of
                ok ->
                    Event = {config_change, Sec, Key, Val, Persist},
                    gen_event:sync_notify(config_event, Event),
                    {reply, ok, Config};
                {error, Else} ->
                    {reply, {error, Else}, Config}
            end
    end;
handle_call({delete, Sec, Key, Persist, Reason}, _From, Config) ->
    true = ets:delete(?MODULE, {Sec, Key}),
    couch_log:notice(
        "~p: [~s] ~s deleted for reason ~p",
        [?MODULE, Sec, Key, Reason]
    ),
    ConfigDeleteReturn =
        case {Persist, Config#config.write_filename} of
            {true, undefined} ->
                ok;
            {true, FileName} ->
                config_writer:save_to_file({{Sec, Key}, ?DELETE}, FileName);
            _ ->
                ok
        end,
    IniMap = ini_map(Config#config.ini_files),
    case maps:find({Sec, Key}, IniMap) of
        {ok, Val} ->
            true = ets:insert(?MODULE, {{Sec, Key}, Val});
        _ ->
            ok
    end,
    case ConfigDeleteReturn of
        ok ->
            Event = {config_change, Sec, Key, deleted, Persist},
            gen_event:sync_notify(config_event, Event),
            {reply, ok, Config};
        Else ->
            {reply, Else, Config}
    end;
handle_call(reload, _From, Config) ->
    % Update ets with ini values.
    IniMap = ini_map(Config#config.ini_files),
    maps:foreach(
        fun({Sec, Key} = K, V) ->
            VExisting = get(Sec, Key, V),
            true = ets:insert(?MODULE, {K, V}),
            case V =:= VExisting of
                true ->
                    ok;
                false ->
                    couch_log:notice(
                        "Reload detected config change ~s.~s = ~p",
                        [Sec, Key, maybe_conceal(V, is_sensitive(Sec, Key))]
                    ),
                    Event = {config_change, Sec, Key, V, true},
                    gen_event:sync_notify(config_event, Event)
            end
        end,
        IniMap
    ),
    % And remove anything in ets that wasn't on disk.
    ets:foldl(
        fun
            ({{Sec, Key} = K, _}, _) when not is_map_key(K, IniMap) ->
                couch_log:notice(
                    "Reload deleting in-memory config ~s.~s",
                    [Sec, Key]
                ),
                ets:delete(?MODULE, K),
                Event = {config_change, Sec, Key, deleted, true},
                gen_event:sync_notify(config_event, Event);
            (_, _) ->
                ok
        end,
        nil,
        ?MODULE
    ),
    {reply, ok, Config}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(Info, State) ->
    couch_log:error("config:handle_info Info: ~p~n", [Info]),
    {noreply, State}.

maybe_conceal(Value, _IsSensitive = false) ->
    Value;
maybe_conceal(_, _IsSensitive = true) ->
    "'****'".

is_sensitive(Section, Key) ->
    Sensitive = application:get_env(config, sensitive, #{}),
    case maps:get(Section, Sensitive, false) of
        all -> true;
        Fields when is_list(Fields) -> lists:member(Key, Fields);
        _ -> false
    end.

ini_map(IniFiles) ->
    lists:foldl(
        fun(IniFile, IniMap0) ->
            {ok, Props} = parse_ini_file(IniFile),
            IniMap = maps:from_list(Props),
            maps:merge(IniMap0, IniMap)
        end,
        #{},
        IniFiles
    ).

parse_ini_file(IniFile) ->
    IniBin = read_ini_file(IniFile),
    ParsedIniValues = parse_ini(IniBin),
    {ok, ParsedIniValues}.

parse_ini(IniBin) when is_binary(IniBin) ->
    Lines0 = re:split(IniBin, "\r\n|\n|\r|\032", [{return, list}]),
    Lines1 = lists:map(fun remove_comments/1, Lines0),
    Lines2 = lists:map(fun trim/1, Lines1),
    Lines3 = lists:filter(fun(Line) -> Line =/= "" end, Lines2),
    {_, IniValues} = lists:foldl(fun parse_fold/2, {"", []}, Lines3),
    lists:reverse(IniValues).

parse_fold("[" ++ Rest, {Section, KVs}) ->
    % Check for end ] brackend, if not found or empty section skip the rest
    case string:split(Rest, "]") of
        ["", _] -> {Section, KVs};
        [NewSection, ""] -> {NewSection, KVs};
        _Else -> {Section, KVs}
    end;
parse_fold(_Line, {"" = Section, KVs}) ->
    % Empty section don't parse any lines until we're in a section
    {Section, KVs};
parse_fold(Line, {Section, KVs}) ->
    case string:split(Line, " = ") of
        [K, V] when V =/= "" ->
            % Key may have "=" in it. Also, assert we'll never have a
            % deletion case here since we're working with a stripped line
            {Section, [{{Section, trim(K)}, trim(V)} | KVs]};
        [_] ->
            % Failed to split on " = ", so try to split on "=".
            % If the line starts with "=" or it's not a KV pair, ignore it.
            case string:split(Line, "=") of
                ["", _] -> {Section, KVs};
                [_, ""] -> {Section, KVs};
                [K, V] -> {Section, [{{Section, trim(K)}, trim(V)} | KVs]};
                [_] -> {Section, KVs}
            end
    end.

read_ini_file(IniFile) ->
    IniFilename = config_util:abs_pathname(IniFile),
    case file:read_file(IniFilename) of
        {ok, IniBin0} ->
            IniBin0;
        {error, enoent} ->
            Fmt = "Couldn't find server configuration file ~s.",
            Msg = list_to_binary(io_lib:format(Fmt, [IniFilename])),
            couch_log:error("~s~n", [Msg]),
            throw({startup_error, Msg})
    end.

remove_comments(Line) ->
    case trim(Line) of
        [$; | _] ->
            % Comment is at the start of line after it's trimmed
            "";
        NoLeadingComment when is_list(NoLeadingComment) ->
            % Check for in-line comments. In-line comments must be preceded by
            % space or a tab character.
            [NoComments | _] = re:split(NoLeadingComment, " ;|\t;", [{return, list}]),
            NoComments
    end.

trim(String) ->
    % May look silly but we're using this quite a bit
    string:trim(String).

debug_config() ->
    case ?MODULE:get("log", "level") of
        "debug" ->
            io:format("Configuration Settings:~n", []),
            lists:foreach(
                fun({{Mod, Key}, Val}) ->
                    io:format("  [~s] ~s=~p~n", [Mod, Key, Val])
                end,
                lists:sort(ets:tab2list(?MODULE))
            );
        _ ->
            ok
    end.

validate_config_update(Sec, Key, Val) ->
    %% See https://erlang.org/doc/man/re.html &
    %% https://pcre.org/original/doc/html/pcrepattern.html
    %%
    %%  only characters that are actually screen-visible are allowed
    %%  tabs and spaces are allowed
    %%  no  [ ] explicitly to avoid section header bypass
    {ok, Forbidden} = re:compile("[\]\[]+", [dollar_endonly, unicode]),
    %% Values are permitted [ ] characters as we use these in
    %% places like mochiweb socket option lists
    %% Values may also be empty to delete manual configuration
    {ok, Printable} = re:compile(
        "^[[:graph:]\t\s]*$",
        [dollar_endonly, unicode]
    ),
    case
        {
            re:run(Sec, Printable),
            re:run(Sec, Forbidden),
            re:run(Key, Printable),
            re:run(Key, Forbidden),
            re:run(Val, Printable)
        }
    of
        {{match, _}, nomatch, {match, _}, nomatch, {match, _}} -> ok;
        {nomatch, _, _, _, _} -> {error, ?INVALID_SECTION};
        {_, {match, _}, _, _, _} -> {error, ?INVALID_SECTION};
        {_, _, nomatch, _, _} -> {error, ?INVALID_KEY};
        {_, _, _, {match, _}, _} -> {error, ?INVALID_KEY};
        {_, _, _, _, nomatch} -> {error, ?INVALID_VALUE}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

to_integer_test() ->
    ?assertEqual(1, to_integer(1)),
    ?assertEqual(1, to_integer(<<"1">>)),
    ?assertEqual(1, to_integer("1")),
    ?assertEqual(-1, to_integer("-01")),
    ?assertEqual(0, to_integer("-0")),
    ?assertEqual(0, to_integer("+0")),
    ok.

to_float_test() ->
    ?assertEqual(1.0, to_float(1)),
    ?assertEqual(1.0, to_float(<<"1.0">>)),
    ?assertEqual(1.0, to_float("1.0")),
    ?assertEqual(-1.1, to_float("-01.1")),
    ?assertEqual(0.0, to_float("-0.0")),
    ?assertEqual(0.0, to_float("+0.0")),
    ok.

validation_test() ->
    ?assertEqual(ok, validate_config_update("section", "key", "value")),
    ?assertEqual(ok, validate_config_update("delete", "empty_value", "")),
    ?assertEqual(
        {error, ?INVALID_SECTION},
        validate_config_update("sect[ion", "key", "value")
    ),
    ?assertEqual(
        {error, ?INVALID_SECTION},
        validate_config_update("sect]ion", "key", "value")
    ),
    ?assertEqual(
        {error, ?INVALID_SECTION},
        validate_config_update("section\n", "key", "value")
    ),
    ?assertEqual(
        {error, ?INVALID_SECTION},
        validate_config_update("section\r", "key", "value")
    ),
    ?assertEqual(
        {error, ?INVALID_SECTION},
        validate_config_update("section\r\n", "key", "value")
    ),
    ?assertEqual(
        {error, ?INVALID_KEY},
        validate_config_update("section", "key\n", "value")
    ),
    ?assertEqual(
        {error, ?INVALID_KEY},
        validate_config_update("section", "key\r", "value")
    ),
    ?assertEqual(
        {error, ?INVALID_KEY},
        validate_config_update("section", "key\r\n", "value")
    ),
    ?assertEqual(
        {error, ?INVALID_VALUE},
        validate_config_update("section", "key", "value\n")
    ),
    ?assertEqual(
        {error, ?INVALID_VALUE},
        validate_config_update("section", "key", "value\r")
    ),
    ?assertEqual(
        {error, ?INVALID_VALUE},
        validate_config_update("section", "key", "value\r\n")
    ),
    ?assertEqual(
        {error, ?INVALID_KEY},
        validate_config_update("section", "k[ey", "value")
    ),
    ?assertEqual(
        {error, ?INVALID_KEY},
        validate_config_update("section", "[key", "value")
    ),
    ?assertEqual(
        {error, ?INVALID_KEY},
        validate_config_update("section", "key]", "value")
    ),
    ok.

ini(List) when is_list(List) ->
    parse_ini(list_to_binary(List)).

parse_skip_test() ->
    ?assertEqual([], ini("")),
    ?assertEqual([], ini("k")),
    ?assertEqual([], ini("\n")),
    ?assertEqual([], ini("\r\n")),
    ?assertEqual([], ini("[s]")),
    ?assertEqual([], ini("\n[s]\n")),
    ?assertEqual([], ini("[s ]")),
    ?assertEqual([], ini("k1\nk2")),
    ?assertEqual([], ini("=")),
    ?assertEqual([], ini("==")),
    ?assertEqual([], ini("===")),
    ?assertEqual([], ini("= =")),
    ?assertEqual([], ini(" = ")),
    ?assertEqual([], ini(";")),
    ?assertEqual([], ini(";;")),
    ?assertEqual([], ini(" ;")),
    ?assertEqual([], ini("k = v")),
    ?assertEqual([], ini("[s]\n;k = v")),
    ?assertEqual([], ini("[s\nk=v")),
    ?assertEqual([], ini("s[\nk=v")),
    ?assertEqual([], ini("s]\nk=v")),
    ?assertEqual([], ini(";[s]\nk = v")),
    ?assertEqual([], ini(" ; [s]\nk = v")),
    ?assertEqual([], ini("[s]\n ;k = v")),
    ?assertEqual([], ini("[s]\n;;k = v")),
    ?assertEqual([], ini("[s]\n\t;k = v")),
    ?assertEqual([], ini("[s]\nk ;=v")),
    ?assertEqual([], ini("[s]\n ; k = v")),
    ?assertEqual([], ini("[]\nk = v")),
    ?assertEqual([], ini(";[s]\n ")).

parse_basic_test() ->
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\nk=v")),
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\n\nk=v")),
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\n\r\n\nk=v")),
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\n;\nk=v")),
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\nk = v")),
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\nk= v")),
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\nk  =v")),
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\nk=  v")),
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\nk=  v  ")),
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\nk  = v")),
    ?assertEqual([{{"s", "k"}, "v"}], ini("[s]\nk = v ; c")).

parse_extra_equal_sign_test() ->
    ?assertEqual([{{"s", "k"}, "=v"}], ini("[s]\nk==v")),
    ?assertEqual([{{"s", "k"}, "v="}], ini("[s]\nk=v=")),
    ?assertEqual([{{"s", "k"}, "=v"}], ini("[s]\nk ==v")),
    ?assertEqual([{{"s", "k"}, "==v"}], ini("[s]\nk===v")),
    ?assertEqual([{{"s", "k"}, "v=v"}], ini("[s]\nk=v=v")),
    ?assertEqual([{{"s", "k"}, "=v"}], ini("[s]\nk = =v")),
    ?assertEqual([{{"s", "k"}, "=v"}], ini("[s]\nk= =v")),
    ?assertEqual([{{"s", "k="}, "v"}], ini("[s]\nk= = v")),
    ?assertEqual([{{"s", "=k="}, "v"}], ini("[s]\n=k= = v")),
    ?assertEqual([{{"s", "==k=="}, "v"}], ini("[s]\n==k== = v")).

parse_delete_test() ->
    ?assertEqual([], ini("[s]\nk=")),
    ?assertEqual([], ini("[s]\nk =")),
    ?assertEqual([], ini("[s]\nk = ")),
    ?assertEqual([], ini("[s]\nk= ")),
    ?assertEqual([], ini("[s]\nk = ")),
    ?assertEqual([], ini("[s]\nk = ;")),
    ?assertEqual([], ini("[s]\nk  =\t;")),
    ?assertEqual([], ini("[s]\nk=\n")),
    ?assertEqual([], ini("[s]\nk = ; ;")),
    ?assertEqual([], ini("[s]\nk = ;v")),
    ?assertEqual([], ini("[s]\nk = ;")).

parse_comments_test() ->
    ?assertEqual([], ini("[s]\n;k=v")),
    ?assertEqual([{{"s", "k"}, ";"}], ini("[s]\nk=;")),
    ?assertEqual([{{"s", "k"}, ";"}], ini("[s]\nk =;")),
    ?assertEqual([{{"s", "k"}, "v;"}], ini("[s]\nk = v;")),
    ?assertEqual([{{"s", "k"}, ";v;"}], ini("[s]\nk =;v;")),
    ?assertEqual([{{"s", "k"}, ";v"}], ini("[s]\nk =;v")),
    ?assertEqual([{{"s", "k"}, "v;"}], ini("[s]\nk =v;")),
    ?assertEqual([{{"s", "k"}, "v;;"}], ini("[s]\nk =v;;")),
    ?assertEqual([{{"s", "k"}, "v1;v2"}], ini("[s]\nk = v1;v2")),
    ?assertEqual([{{"s", "k"}, "v1;v2;v3"}], ini("[s]\nk = v1;v2;v3")),
    ?assertEqual([{{"s", "k"}, "v1;v2"}], ini("[s]\nk = v1;v2 ;")),
    ?assertEqual([{{"s", "k"}, "v1;v2"}], ini("[s]\nk = v1;v2\t;")),
    ?assertEqual([{{"s", "k"}, "v1;v2"}], ini("[s]\nk = v1;v2 ;;")),
    ?assertEqual([{{"s", "k"}, "v1;v2"}], ini("[s]\nk = v1;v2 ;c1; c2")).

parse_multiple_kvs_test() ->
    ?assertEqual(
        [
            {{"s", "k1"}, "v1"},
            {{"s", "k2"}, "v2"}
        ],
        ini("[s]\nk1=v1\nk2=v2")
    ),
    ?assertEqual(
        [
            {{"s", "k1"}, "v1"},
            {{"s", "k2"}, "v2"}
        ],
        ini("[s]\nk1 = v1\nk2 = v2\n")
    ),
    ?assertEqual(
        [
            {{"s1", "k"}, "v"},
            {{"s2", "k"}, "v"}
        ],
        ini("[s1]\nk=v\n;\n\n[s2]\nk=v")
    ),
    ?assertEqual(
        [
            {{"s", "k1"}, "v1"},
            {{"s", "k2"}, "v2"}
        ],
        ini("[s]\nk1=v1\ngarbage\n= more garbage\nk2=v2")
    ).

read_non_existent_ini_file_test_() ->
    {
        setup,
        fun() -> meck:expect(couch_log, error, fun(_, _) -> ok end) end,
        fun(_) -> meck:unload() end,
        [
            ?_assertException(
                throw, {startup_error, _}, read_ini_file("non-existent")
            )
        ]
    }.

-endif.
