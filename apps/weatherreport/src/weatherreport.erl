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
%% File renamed from riaknostic.erl to weatherreport.erl and modified
%% to work with Apache CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% -------------------------------------------------------------------

%% @doc <p>The <code>weatherreport</code> module is the entry point for
%% the escript. It is responsible for parsing command-line arguments
%% and switches, printing the available checks, listing the help text,
%% or running all or the specified checks, depending on the command
%% line.</p>
%%
%% <p>The <code>getopt</code> application and module is used
%% for command-line parsing. The defined switches and arguments are:</p>
%% <pre>$ ./weatherreport --etc etc [-d level] [-l] [-h] [check_name...]</pre>
%%
%% <table class="options">
%% <tr><td><code>--etc etc</code></td><td>the location of the CouchDB
%%   configuration directory</td></tr>
%% <tr><td><code>-d, --level level</code>&#160;&#160;</td><td>the severity of
%%   messages you want to see, defaulting to 'notice'. Equivalent to
%%   syslog severity levels.</td></tr>
%% <tr><td><code>-l, --list</code></td><td>lists available checks,
%%   that is, modules that implement <code>weatherreport_check</code>. A
%%   "short name" will be given for ease-of-use.</td></tr>
%% <tr><td><code>-h, --help</code></td><td> - print command usage
%%   ("help")</td></tr>
%% <tr><td><code>check_name</code></td><td>when given, a specific
%%   check or list of checks to run</td></tr>
%% </table>
%% @end
-module(weatherreport).
-export([main/1]).

-define(OPTS, [
    {etc, $c, "etc", string, "Path to the CouchDB configuration directory"},
    {level, $d, "level", {atom, notice}, "Minimum message severity level (default: notice)"},
    {expert, $e, "expert", undefined, "Perform more detailed diagnostics"},
    {usage, $h, "help", undefined, "Display help/usage"},
    {list, $l, "list", undefined, "Describe available diagnostic tasks"},
    {all_nodes, $a, "all-nodes", undefined, "Run weatherreport on all cluster nodes"},
    {timeout, $t, "timeout", integer, "Timeout value (in ms) for each diagnostic check"}
]).

-define(USAGE_OPTS, [
    O
 || O <- ?OPTS,
    element(5, O) =/= undefined
]).

%% @doc The main entry point for the weatherreport escript.
-spec main(CommandLineArguments :: [string()]) -> any().
main(Args) ->
    application:load(weatherreport),

    case weatherreport_getopt:parse(?OPTS, Args) of
        {ok, {Opts, NonOptArgs}} ->
            case process_opts(Opts) of
                list -> list_checks();
                usage -> usage();
                run -> run(NonOptArgs)
            end;
        {error, Error} ->
            io:format("Invalid option sequence given: ~w~n", [Error]),
            usage()
    end.

list_checks() ->
    Descriptions = [
        {weatherreport_util:short_name(Mod), Mod:description()}
     || Mod <- weatherreport_check:modules()
    ],
    io:format("Available diagnostic checks:~n~n"),
    lists:foreach(
        fun({Mod, Desc}) ->
            io:format("  ~.20s ~s~n", [Mod, Desc])
        end,
        lists:sort(Descriptions)
    ).

usage() ->
    weatherreport_getopt:usage(?USAGE_OPTS, "weatherreport ", "[check_name ...]", [
        {"check_name", "A specific check to run"}
    ]).

run(InputChecks) ->
    case weatherreport_config:prepare() of
        {error, Reason} ->
            io:format("Fatal error: ~s~n", [Reason]),
            halt(1);
        _ ->
            ok
    end,
    Checks =
        case InputChecks of
            [] ->
                weatherreport_check:modules();
            _ ->
                ShortNames = [
                    {weatherreport_util:short_name(Mod), Mod}
                 || Mod <- weatherreport_check:modules()
                ],
                element(1, lists:foldr(fun validate_checks/2, {[], ShortNames}, InputChecks))
        end,
    Messages =
        case application:get_env(weatherreport, all_nodes) of
            {ok, true} ->
                weatherreport_runner:run(Checks, all);
            _ ->
                weatherreport_runner:run(Checks)
        end,
    case Messages of
        [] ->
            io:format("No diagnostic messages to report.~n"),
            halt(0);
        _ ->
            %% Print the most critical messages first
            FilteredMessages = lists:filter(
                fun({_, Level, _, _}) ->
                    weatherreport_log:should_log(Level)
                end,
                Messages
            ),
            SortedMessages = lists:sort(
                fun({_, ALevel, _, _}, {_, BLevel, _, _}) ->
                    weatherreport_log:level(ALevel) =< weatherreport_log:level(BLevel)
                end,
                FilteredMessages
            ),
            case SortedMessages of
                [] ->
                    io:format("No diagnostic messages to report.~n"),
                    halt(0);
                _ ->
                    lists:foreach(fun weatherreport_check:print/1, SortedMessages),
                    weatherreport_util:flush_stdout(),
                    halt(1)
            end,
            halt(1)
    end.

validate_checks(Check, {Mods, SNames}) ->
    case lists:keyfind(Check, 1, SNames) of
        {Check, Mod} ->
            {[Mod | Mods], lists:delete({Check, Mod}, SNames)};
        _ ->
            io:format("Unknown check '~s' specified, skipping.~n", [Check]),
            {Mods, SNames}
    end.

process_opts(Opts) ->
    process_opts(Opts, run).

process_opts([], Result) ->
    Result;
process_opts([H | T], Result) ->
    process_opts(T, process_option(H, Result)).

process_option({etc, Path}, Result) ->
    application:set_env(weatherreport, etc, filename:absname(Path)),
    Result;
process_option({level, Level}, Result) ->
    application:set_env(weatherreport, log_level, Level),
    Result;
process_option({timeout, Timeout}, Result) ->
    application:set_env(weatherreport, timeout, Timeout),
    Result;
process_option(expert, Result) ->
    application:set_env(weatherreport, expert, true),
    Result;
process_option(all_nodes, Result) ->
    application:set_env(weatherreport, all_nodes, true),
    Result;
%% Help should have precedence over listing checks
process_option(list, usage) ->
    usage;
process_option(list, _) ->
    list;
process_option(usage, _) ->
    usage.
