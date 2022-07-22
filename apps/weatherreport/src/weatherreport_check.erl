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
%% File renamed from riaknostic_check.erl to weatherreport_check.erl
%% and modified to work with Apache CouchDB
%%
%% Copyright (c) 2014 Cloudant
%%
%% -------------------------------------------------------------------

%% @doc <p>Enforces a common API among all diagnostic modules and
%% provides some automation around their execution.</p>
%% <h2>Behaviour Specification</h2>
%%
%% <h3>description/0</h3>
%% <pre>-spec description() -> iodata().</pre>
%% <p>A short description of what the diagnostic does, which will be
%% printed when the script is given the <code>-l</code> flag.</p>
%%
%% <h3>valid/0</h3>
%% <pre>-spec valid() -> boolean().</pre>
%% <p>Whether the diagnostic is valid to run. For example, some checks
%% require connectivity to the cluster node and hence call {@link
%% weatherreport_node:can_connect/0. weatherreport_node:can_connect()}.</p>
%%
%% <h3>check/0</h3>
%% <pre>-spec check() -> [{atom(), term()}].</pre>
%% <p>Runs the diagnostic, returning a list of pairs, where the first
%% is a severity level and the second is any term that is understood
%% by the <code>format/1</code> callback.</p>
%%
%% <h3>format/1</h3>
%% <pre>-spec format(term()) -> iodata() | {io:format(), [term()]}.</pre>
%% <p>Formats terms that were returned from <code>check/0</code> for
%% output to the console. Valid return values are an iolist (string,
%% binary, etc) or a pair of a format string and a list of terms, as
%% you would pass to {@link io:format/2. io:format/2}.</p>
%% @end

-module(weatherreport_check).
-export([behaviour_info/1]).
-export([
    check/2,
    modules/0,
    print/1
]).

%% @doc The behaviour definition for diagnostic modules.
-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
    [
        {description, 0},
        {valid, 0},
        {check, 1},
        {format, 1}
    ];
behaviour_info(_) ->
    undefined.

%% @doc Runs the diagnostic in the given module, if it is valid. Returns a
%% list of messages that will be printed later using print/1.
-spec check(Module :: module(), list()) -> [{atom(), module(), term()}].
check(Module, Opts) ->
    case Module:valid() of
        true ->
            [{Level, Module, Message} || {Level, Message} <- Module:check(Opts)];
        _ ->
            []
    end.

%% @doc Collects a list of diagnostic modules included in the
%% weatherreport application.
-spec modules() -> [module()].
modules() ->
    {ok, Mods} = application:get_key(weatherreport, modules),
    [
        M
     || M <- Mods,
        Attr <- M:module_info(attributes),
        {behaviour, [?MODULE]} =:= Attr orelse {behavior, [?MODULE]} =:= Attr
    ].

%% @doc Formats and prints the given message. The diagnostic
%% module's format/1 function will be called to provide a
%% human-readable message. It should return an iolist() or a 2-tuple
%% consisting of a format string and a list of terms.
-spec print({Node :: atom(), Level :: atom(), Module :: module(), Data :: term()}) -> ok.
print({Node, Level, Mod, Data}) ->
    case Mod:format(Data) of
        {Format, Terms} ->
            weatherreport_log:log(Node, Level, Format, Terms);
        String ->
            weatherreport_log:log(Node, Level, String)
    end.
