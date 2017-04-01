% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_epi_data).

-include("couch_epi.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([interval/1, data/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

interval(Specs) ->
    extract_minimal_interval(Specs).

data(Specs) ->
    Locators = locate_sources(Specs),
    case lists:foldl(fun collect_data/2, {ok, [], []}, Locators) of
        {ok, Hashes, Data} ->
            {ok, couch_epi_util:hash(Hashes), Data};
        Error ->
            Error
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

collect_data({App, Locator}, {ok, HashAcc, DataAcc}) ->
    case definitions(Locator) of
        {ok, Hash, Data} ->
            {ok, [Hash | HashAcc], [{App, Data} | DataAcc]};
        Error ->
            Error
    end;
collect_data({_App, _Locator}, Error) ->
    Error.

extract_minimal_interval(Specs) ->
    lists:foldl(fun minimal_interval/2, undefined, Specs).

minimal_interval({_App, #couch_epi_spec{options = Options}}, Min) ->
    case lists:keyfind(interval, 1, Options) of
        {interval, Interval} -> min(Interval, Min);
        false -> Min
    end.

locate_sources(Specs) ->
    lists:map(fun({ProviderApp, #couch_epi_spec{value = Src}}) ->
        {ok, Locator} = locate(ProviderApp, Src),
        {ProviderApp, Locator}
    end, Specs).

locate(App, {priv_file, FileName}) ->
    case priv_path(App, FileName) of
        {ok, FilePath} ->
            ok = check_exists(FilePath),
            {ok, {file, FilePath}};
        Else ->
            Else
    end;
locate(_App, {file, FilePath}) ->
    ok = check_exists(FilePath),
    {ok, {file, FilePath}};
locate(_App, Locator) ->
    {ok, Locator}.

priv_path(AppName, FileName) ->
    case code:priv_dir(AppName) of
        {error, _Error} = Error ->
            Error;
        Dir ->
            {ok, filename:join(Dir, FileName)}
    end.

check_exists(FilePath) ->
    case filelib:is_regular(FilePath) of
        true ->
            ok;
        false ->
            {error, {notfound, FilePath}}
    end.

definitions({file, FilePath}) ->
    case file:consult(FilePath) of
        {ok, Data} ->
            {ok, hash_of_file(FilePath), Data};
        {error, Reason} ->
            {error, {FilePath, Reason}}
    end;
definitions({module, Module}) when is_atom(Module) ->
    definitions({module, [Module]});
definitions({module, Modules}) ->
    Data = lists:append([M:data() || M <- Modules]),
    Hash = couch_epi_functions_gen:hash(Modules),
    {ok, Hash, Data}.

hash_of_file(FilePath) ->
    {ok, Data} = file:read_file(FilePath),
    couch_epi_util:md5(Data).
