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

-module(couch_quickjs).

-export([
    mainjs_cmd/0,
    coffee_cmd/0
]).

mainjs_cmd() ->
    Path = filename:join([priv_dir(), "couchjs_mainjs"]),
    check_path(Path),
    apply_config(Path).

coffee_cmd() ->
    Path = filename:join([priv_dir(), "couchjs_coffee"]),
    check_path(Path),
    apply_config(Path).

priv_dir() ->
    case code:priv_dir(couch_quickjs) of
        {error, bad_name} -> error({?MODULE, app_dir_not_found});
        Dir when is_list(Dir) -> Dir
    end.

check_path(Path) ->
    case filelib:is_file(Path) of
        true -> ok;
        false -> error({?MODULE, {missing, Path}})
    end.

apply_config(Path) ->
    case config:get("quickjs", "memory_limit_bytes") of
        Val when is_list(Val) -> Path ++ " -M " ++ Val;
        undefined -> Path
    end.
