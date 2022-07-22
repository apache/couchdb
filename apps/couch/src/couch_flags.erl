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

% This module serves two functions
% - provides public API to use to get value for a given feature flag and subject
% - implements {feature_flags, couch_flags} service

% The module relies on couch_epi_data_gen which uses the data returned by
% `couch_flags_config:data()` to generate callback module `couch_epi_data_gen_flags_config`.
% The generated module shouldn't be used directly. We use following APIs
% - `couch_epi:get_handle({flags, config})` - to get handler (name of generated module)
% - `couch_epi:get_value(Handle, Key) - to do efficient matching
%
% The generated module implements clauses like the following
%  - get(couch, {binary_match_rule()}) ->
%       {matched_pattern(), size(matched_pattern()), [flag()]} | undefined
% For example
%  - get(couch, {<<"/shards/test/exact">>}) ->
%        {<<"/shards/test/exact">>,18,[baz,flag_bar,flag_foo]};
%  - get(couch, {<<"/shards/test", _/binary>>}) ->
%        {<<"/shards/test*">>,13,[baz,flag_bar,flag_foo]};
%  - get(couch, {<<"/shards/exact">>}) ->
%        {<<"/shards/exact">>,13,[flag_bar,flag_foo]};
%  - get(couch, {<<"/shards/blacklist", _/binary>>}) ->
%        {<<"/shards/blacklist*">>,18,[]};
%  - get(couch, {<<"/", _/binary>>}) ->
%        {<<"/*">>,2,[flag_foo]};
%  - get(_, _) -> undefined.
%
% The `couch_epi:get/2` uses the Handler module to implement efficient matching.

% In order to distinguish between shards and clustered db the following
% convention is used.
% - it is a shard if pattern starts with `/`

-module(couch_flags).

%% Public API
-export([
    enabled/1,
    is_enabled/2
]).

%% For internal use
-export([
    rules/0
]).

%% For use from plugin
-export([
    subject_key/1
]).

-include_lib("couch/include/couch_db.hrl").
-include_lib("mem3/include/mem3.hrl").
-include("couch_db_int.hrl").

-type subject() ::
    #db{}
    | #httpd{}
    | #shard{}
    | #ordered_shard{}
    | string()
    | binary().

-define(SERVICE_ID, feature_flags).

-spec enabled(subject()) -> [atom()].

enabled(Subject) ->
    Key = maybe_handle(subject_key, [Subject], fun subject_key/1),
    Handle = couch_epi:get_handle({flags, config}),
    lists:usort(
        enabled(Handle, {<<"/", Key/binary>>}) ++
            enabled(Handle, {couch_db:normalize_dbname(Key)})
    ).

-spec is_enabled(FlagId :: atom(), subject()) -> boolean().

is_enabled(FlagId, Subject) ->
    lists:member(FlagId, enabled(Subject)).

-spec rules() ->
    [{Key :: string(), Value :: string()}].

rules() ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    lists:flatten(couch_epi:apply(Handle, ?SERVICE_ID, rules, [], [])).

-spec enabled(Handle :: couch_epi:handle(), Key :: {binary()}) -> [atom()].

enabled(Handle, Key) ->
    case couch_epi:get_value(Handle, couch, Key) of
        {_, _, Flags} -> Flags;
        undefined -> []
    end.

-spec subject_key(subject()) -> binary().

subject_key(#db{name = Name}) ->
    subject_key(Name);
subject_key(#httpd{path_parts = [Name | _Rest]}) ->
    subject_key(Name);
subject_key(#httpd{path_parts = []}) ->
    <<>>;
subject_key(#shard{name = Name}) ->
    subject_key(Name);
subject_key(#ordered_shard{name = Name}) ->
    subject_key(Name);
subject_key(Name) when is_list(Name) ->
    subject_key(list_to_binary(Name));
subject_key(Name) when is_binary(Name) ->
    Name.

-spec maybe_handle(
    Function :: atom(),
    Args :: [term()],
    Default :: fun((Args :: [term()]) -> term())
) ->
    term().

maybe_handle(Func, Args, Default) ->
    Handle = couch_epi:get_handle(?SERVICE_ID),
    case couch_epi:decide(Handle, ?SERVICE_ID, Func, Args, []) of
        no_decision when is_function(Default) ->
            apply(Default, Args);
        {decided, Result} ->
            Result
    end.
