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

-module(mem3_hash).

-export([
    calculate/2,

    get_hash_fun/1,

    crc32/1
]).

-include_lib("mem3/include/mem3.hrl").

calculate(#shard{opts = Opts}, DocId) ->
    Props = couch_util:get_value(props, Opts, []),
    MFA = get_hash_fun_int(Props),
    calculate(MFA, DocId);
calculate(#ordered_shard{opts = Opts}, DocId) ->
    Props = couch_util:get_value(props, Opts, []),
    MFA = get_hash_fun_int(Props),
    calculate(MFA, DocId);
calculate(DbName, DocId) when is_binary(DbName) ->
    MFA = get_hash_fun(DbName),
    calculate(MFA, DocId);
calculate({Mod, Fun, Args}, DocId) ->
    erlang:apply(Mod, Fun, [DocId | Args]).

get_hash_fun(#shard{opts = Opts}) ->
    get_hash_fun_int(Opts);
get_hash_fun(#ordered_shard{opts = Opts}) ->
    get_hash_fun_int(Opts);
get_hash_fun(DbName0) when is_binary(DbName0) ->
    DbName = mem3:dbname(DbName0),
    try
        [#shard{opts = Opts} | _] = mem3_shards:for_db(DbName),
        get_hash_fun_int(couch_util:get_value(props, Opts, []))
    catch
        error:database_does_not_exist ->
            {?MODULE, crc32, []}
    end.

crc32(Item) when is_binary(Item) ->
    erlang:crc32(Item);
crc32(Item) ->
    erlang:crc32(term_to_binary(Item)).

get_hash_fun_int(Opts) when is_list(Opts) ->
    case lists:keyfind(hash, 1, Opts) of
        {hash, [Mod, Fun, Args]} ->
            {Mod, Fun, Args};
        _ ->
            {?MODULE, crc32, []}
    end.
