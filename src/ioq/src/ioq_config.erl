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

-module(ioq_config).


-include_lib("mem3/include/mem3.hrl").
-include_lib("ioq/include/ioq.hrl").


-export([
    build_shard_priorities/0,
    build_shard_priorities/1,
    build_user_priorities/0,
    build_user_priorities/1,
    build_class_priorities/0,
    build_class_priorities/1,
    add_default_class_priorities/1,
    to_float/1,
    to_float/2,
    parse_shard_string/1,
    ioq_classes/0,
    is_valid_class/1
]).
-export([
    prioritize/4,
    check_priority/3
]).
-export([
    set_db_config/4,
    set_shards_config/4,
    set_shard_config/4,
    set_class_config/3,
    set_user_config/3
]).
-export([
    set_bypass/3,
    set_enabled/2,
    set_max_priority/2,
    set_dedupe/2,
    set_scale_factor/2,
    set_resize_limit/2,
    set_concurrency/2,
    set_dispatch_strategy/2
]).


ioq_classes() ->
    [Class || {Class, _Priority} <- ?DEFAULT_CLASS_PRIORITIES].


set_bypass(Class, Value, Reason) when is_atom(Class), is_boolean(Value) ->
    true = is_valid_class(Class),
    set_config(?IOQ2_BYPASS_CONFIG, atom_to_list(Class), atom_to_list(Value), Reason).


set_enabled(Value, Reason) when is_boolean(Value) ->
    set_config(?IOQ2_CONFIG, "enabled", atom_to_list(Value), Reason).


set_max_priority(Value, Reason) when is_float(Value) ->
    set_config(?IOQ2_CONFIG, "max_priority", Value, Reason).


set_dedupe(Value, Reason) when is_boolean(Value) ->
    set_config(?IOQ2_CONFIG, "dedupe", atom_to_list(Value), Reason).


set_scale_factor(Value, Reason) when is_float(Value) ->
    set_config(?IOQ2_CONFIG, "scale_factor", float_to_list(Value), Reason).


set_resize_limit(Value, Reason) when is_integer(Value) ->
    set_config(?IOQ2_CONFIG, "resize_limit", integer_to_list(Value), Reason).


set_concurrency(Value, Reason) when is_integer(Value) ->
    set_config(?IOQ2_CONFIG, "concurrency", integer_to_list(Value), Reason).


set_dispatch_strategy(Value, Reason) ->
    ErrorMsg = "Dispatch strategy must be one of "
        "random, fd_hash, server_per_scheduler, or single_server.",
    ok = case Value of
        ?DISPATCH_RANDOM               -> ok;
        ?DISPATCH_FD_HASH              -> ok;
        ?DISPATCH_SINGLE_SERVER        -> ok;
        ?DISPATCH_SERVER_PER_SCHEDULER -> ok;
        _                              -> throw({badarg, ErrorMsg})
    end,
    config:set(?IOQ2_CONFIG, "dispatch_strategy", Value, Reason).


set_db_config(DbName, Class, Value, Reason) when is_binary(DbName) ->
    ok = check_float_value(Value),
    ok = set_shards_config(mem3:shards(DbName), Class, Value, Reason).


set_shards_config(Shards, Class, Value, Reason) ->
    ok = check_float_value(Value),
    ok = lists:foreach(fun(Shard) ->
        ok = set_shard_config(Shard, Class, Value, Reason)
    end, Shards).


set_shard_config(#shard{name=Name0}, Class0, Value, Reason) when is_atom(Class0) ->
    ok = check_float_value(Value),
    true = is_valid_class(Class0),
    Name = binary_to_list(filename:rootname(Name0)),
    Class = atom_to_list(Class0),
    ConfigName = Name ++ ?SHARD_CLASS_SEPARATOR ++ Class,
    ok = set_config(?IOQ2_SHARDS_CONFIG, ConfigName, Value, Reason).


set_class_config(Class, Value, Reason) when is_atom(Class)->
    ok = check_float_value(Value),
    true = is_valid_class(Class),
    ok = set_config(?IOQ2_CLASSES_CONFIG, atom_to_list(Class), Value, Reason).


set_user_config(User, Value, Reason) when is_binary(User) ->
    set_user_config(binary_to_list(User), Value, Reason);
set_user_config(User, Value, Reason) ->
    ok = check_float_value(Value),
    %% TODO: validate User exists (how to do this without a Req?)
    ok = set_config(?IOQ2_USERS_CONFIG, User, Value, Reason).


is_valid_class(Class) when is_atom(Class) ->
    case lists:member(Class, ioq_classes()) of
        true ->
            true;
        false ->
            SClass = atom_to_list(Class),
            case config:get(?IOQ2_CLASSES_CONFIG, SClass, undefined) of
                undefined ->
                    false;
                _ ->
                    true
            end
    end.


check_float_value(Value) when is_float(Value) ->
    ok;
check_float_value(_) ->
    erlang:error({badarg, invalid_float_value}).


set_config(Section, Key, Value, Reason) when is_float(Value) ->
    set_config(Section, Key, float_to_list(Value), Reason);
set_config(Section, Key, Value, Reason) when is_binary(Key) ->
    set_config(Section, binary_to_list(Key), Value, Reason);
set_config(Section, Key, Value, Reason) ->
    ok = config:set(Section, Key, Value, Reason).


-spec build_shard_priorities() -> {ok, khash:khash()}.
build_shard_priorities() ->
    Configs = lists:foldl(
        fun({Key0, Val}, Acc) ->
            case parse_shard_string(Key0) of
                {error, ShardString} ->
                    couch_log:error(
                        "IOQ error parsing shard config: ~p",
                        [ShardString]
                    ),
                    Acc;
                Key ->
                    [{Key, to_float(Val)} | Acc]
            end
        end,
        [],
        config:get("ioq2.shards")
    ),
    build_shard_priorities(Configs).


-spec build_shard_priorities([{any(), float()}]) -> {ok, khash:khash()}.
build_shard_priorities(Configs) ->
    init_config_priorities(Configs).


-spec build_user_priorities() -> {ok, khash:khash()}.
build_user_priorities() ->
    build_user_priorities(config:get("ioq2.users")).


-spec build_user_priorities([{any(), float()}]) -> {ok, khash:khash()}.
build_user_priorities(Configs0) ->
    Configs = [{list_to_binary(K), to_float(V)} || {K,V} <- Configs0],
    init_config_priorities(Configs).


-spec build_class_priorities() -> {ok, khash:khash()}.
build_class_priorities() ->
    build_class_priorities(config:get("ioq2.classes")).


-spec build_class_priorities([{any(), float()}]) -> {ok, khash:khash()}.
build_class_priorities(Configs0) ->
    {ok, ClassP} = khash:new(),
    ok = add_default_class_priorities(ClassP),
    Configs = [{list_to_existing_atom(K), to_float(V)} || {K,V} <- Configs0],
    init_config_priorities(Configs, ClassP).


-spec parse_shard_string(string()) -> {binary(), atom()}
    | {error, string()}.
parse_shard_string(ShardString) ->
    case string:tokens(ShardString, ?SHARD_CLASS_SEPARATOR) of
        [Shard, Class] ->
            {list_to_binary(Shard), list_to_existing_atom(Class)};
        _ ->
            {error, ShardString}
    end.


-spec add_default_class_priorities(khash:khash()) -> ok.
add_default_class_priorities(ClassP) ->
    ok = lists:foreach(
        fun({Class, Priority}) ->
            ok = khash:put(ClassP, Class, Priority)
        end,
        ?DEFAULT_CLASS_PRIORITIES
    ).


-spec to_float(any()) -> float().
to_float(V) ->
    to_float(V, ?DEFAULT_PRIORITY).


-spec to_float(any(), float()) -> float().
to_float(Float, _) when is_float(Float) ->
    Float;
to_float(Int, _) when is_integer(Int) ->
    float(Int);
to_float(String, Default) when is_list(String) ->
    try
        list_to_float(String)
    catch error:badarg ->
        try
            to_float(list_to_integer(String))
        catch error:badarg ->
            Default
        end
    end;
to_float(_, Default) ->
    Default.


-spec prioritize(ioq_request(), khash:khash(), khash:khash(), khash:khash()) ->
    float().
prioritize(#ioq_request{} = Req, ClassP, UserP, ShardP) ->
    #ioq_request{
        user=User,
        shard=Shard,
        class=Class
    } = Req,
    UP = get_priority(UserP, User),
    CP = get_priority(ClassP, Class),
    SP = get_priority(ShardP, {Shard, Class}),
    UP * CP * SP.


-spec init_config_priorities([{any(), float()}]) -> {ok, khash:khash()}.
init_config_priorities(Configs) ->
    {ok, Hash} = khash:new(),
    init_config_priorities(Configs, Hash).


-spec init_config_priorities([{any(), float()}], khash:khash()) ->
    {ok, khash:khash()}.
init_config_priorities(Configs, Hash) ->
    ok = lists:foreach(
        fun({Key, Val}) ->
            ok = khash:put(Hash, Key, Val)
        end,
        Configs
    ),
    {ok, Hash}.


-spec check_priority(atom(), binary(), binary()) -> float().
check_priority(Class, User, Shard0) ->
    {ok, ClassP} = build_class_priorities(),
    {ok, UserP} = build_user_priorities(),
    {ok, ShardP} = build_shard_priorities(),

    Shard = filename:rootname(Shard0),
    Req = #ioq_request{
        user = User,
        shard = Shard,
        class = Class
    },

    prioritize(Req, ClassP, UserP, ShardP).


get_priority(KH, Key) ->
    get_priority(KH, Key, ?DEFAULT_PRIORITY).


get_priority(_KH, undefined, Default) ->
    Default;
get_priority(KH, Key, Default) ->
    khash:get(KH, Key, Default).
