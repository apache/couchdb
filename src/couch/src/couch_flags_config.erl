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

% This module implements {flags, config} data provider
-module(couch_flags_config).

-export([
    enable/2,
    data/0,
    data/1,
    data_provider/0
]).

%% for test suite only
-export([
    parse_flags_term/1
]).

-define(DATA_INTERVAL, 1000).
-define(MAX_FLAG_NAME_LENGTH, 256).

-type pattern() ::
    %% non empty binary which optionally can end with *
    binary().

-type flag_id() :: atom().

-type flags() :: list(flag_id()).

-type parse_pattern() ::
    {
        %% pattern without trainig * if it is present
        binary(),
        pattern(),
        %% true if the pattern has training *
        IsWildCard :: boolean(),
        PatternSize :: pos_integer()
    }.

-type rule() ::
    {
        parse_pattern(),
        EnabledFlags :: flags(),
        DisabledFlags :: flags()
    }.

data_provider() ->
    {
        {flags, config},
        {callback_module, ?MODULE},
        [{interval, ?DATA_INTERVAL}]
    }.

-spec enable(FlagId :: atom(), Pattern :: string()) ->
    ok | {error, Reason :: term()}.

enable(FlagId, Pattern) ->
    Key = atom_to_list(FlagId) ++ "||" ++ Pattern,
    config:set("feature_flags", Key, "true", false).

-spec data() ->
    [{{pattern()}, {pattern(), PatternSize :: pos_integer(), flags()}}].

data() ->
    data(get_config_section("feature_flags") ++ couch_flags:rules()).

-spec data(Rules :: [{Key :: string(), Value :: string()}]) ->
    [{{pattern()}, {pattern(), PatternSize :: pos_integer(), flags()}}].

data(Config) ->
    ByPattern = collect_rules(Config),
    lists:reverse([{{P}, {P, size(P), E -- D}} || {P, {_, E, D}} <- ByPattern]).

-spec parse_rules([{Key :: string(), Value :: string()}]) -> [rule()].

parse_rules(Config) ->
    lists:filtermap(
        fun({K, V}) ->
            case parse_rule(K, V) of
                {error, {Format, Args}} ->
                    couch_log:error(Format, Args),
                    false;
                Rule ->
                    {true, Rule}
            end
        end,
        Config
    ).

-spec parse_rule(Key :: string(), Value :: string()) ->
    rule()
    | {error, Reason :: term()}.

parse_rule(Key, "true") ->
    parse_flags(binary:split(list_to_binary(Key), <<"||">>), true);
parse_rule(Key, "false") ->
    parse_flags(binary:split(list_to_binary(Key), <<"||">>), false);
parse_rule(Key, Value) ->
    Reason = {
        "Expected value for the `~p` either `true` or `false`, (got ~p)",
        [Key, Value]
    },
    {error, Reason}.

-spec parse_flags([binary()], Value :: boolean()) ->
    rule() | {error, Reason :: term()}.

parse_flags([FlagsBin, PatternBin], Value) ->
    case {parse_flags_term(FlagsBin), Value} of
        {{error, _} = Error, _} ->
            Error;
        {Flags, true} ->
            {parse_pattern(PatternBin), Flags, []};
        {Flags, false} ->
            {parse_pattern(PatternBin), [], Flags}
    end;
parse_flags(_Tokens, _) ->
    couch_log:error(
        "Key should be in the form of `[flags]||pattern` (got ~s)", []
    ),
    false.

-spec parse_flags_term(Flags :: binary()) ->
    [flag_id()] | {error, Reason :: term()}.

parse_flags_term(FlagsBin) ->
    {Flags, Errors} = lists:splitwith(
        fun erlang:is_atom/1,
        [parse_flag(F) || F <- split_by_comma(FlagsBin)]
    ),
    case Errors of
        [] ->
            lists:usort(Flags);
        _ ->
            {error, {
                "Cannot parse list of tags: ~n~p",
                Errors
            }}
    end.

split_by_comma(Binary) ->
    case binary:split(Binary, <<",">>, [global]) of
        [<<>>] -> [];
        Tokens -> Tokens
    end.

parse_flag(FlagName) when size(FlagName) > ?MAX_FLAG_NAME_LENGTH ->
    {too_long, FlagName};
parse_flag(FlagName) ->
    FlagNameS = string:strip(binary_to_list(FlagName)),
    try
        list_to_existing_atom(FlagNameS)
    catch
        _:_ -> {invalid_flag, FlagName}
    end.

-spec parse_pattern(Pattern :: binary()) -> parse_pattern().

parse_pattern(PatternBin) ->
    PatternSize = size(PatternBin),
    case binary:last(PatternBin) of
        $* ->
            PrefixBin = binary:part(PatternBin, 0, PatternSize - 1),
            {PrefixBin, PatternBin, true, PatternSize - 1};
        _ ->
            {PatternBin, PatternBin, false, PatternSize}
    end.

-spec collect_rules([{ConfigurationKey :: string(), ConfigurationValue :: string()}]) ->
    [{pattern(), rule()}].

collect_rules(ConfigData) ->
    ByKey = by_key(parse_rules(ConfigData)),
    Keys = lists:sort(fun sort_by_length/2, gb_trees:keys(ByKey)),
    FuzzyKeys = lists:sort(
        fun sort_by_length/2,
        [K || {K, {{_, _, true, _}, _, _}} <- gb_trees:to_list(ByKey)]
    ),
    Rules = collect_rules(lists:reverse(Keys), FuzzyKeys, ByKey),
    gb_trees:to_list(Rules).

-spec sort_by_length(A :: binary(), B :: binary()) -> boolean().

sort_by_length(A, B) ->
    size(A) =< size(B).

-spec by_key(Items :: [rule()]) -> Dictionary :: gb_trees:tree().

by_key(Items) ->
    lists:foldl(
        fun({{_, K, _, _}, _, _} = Item, Acc) ->
            update_element(Acc, K, Item, fun(Value) ->
                update_flags(Value, Item)
            end)
        end,
        gb_trees:empty(),
        Items
    ).

-spec update_element(
    Tree :: gb_trees:tree(),
    Key :: pattern(),
    Default :: rule(),
    Fun :: fun((Item :: rule()) -> rule())
) ->
    gb_trees:tree().

update_element(Tree, Key, Default, Fun) ->
    case gb_trees:lookup(Key, Tree) of
        none ->
            gb_trees:insert(Key, Default, Tree);
        {value, Value} ->
            gb_trees:update(Key, Fun(Value), Tree)
    end.

-spec collect_rules(
    Keys :: [pattern()],
    FuzzyKeys :: [pattern()],
    ByKey :: gb_trees:tree()
) ->
    gb_trees:tree().

collect_rules([], _, Acc) ->
    Acc;
collect_rules([Current | Rest], Items, Acc) ->
    collect_rules(Rest, Items -- [Current], inherit_flags(Current, Items, Acc)).

-spec inherit_flags(
    Current :: pattern(),
    FuzzyKeys :: [pattern()],
    ByKey :: gb_trees:tree()
) ->
    gb_trees:tree().

inherit_flags(_Current, [], Acc) ->
    Acc;
inherit_flags(Current, [Item | Items], Acc) ->
    case match_prefix(Current, Item, Acc) of
        true ->
            inherit_flags(Current, Items, update_flags(Current, Item, Acc));
        false ->
            inherit_flags(Current, Items, Acc)
    end.

-spec match_prefix(
    AKey :: pattern(),
    BKey :: pattern(),
    ByKey :: gb_trees:tree()
) ->
    boolean().

match_prefix(AKey, BKey, Acc) ->
    {value, A} = gb_trees:lookup(AKey, Acc),
    {value, B} = gb_trees:lookup(BKey, Acc),
    match_prefix(A, B).

-spec match_prefix(A :: rule(), B :: rule()) -> boolean().

match_prefix({{_, _, _, _}, _, _}, {{_, _, false, _}, _, _}) ->
    false;
match_prefix({{Key, _, _, _}, _, _}, {{Key, _, true, _}, _, _}) ->
    true;
match_prefix({{Key0, _, _, _}, _, _}, {{Key1, _, true, S1}, _, _}) ->
    case Key0 of
        <<Key1:S1/binary, _/binary>> -> true;
        _ -> false
    end.

-spec update_flags(
    AKey :: pattern(),
    BKey :: pattern(),
    ByKey :: gb_trees:tree()
) ->
    gb_trees:tree().

update_flags(AKey, BKey, Acc) ->
    {value, A} = gb_trees:lookup(AKey, Acc),
    {value, B} = gb_trees:lookup(BKey, Acc),
    gb_trees:update(AKey, update_flags(A, B), Acc).

-spec update_flags(A :: rule(), B :: rule()) -> rule().

update_flags({Pattern, E0, D0}, {_, E1, D1}) ->
    DisabledByParent = lists:usort(D1 -- E0),
    E = lists:usort(lists:usort(E0 ++ E1) -- D0),
    D = lists:usort(D0 ++ DisabledByParent),
    {Pattern, E, D}.

-spec get_config_section(Section :: string()) ->
    [{Key :: string(), Value :: string()}].

%% When we start couch_epi the config is not started yet
% so we would get `badarg` for some time
get_config_section(Section) ->
    try
        config:get(Section)
    catch
        error:badarg ->
            []
    end.
