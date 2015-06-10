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

-module(couch_epi_data_gen).

%% @doc
%% We generate and compile module with name constructed as:
%%   "couch_epi_data_" + Service + "_" + Key
%% To get an idea about he code of the generated module see preamble()

-export([get_handle/1]).
-export([set/3, get/1, get/2, get/3]).
-export([by_key/1, by_key/2]).
-export([by_source/1, by_source/2]).
-export([keys/1, subscribers/1]).

set(Handle, Source, Data) ->
    case is_updated(Handle, Source, Data) of
        false ->
            ok;
        true ->
            save(Handle, Source, Data)
    end.

get(Handle) ->
    Handle:all().

get(Handle, Key) ->
    Handle:all(Key).

get(Handle, Source, Key) ->
    Handle:get(Source, Key).

by_key(Handle) ->
    Handle:by_key().

by_key(Handle, Key) ->
    Handle:by_key(Key).

by_source(Handle) ->
    Handle:by_source().

by_source(Handle, Source) ->
    Handle:by_source(Source).

keys(Handle) ->
    Handle:keys().

subscribers(Handle) ->
    Handle:subscribers().

get_handle({Service, Key}) ->
    module_name({atom_to_list(Service), atom_to_list(Key)}).

%% ------------------------------------------------------------------
%% Codegeneration routines
%% ------------------------------------------------------------------

preamble() ->
    "
    -export([by_key/0, by_key/1]).
    -export([by_source/0, by_source/1]).
    -export([all/0, all/1, get/2]).
    -export([version/0, version/1]).
    -export([keys/0, subscribers/0]).
    -compile({no_auto_import,[get/0, get/1]}).
    all() ->
        lists:foldl(fun({Key, Defs}, Acc) ->
           [D || {_Subscriber, D} <- Defs ] ++ Acc
        end, [], by_key()).

    all(Key) ->
        lists:foldl(fun({Subscriber, Data}, Acc) ->
           [Data | Acc]
        end, [], by_key(Key)).

    by_key() ->
        [{Key, by_key(Key)} || Key <- keys()].

    by_key(Key) ->
        lists:foldl(
            fun(Source, Acc) -> append_if_defined(Source, get(Source, Key), Acc)
        end, [], subscribers()).


    by_source() ->
        [{Source, by_source(Source)} || Source <- subscribers()].

    by_source(Source) ->
        lists:foldl(
            fun(Key, Acc) -> append_if_defined(Key, get(Source, Key), Acc)
        end, [], keys()).

    version() ->
        [{Subscriber, version(Subscriber)} || Subscriber <- subscribers()].

    %% Helper functions
    append_if_defined(Type, undefined, Acc) -> Acc;
    append_if_defined(Type, Value, Acc) -> [{Type, Value} | Acc].
    "
    %% In addition to preamble we also generate following methods
    %% get(Source1, Key1) -> Data;
    %% get(Source, Key) -> undefined.

    %% version(Source1) -> "HASH";
    %% version(Source) -> {error, {unknown, Source}}.

    %% keys() -> [].
    %% subscribers() -> [].
    .

generate(Handle, Defs) ->
    GetFunForms = couch_epi_codegen:function(getters(Defs)),
    VersionFunForms = couch_epi_codegen:function(version_method(Defs)),
    KeysForms = keys_method(Defs),
    SubscribersForms = subscribers_method(Defs),

    Forms = couch_epi_codegen:scan(preamble())
        ++ GetFunForms ++ VersionFunForms
        ++ KeysForms ++ SubscribersForms,

    couch_epi_codegen:generate(Handle, Forms).

keys_method(Defs) ->
    Keys = couch_epi_codegen:format_term(defined_keys(Defs)),
    couch_epi_codegen:scan("keys() -> " ++ Keys ++ ".").

subscribers_method(Defs) ->
    Subscribers = couch_epi_codegen:format_term(defined_subscribers(Defs)),
    couch_epi_codegen:scan("subscribers() -> " ++ Subscribers ++ ".").

getters(Defs) ->
    DefaultClause = "get(_S, _K) -> undefined.",
    fold_defs(Defs, [couch_epi_codegen:scan(DefaultClause)],
        fun({Source, Key, Data}, Acc) ->
            getter(Source, Key, Data) ++ Acc
        end).

version_method(Defs) ->
    DefaultClause = "version(S) -> {error, {unknown, S}}.",
    lists:foldl(fun({Source, Data}, Clauses) ->
        version(Source, Data) ++ Clauses
    end, [couch_epi_codegen:scan(DefaultClause)], Defs).

getter(Source, Key, Data) ->
    D = couch_epi_codegen:format_term(Data),
    Src = atom_to_list(Source),
    K = couch_epi_codegen:format_term(Key),
    couch_epi_codegen:scan(
        "get(" ++ Src ++ ", " ++ K ++ ") ->" ++ D ++ ";").

version(Source, Data) ->
    Src = atom_to_list(Source),
    VSN = couch_epi_util:hash(Data),
    couch_epi_codegen:scan("version(" ++ Src ++ ") ->" ++ VSN ++ ";").

%% ------------------------------------------------------------------
%% Helper functions
%% ------------------------------------------------------------------

module_name({Service, Key}) when is_list(Service) andalso is_list(Key) ->
    list_to_atom(string:join([atom_to_list(?MODULE), Service, Key], "_")).

is_updated(Handle, Source, Data) ->
    Sig = couch_epi_util:hash(Data),
    try Handle:version(Source) of
        {error, {unknown, Source}} -> true;
        {error, Reason} -> throw(Reason);
        Sig -> false;
        _ -> true
    catch
        error:undef -> true;
        Class:Reason ->
            throw({Class, {Source, Reason}})
    end.

save(Handle, Source, Data) ->
    CurrentData = get_current_data(Handle),
    NewDefs = lists:keystore(Source, 1, CurrentData, {Source, Data}),
    generate(Handle, NewDefs).

get_current_data(Handle) ->
    try Handle:by_source()
    catch error:undef -> []
    end.


defined_keys(Defs) ->
    Keys = fold_defs(Defs, [], fun({_Source, Key, _Data}, Acc) ->
        [Key | Acc]
    end),
    lists:usort(Keys).

defined_subscribers(Defs) ->
    [Source || {Source, _} <- Defs].

fold_defs(Defs, Acc, Fun) ->
    lists:foldl(fun({Source, SourceData}, Clauses) ->
        lists:foldl(fun({Key, Data}, InAcc) ->
            Fun({Source, Key, Data}, InAcc)
        end, [], SourceData) ++ Clauses
    end, Acc, Defs).

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

basic_test() ->
    Module = foo_bar_baz_bugz,
    Data1 = [some_nice_data],
    Data2 = "other data",
    Data3 = {"even more data"},
    Defs1 = [{foo, Data1}],
    Defs2 = lists:usort([{foo, Data2}, {bar, Data3}]),

    set(Module, app1, Defs1),
    set(Module, app2, Defs2),

    ?assertEqual([bar, foo], lists:usort(Module:keys())),
    ?assertEqual([app1, app2], lists:usort(Module:subscribers())),

    ?assertEqual(Data1, Module:get(app1, foo)),
    ?assertEqual(Data2, Module:get(app2, foo)),
    ?assertEqual(Data3, Module:get(app2, bar)),

    ?assertEqual(undefined, Module:get(bad, key)),
    ?assertEqual(undefined, Module:get(source, bad)),

    ?assertEqual("3KZ4EG4WBF4J683W8GSDDPYR3", Module:version(app1)),
    ?assertEqual("4EFUU47W9XDNMV9RMZSSJQU3Y", Module:version(app2)),

    ?assertEqual({error,{unknown,bad}}, Module:version(bad)),

    ?assertEqual(
        [{app1,"3KZ4EG4WBF4J683W8GSDDPYR3"},
         {app2,"4EFUU47W9XDNMV9RMZSSJQU3Y"}], lists:usort(Module:version())),

    ?assertEqual(
       [{app1,[some_nice_data]},{app2,"other data"}],
       lists:usort(Module:by_key(foo))),

    ?assertEqual([], lists:usort(Module:by_key(bad))),

    ?assertEqual(
        [
            {bar, [{app2, {"even more data"}}]},
            {foo, [{app2, "other data"}, {app1, [some_nice_data]}]}
        ],
        lists:usort(Module:by_key())),


    ?assertEqual(Defs1, lists:usort(Module:by_source(app1))),
    ?assertEqual(Defs2, lists:usort(Module:by_source(app2))),

    ?assertEqual([], lists:usort(Module:by_source(bad))),

    ?assertEqual(
        [
            {app1, [{foo, [some_nice_data]}]},
            {app2, [{foo, "other data"}, {bar, {"even more data"}}]}
        ],
        lists:usort(Module:by_source())),

    ?assertEqual(
        lists:usort([Data1, Data2, Data3]), lists:usort(Module:all())),
    ?assertEqual(lists:usort([Data1, Data2]), lists:usort(Module:all(foo))),
    ?assertEqual([], lists:usort(Module:all(bad))),

    ok.

-endif.
