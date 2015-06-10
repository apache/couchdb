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

-module(couch_epi_functions_gen).

-export([add/3, remove/3, get_handle/1, hash/1, apply/4, apply/5]).

-ifdef(TEST).

-export([foo/2, bar/0]).

-endif.

-record(opts, {
    ignore_errors = false,
    ignore_providers = false,
    pipe = false,
    concurrent = false
}).

add(Handle, Source, Modules) ->
    case is_updated(Handle, Source, Modules) of
        false ->
            ok;
        true ->
            save(Handle, Source, Modules)
    end.

remove(Handle, Source, Modules) ->
    CurrentDefs = get_current_definitions(Handle),
    {SourceDefs, Defs} = remove_from_definitions(CurrentDefs, Source),

    NewSourceDefs = lists:filter(fun({M, _}) ->
        not lists:member(M, Modules)
    end, SourceDefs),

    generate(Handle, Defs ++ NewSourceDefs).

get_handle(ServiceId) ->
    module_name(atom_to_list(ServiceId)).

apply(ServiceId, Function, Args, Opts) when is_atom(ServiceId) ->
    apply(get_handle(ServiceId), ServiceId, Function, Args, Opts).

-spec apply(Handle :: atom(), ServiceId :: atom(), Function :: atom(),
    Args :: [term()], Opts :: couch_epi:apply_opts()) -> ok.

apply(Handle, _ServiceId, Function, Args, Opts) ->
    DispatchOpts = parse_opts(Opts),
    Modules = providers(Handle, Function, length(Args), DispatchOpts),
    dispatch(Handle, Modules, Function, Args, DispatchOpts).


%% ------------------------------------------------------------------
%% Codegeneration routines
%% ------------------------------------------------------------------

preamble() ->
    "
    -export([version/0, version/1]).
    -export([providers/0, providers/2]).
    -export([definitions/0, definitions/1]).
    -export([dispatch/3]).
    -export([callbacks/2]).

    version() ->
        [{Provider, version(Provider)} || Provider <- providers()].

    definitions() ->
        [{Provider, definitions(Provider)} || Provider <- providers()].

    callbacks(Provider, Function) ->
        [].

    "
    %% In addition to preamble we also generate following methods
    %% dispatch(Module, Function, [A1, A2]) -> Module:Function(A1, A2);

    %% version(Source1) -> "HASH";
    %% version(Source) -> {error, {unknown, Source}}.

    %% providers() -> [].
    %% providers(Function, Arity) -> [].
    %% definitions(Provider) -> [{Module, [{Fun, Arity}]}].
    .

generate(Handle, Defs) ->
    DispatchFunForms = couch_epi_codegen:function(dispatchers(Defs)),
    VersionFunForms = couch_epi_codegen:function(version_method(Defs)),

    AllProvidersForms = all_providers_method(Defs),
    ProvidersForms = couch_epi_codegen:function(providers_method(Defs)),
    DefinitionsForms = couch_epi_codegen:function(definitions_method(Defs)),

    Forms = couch_epi_codegen:scan(preamble())
        ++ DispatchFunForms ++ VersionFunForms
        ++ ProvidersForms ++ AllProvidersForms
        ++ DefinitionsForms,

    couch_epi_codegen:generate(Handle, Forms).

all_providers_method(Defs) ->
    Providers = couch_epi_codegen:format_term(defined_providers(Defs)),
    couch_epi_codegen:scan("providers() -> " ++ Providers ++ ".").

providers_method(Defs) ->
    Providers = providers_by_function(Defs),
    DefaultClause = "providers(_, _) -> [].",
    lists:foldl(fun({{Fun, Arity}, Modules}, Clauses) ->
        providers(Fun, Arity, Modules) ++ Clauses
    end, [couch_epi_codegen:scan(DefaultClause)], Providers).

providers(Function, Arity, Modules) ->
    ArityStr = integer_to_list(Arity),
    Mods = couch_epi_codegen:format_term(Modules),
    Fun = atom_to_list(Function),
    %% providers(Function, Arity) -> [Module];
    couch_epi_codegen:scan(
        "providers(" ++ Fun ++ "," ++ ArityStr ++ ") ->" ++ Mods ++ ";").

dispatchers(Defs) ->
    DefaultClause = "dispatch(_Module, _Fun, _Args) -> ok.",
    fold_defs(Defs, [couch_epi_codegen:scan(DefaultClause)],
        fun({_Source, Module, Function, Arity}, Acc) ->
            dispatcher(Module, Function, Arity) ++ Acc
        end).

version_method(Defs) ->
    DefaultClause = "version(S) -> {error, {unknown, S}}.",
    lists:foldl(fun({Source, SrcDefs}, Clauses) ->
        version(Source, SrcDefs) ++ Clauses
    end, [couch_epi_codegen:scan(DefaultClause)], Defs).

definitions_method(Defs) ->
    DefaultClause = "definitions(S) -> {error, {unknown, S}}.",
    lists:foldl(fun({Source, SrcDefs}, Clauses) ->
        definition(Source, SrcDefs) ++ Clauses
    end, [couch_epi_codegen:scan(DefaultClause)], Defs).

definition(Source, Defs) ->
    Src = atom_to_list(Source),
    DefsStr = couch_epi_codegen:format_term(Defs),
    couch_epi_codegen:scan("definitions(" ++ Src ++ ") -> " ++ DefsStr ++ ";").

dispatcher(Module, Function, 0) ->
    M = atom_to_list(Module),
    Fun = atom_to_list(Function),

    %% dispatch(Module, Function, []) -> Module:Function();
    couch_epi_codegen:scan(
        "dispatch(" ++ M ++ "," ++ Fun ++ ", []) ->"
            ++ M ++ ":" ++ Fun ++ "();");
dispatcher(Module, Function, Arity) ->
    Args = args_string(Arity),
    M = atom_to_list(Module),
    Fun = atom_to_list(Function),
    %% dispatch(Module, Function, [A1, A2]) -> Module:Function(A1, A2);
    couch_epi_codegen:scan(
        "dispatch(" ++ M ++ "," ++ Fun ++ ", [" ++ Args ++ "]) ->"
            ++ M ++ ":" ++ Fun ++ "(" ++ Args ++ ");").

args_string(Arity) ->
    Vars = ["A" ++ integer_to_list(Seq)  || Seq <- lists:seq(1, Arity)],
    string:join(Vars, ", ").

version(Source, SrcDefs) ->
    Modules = [Module || {Module, _Exports} <- SrcDefs],
    couch_epi_codegen:scan(
        "version(" ++ atom_to_list(Source) ++ ") ->" ++ hash(Modules) ++ ";").



%% ------------------------------------------------------------------
%% Helper functions
%% ------------------------------------------------------------------

module_name(ServiceId) when is_list(ServiceId) ->
    list_to_atom(string:join([atom_to_list(?MODULE), ServiceId], "_")).

is_updated(Handle, Source, Modules) ->
    Sig = hash(Modules),
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

save(Handle, Source, Modules) ->
    CurrentDefs = get_current_definitions(Handle),
    Definitions = definitions(Source, Modules),
    NewDefs = lists:keystore(Source, 1, CurrentDefs, Definitions),
    generate(Handle, NewDefs).

definitions(Source, Modules) ->
    Blacklist = [{module_info, 0}, {module_info, 1}],
    SrcDefs = [{M, M:module_info(exports) -- Blacklist} || M <- Modules],
    {Source, SrcDefs}.

get_current_definitions(Handle) ->
    try Handle:definitions()
    catch error:undef -> []
    end.

defined_providers(Defs) ->
    [Source || {Source, _} <- Defs].

%% Defs = [{Source, [{Module, [{Fun, Arity}]}]}]
fold_defs(Defs, Acc, Fun) ->
    lists:foldl(fun({Source, SourceData}, Clauses) ->
        lists:foldl(fun({Module, Exports}, ExportsAcc) ->
            lists:foldl(fun({Function, Arity}, InAcc) ->
                Fun({Source, Module, Function, Arity}, InAcc)
            end, [], Exports) ++ ExportsAcc
        end, [], SourceData) ++ Clauses
    end, Acc, Defs).

providers_by_function(Defs) ->
    Providers = fold_defs(Defs, [],
        fun({_Source, Module, Function, Arity}, Acc) ->
            [{{Function, Arity}, Module} | Acc]
        end
    ),
    Dict = lists:foldl(fun({K, V}, Acc) ->
        dict:append(K, V, Acc)
    end, dict:new(), Providers),
    dict:to_list(Dict).


hash(Modules) ->
    VSNs = [couch_epi_util:module_version(M) || M <- lists:usort(Modules)],
    couch_epi_util:hash(VSNs).

dispatch(_Handle, _Modules, _Func, _Args, #opts{concurrent = true, pipe = true}) ->
    throw({error, {incompatible_options, [concurrent, pipe]}});
dispatch(Handle, Modules, Function, Args,
        #opts{pipe = true, ignore_errors = true}) ->
    lists:foldl(fun(Module, Acc) ->
        try
            Handle:dispatch(Module, Function, Acc)
        catch _:_ ->
            Acc
        end
    end, Args, Modules);
dispatch(Handle, Modules, Function, Args,
        #opts{pipe = true}) ->
    lists:foldl(fun(Module, Acc) ->
        Handle:dispatch(Module, Function, Acc)
    end, Args, Modules);
dispatch(Handle, Modules, Function, Args, #opts{} = Opts) ->
    [do_dispatch(Handle, Module, Function, Args, Opts) || Module <- Modules].

do_dispatch(Handle, Module, Function, Args,
        #opts{concurrent = true, ignore_errors = true}) ->
    spawn(fun() ->
        (catch Handle:dispatch(Module, Function, Args))
    end);
do_dispatch(Handle, Module, Function, Args,
        #opts{ignore_errors = true}) ->
    (catch Handle:dispatch(Module, Function, Args));
do_dispatch(Handle, Module, Function, Args,
        #opts{concurrent = true}) ->
    spawn(fun() -> Handle:dispatch(Module, Function, Args) end);
do_dispatch(Handle, Module, Function, Args, #opts{}) ->
    Handle:dispatch(Module, Function, Args).


parse_opts(Opts) ->
    parse_opts(Opts, #opts{}).

parse_opts([ignore_errors|Rest], #opts{} = Acc) ->
    parse_opts(Rest, Acc#opts{ignore_errors = true});
parse_opts([pipe|Rest], #opts{} = Acc) ->
    parse_opts(Rest, Acc#opts{pipe = true});
parse_opts([concurrent|Rest], #opts{} = Acc) ->
    parse_opts(Rest, Acc#opts{concurrent = true});
parse_opts([ignore_providers|Rest], #opts{} = Acc) ->
    parse_opts(Rest, Acc#opts{ignore_providers = true});
parse_opts([], Acc) ->
    Acc.

providers(Handle, Function, Arity, #opts{ignore_providers = true}) ->
    try
        Handle:providers(Function, Arity)
    catch
        error:undef -> []
    end;
providers(Handle, Function, Arity, #opts{}) ->
    Handle:providers(Function, Arity).

remove_from_definitions(Defs, Source) ->
    case lists:keytake(Source, 1, Defs) of
        {value, {Source, Value}, Rest} ->
            {Value, Rest};
        false ->
            {[], Defs}
    end.

%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

foo(A1, A2) ->
    {A1, A2}.

bar() ->
    [].

basic_test() ->
    Module = foo_bar_dispatcher,
    add(Module, app1, [?MODULE]),

    ?assert(is_list(Module:version(app1))),

    Defs1 = lists:usort(Module:definitions()),
    ?assertMatch([{app1, [{?MODULE, _}]}], Defs1),
    [{app1, [{?MODULE, Exports}]}] = Defs1,
    ?assert(lists:member({bar, 0}, Exports)),

    add(Module, app2, [?MODULE]),
    Defs2 = lists:usort(Module:definitions()),
    ?assertMatch([{app1, [{?MODULE, _}]}, {app2, [{?MODULE, _}]}], Defs2),

    ?assertMatch([{app1, Hash}, {app2, Hash}], Module:version()),

    ?assertMatch([], Module:dispatch(?MODULE, bar, [])),
    ?assertMatch({1, 2}, Module:dispatch(?MODULE, foo, [1, 2])),

    ok.

-endif.
