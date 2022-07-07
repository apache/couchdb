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

%% Based on Bob Ippolitto's mochiglobal.erl

%%
-module(ioq_kv).
-export([init/0]).
-export([all/0, get/1, get/2, put/2, delete/1]).
-export([validate_term/1]).

-define(DYNMOD, ioq_kv_dyn).
-define(ERLFILE, "ioq_kv_dyn.erl").

-spec init() -> ok.
%% @doc Initialize the dynamic module
init() ->
    compile(all()).

-spec all() -> [{any(), any()}].
%% @doc Get the list of Key/Val pairs stored
all() ->
    try
        ?DYNMOD:list()
    catch error:undef ->
        []
    end.

-spec get(any()) -> any() | undefined.
%% @equiv get(Key, undefined)
get(Key) ->
    get(Key, undefined).

-spec get(any(), T) -> any() | T.
%% @doc Get the term for Key or return Default.
get(Key, Default) ->
    try
        ?DYNMOD:lookup(Key, Default)
    catch error:undef ->
        Default
    end.

-spec put(any(), any()) -> ok.
%% @doc Store term Val at Key, replaces an existing term if present.
put(Key, Val) ->
    KVs = proplists:delete(Key, all()),
    compile([{Key, Val} | KVs]).

-spec delete(any()) -> ok.
%% @doc Delete term stored at Key, no-op if non-existent.
delete(Key) ->
    KVs = proplists:delete(Key, all()),
    compile(KVs).


compile(KVs) ->
    Bin = compile_mod(KVs),
    code:purge(?DYNMOD),
    {module, ?DYNMOD} = code:load_binary(?DYNMOD, ?ERLFILE, Bin),
    ok.


-spec compile_mod([any()]) -> binary().
compile_mod(KVs) ->
    Opts = [verbose, report_errors],
    {ok, ?DYNMOD, Bin} = compile:forms(forms(KVs), Opts),
    Bin.


-spec forms([any()]) -> [erl_syntax:syntaxTree()].
forms(KVs) ->
    validate_term(KVs),
    Statements = [
        module_stmt(),
        export_stmt(),
        list_function(KVs),
        lookup_function(KVs)
    ],
    [erl_syntax:revert(X) || X <- Statements].


-spec module_stmt() -> erl_syntax:syntaxTree().
module_stmt() ->
    erl_syntax:attribute(
        erl_syntax:atom(module),
        [erl_syntax:atom(?DYNMOD)]
    ).

-spec export_stmt() -> erl_syntax:syntaxTree().
export_stmt() ->
    erl_syntax:attribute(
        erl_syntax:atom(export),
        [erl_syntax:list([
            erl_syntax:arity_qualifier(
                erl_syntax:atom(list),
                erl_syntax:integer(0)),
            erl_syntax:arity_qualifier(
                erl_syntax:atom(lookup),
                erl_syntax:integer(2))
        ])]
    ).


-spec list_function([any()]) -> erl_syntax:syntaxTree().
list_function(KVs) ->
    erl_syntax:function(
        erl_syntax:atom(list),
        [erl_syntax:clause([], none, [erl_syntax:abstract(KVs)])]).


-spec lookup_function([any()]) -> erl_syntax:syntaxTree().
lookup_function(KVs) ->
    Clauses = lists:foldl(fun({K, V}, ClauseAcc) ->
        Patterns = [erl_syntax:abstract(K), erl_syntax:underscore()],
        Bodies = [erl_syntax:abstract(V)],
        [erl_syntax:clause(Patterns, none, Bodies) | ClauseAcc]
    end, [default_clause()], KVs),
    erl_syntax:function(erl_syntax:atom(lookup), Clauses).


-spec default_clause() -> erl_syntax:syntaxTree().
default_clause() ->
    Patterns = [erl_syntax:underscore(), erl_syntax:variable("Default")],
    Bodies = [erl_syntax:variable("Default")],
    erl_syntax:clause(Patterns, none, Bodies).


-spec validate_term(any()) -> ok.
%% @doc Validate that a term is supported. Throws invalid_term
%% on error.
validate_term(T) when is_list(T) ->
    validate_list(T);
validate_term(T) when is_tuple(T) ->
    validate_tuple(T);
validate_term(T) when is_bitstring(T) ->
    case bit_size(T) rem 8 of
        0 -> ok;
        _ -> erlang:error(invalid_term)
    end;
validate_term(_T) ->
    ok.

-spec validate_list(list()) -> ok.
validate_list([]) ->
    ok;
validate_list([H|T]) ->
    validate_term(H),
    validate_list(T).

-spec validate_tuple(tuple()) -> ok.
validate_tuple(T) ->
    validate_tuple(T, 1, size(T)).

-spec validate_tuple(tuple(), pos_integer(), pos_integer()) -> ok.
validate_tuple(T, Pos, Size) when Pos =< Size ->
    validate_term(element(Pos, T)),
    validate_tuple(T, Pos+1, Size);
validate_tuple(_, _, _) ->
    ok.

