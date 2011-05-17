%% @author Bob Ippolito <bob@mochimedia.com>
%% @copyright 2010 Mochi Media, Inc.
%% @doc Abuse module constant pools as a "read-only shared heap" (since erts 5.6)
%%      <a href="http://www.erlang.org/pipermail/erlang-questions/2009-March/042503.html">[1]</a>.
-module(mochiglobal).
-author("Bob Ippolito <bob@mochimedia.com>").
-export([get/1, get/2, put/2, delete/1]).

-spec get(atom()) -> any() | undefined.
%% @equiv get(K, undefined)
get(K) ->
    get(K, undefined).

-spec get(atom(), T) -> any() | T.
%% @doc Get the term for K or return Default.
get(K, Default) ->
    get(K, Default, key_to_module(K)).

get(_K, Default, Mod) ->
    try Mod:term()
    catch error:undef ->
            Default
    end.

-spec put(atom(), any()) -> ok.
%% @doc Store term V at K, replaces an existing term if present.
put(K, V) ->
    put(K, V, key_to_module(K)).

put(_K, V, Mod) ->
    Bin = compile(Mod, V),
    code:purge(Mod),
    code:load_binary(Mod, atom_to_list(Mod) ++ ".erl", Bin),
    ok.

-spec delete(atom()) -> boolean().
%% @doc Delete term stored at K, no-op if non-existent.
delete(K) ->
    delete(K, key_to_module(K)).

delete(_K, Mod) ->
    code:purge(Mod),
    code:delete(Mod).

-spec key_to_module(atom()) -> atom().
key_to_module(K) ->
    list_to_atom("mochiglobal:" ++ atom_to_list(K)).

-spec compile(atom(), any()) -> binary().
compile(Module, T) ->
    {ok, Module, Bin} = compile:forms(forms(Module, T),
                                      [verbose, report_errors]),
    Bin.

-spec forms(atom(), any()) -> [erl_syntax:syntaxTree()].
forms(Module, T) ->
    [erl_syntax:revert(X) || X <- term_to_abstract(Module, term, T)].

-spec term_to_abstract(atom(), atom(), any()) -> [erl_syntax:syntaxTree()].
term_to_abstract(Module, Getter, T) ->
    [%% -module(Module).
     erl_syntax:attribute(
       erl_syntax:atom(module),
       [erl_syntax:atom(Module)]),
     %% -export([Getter/0]).
     erl_syntax:attribute(
       erl_syntax:atom(export),
       [erl_syntax:list(
         [erl_syntax:arity_qualifier(
            erl_syntax:atom(Getter),
            erl_syntax:integer(0))])]),
     %% Getter() -> T.
     erl_syntax:function(
       erl_syntax:atom(Getter),
       [erl_syntax:clause([], none, [erl_syntax:abstract(T)])])].

%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).
get_put_delete_test() ->
    K = '$$test$$mochiglobal',
    delete(K),
    ?assertEqual(
       bar,
       get(K, bar)),
    try
        ?MODULE:put(K, baz),
        ?assertEqual(
           baz,
           get(K, bar)),
        ?MODULE:put(K, wibble),
        ?assertEqual(
           wibble,
           ?MODULE:get(K))
    after
        delete(K)
    end,
    ?assertEqual(
       bar,
       get(K, bar)),
    ?assertEqual(
       undefined,
       ?MODULE:get(K)),
    ok.
-endif.
