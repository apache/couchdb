%% @copyright Copyright (c) 2010 Mochi Media, Inc.
%% @author David Reid <dreid@mochimedia.com>

%% @doc Utility functions for dealing with proplists.

-module(mochilists).
-author("David Reid <dreid@mochimedia.com>").
-export([get_value/2, get_value/3, is_defined/2, set_default/2, set_defaults/2]).

%% @spec set_default({Key::term(), Value::term()}, Proplist::list()) -> list()
%%
%% @doc Return new Proplist with {Key, Value} set if not is_defined(Key, Proplist).
set_default({Key, Value}, Proplist) ->
    case is_defined(Key, Proplist) of
        true ->
            Proplist;
        false ->
            [{Key, Value} | Proplist]
    end.

%% @spec set_defaults([{Key::term(), Value::term()}], Proplist::list()) -> list()
%%
%% @doc Return new Proplist with {Key, Value} set if not is_defined(Key, Proplist).
set_defaults(DefaultProps, Proplist) ->
    lists:foldl(fun set_default/2, Proplist, DefaultProps).


%% @spec is_defined(Key::term(), Proplist::list()) -> bool()
%%
%% @doc Returns true if Propist contains at least one entry associated
%%      with Key, otherwise false is returned.
is_defined(Key, Proplist) ->
    lists:keyfind(Key, 1, Proplist) =/= false.


%% @spec get_value(Key::term(), Proplist::list()) -> term() | undefined
%%
%% @doc Return the value of <code>Key</code> or undefined
get_value(Key, Proplist) ->
    get_value(Key, Proplist, undefined).

%% @spec get_value(Key::term(), Proplist::list(), Default::term()) -> term()
%%
%% @doc Return the value of <code>Key</code> or <code>Default</code>
get_value(_Key, [], Default) ->
    Default;
get_value(Key, Proplist, Default) ->
    case lists:keyfind(Key, 1, Proplist) of
        false ->
            Default;
        {Key, Value} ->
            Value
    end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

set_defaults_test() ->
    ?assertEqual(
       [{k, v}],
       set_defaults([{k, v}], [])),
    ?assertEqual(
       [{k, v}],
       set_defaults([{k, vee}], [{k, v}])),
    ?assertEqual(
       lists:sort([{kay, vee}, {k, v}]),
       lists:sort(set_defaults([{k, vee}, {kay, vee}], [{k, v}]))),
    ok.

set_default_test() ->
    ?assertEqual(
       [{k, v}],
       set_default({k, v}, [])),
    ?assertEqual(
       [{k, v}],
       set_default({k, vee}, [{k, v}])),
    ok.

get_value_test() ->
    ?assertEqual(
       undefined,
       get_value(foo, [])),
    ?assertEqual(
       undefined,
       get_value(foo, [{bar, baz}])),
    ?assertEqual(
       bar,
       get_value(foo, [{foo, bar}])),
    ?assertEqual(
       default,
       get_value(foo, [], default)),
    ?assertEqual(
       default,
       get_value(foo, [{bar, baz}], default)),
    ?assertEqual(
       bar,
       get_value(foo, [{foo, bar}], default)),
    ok.

-endif.

