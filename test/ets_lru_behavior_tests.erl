-module(ets_lru_behavior_tests).
-behaviour(proper_statem).


-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


-export([
    initial_state/0,
    command/1,
    precondition/2,
    postcondition/3,
    next_state/3,
    random_key/1
]).


-export([
    kvs_insert/4,
    kvs_lookup/2,
    kvs_remove/2,
    kvs_hit/2
]).


-record(st, {
    ets_lru,
    kvs_lru
}).


proper_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 20},
        {numtests, 1000}
    ],
    {timeout, 3600, ?_assertEqual([], proper:module(?MODULE, PropErOpts))}.


prop_lru() ->
    Fmt = "History: ~p~nState: ~p~nRes: ~p~nCmds:~n~p~n",
    ?FORALL(Cmds, commands(?MODULE),
        begin
            {H, S, R} = run_commands(?MODULE, Cmds),
            cleanup(S),
            ?WHENFAIL(
                io:format(standard_error, Fmt, [H, S, R, Cmds]),
                R =:= ok
            )
        end
    ).


initial_state() ->
    #st{}.


command(#st{ets_lru=undefined}=S) ->
    MaxObjs = 1 + random:uniform(5),
    Opts = [{max_objects, MaxObjs}],
    {call, ets_lru, create, [proper_ets_lru_tab, Opts]};
command(S) ->
    Key = {call, ?MODULE, random_key, [S]},
    frequency([
        % Common operations
        {50, {call, ets_lru, insert, [S#st.ets_lru, key(), val()]}},
        {5, {call, ets_lru, lookup, [S#st.ets_lru, Key]}},

        % Make removes less common so we hit limits
        {3, {call, ets_lru, remove, [S#st.ets_lru, Key]}},

        {1, {call, ets_lru, insert, [S#st.ets_lru, Key, val()]}},
        {1, {call, ets_lru, lookup, [S#st.ets_lru, key()]}},
        {1, {call, ets_lru, remove, [S#st.ets_lru, key()]}},

        {1, {call, ets_lru, hit, [S#st.ets_lru, Key]}},
        {1, {call, ets_lru, clear, [S#st.ets_lru]}}
    ]).


precondition(_, _) ->
    true.


postcondition(_S, {call, _, create, [_, _]}, {ok, _}) ->
    true;
postcondition(S, {call, _, insert, [_, _Key, _]}, ok) ->
    check_constraints(S);
postcondition(S, {call, _, lookup, [_, Key]}, Val) ->
    case lists:keysearch(Key, 1, S#st.kvs_lru) of
        {value, {Key, V}} when {ok, V} == Val ->
            check_constraints(S);
        false when Val == not_found ->
            check_constraints(S);
        E ->
            io:format(standard_error, "Bad lookup: ~p ~p~n",
                [E, Val]),
            false
    end;
postcondition(S, {call, _, remove, [_, _Key]}, ok) ->
    check_constraints(S);
postcondition(S, {call, _, hit, [_, _Key]}, ok) ->
    check_constraints(S);
postcondition(S, {call, _, clear, [_]}, ok) ->
    check_constraints(S);
postcondition(_S, _C, _V) ->
    io:format(standard_error, "BAD CALL: ~p ~p~n", [_C, _V]),
    false.


check_constraints(S) ->
    ELRU = S#st.ets_lru,
    Count = ets:info(element(2, ELRU), size),
    MaxCount = element(5, ELRU),
    case Count > MaxCount of
        true ->
            io:format(standard_error, "Max count exceeded: ~p ~p~n",
                [Count, MaxCount]);
        false ->
            ok
    end,
    Count =< MaxCount.


next_state(S, V, {call, _, create, [_, _]}) ->
    S#st{
        ets_lru={call, erlang, element, [2, V]},
        kvs_lru=[]
    };
next_state(#st{kvs_lru=KVs}=S, _V, {call, _, insert, [_, Key, Val]}) ->
    S#st{
        kvs_lru={call, ?MODULE, kvs_insert, [KVs, Key, Val, S#st.ets_lru]}
    };
next_state(#st{kvs_lru=KVs}=S, _V, {call, _, lookup, [_, Key]}) ->
    S#st{
        kvs_lru={call, ?MODULE, kvs_lookup, [KVs, Key]}
    };
next_state(#st{kvs_lru=KVs}=S, _V, {call, _, remove, [_, Key]}) ->
    S#st{
        kvs_lru={call, ?MODULE, kvs_remove, [KVs, Key]}
    };
next_state(#st{kvs_lru=KVs}=S, _V, {call, _, hit, [_, Key]}) ->
    S#st{
        kvs_lru={call, ?MODULE, kvs_hit, [KVs, Key]}
    };
next_state(S, _V, {call, _, clear, [_]}) ->
    S#st{
        kvs_lru=[]
    }.


cleanup(#st{ets_lru=undefined}) ->
    ok;
cleanup(S) ->
    ets_lru:destroy(S#st.ets_lru).


random_key(#st{kvs_lru=KVs}) ->
    Keys = [foo] ++ [K || {K, _V, _T} <- KVs],
    NumKeys = erlang:length(Keys),
    KeyPos = random:uniform(NumKeys),
    lists:nth(KeyPos, Keys).


% Simple inefficient LRU implementation

kvs_insert(KVs, K, V, ELRU) ->
    Max = element(5, ELRU),
    NewKVs = [{K, V} | lists:keydelete(K, 1, KVs)],
    lists:sublist(NewKVs, Max).


kvs_lookup(KVs, K) ->
    case lists:keysearch(K, 1, KVs) of
        {value, {K, V}} ->
            TmpKVs = lists:keydelete(K, 1, KVs),
            [{K, V} | TmpKVs];
        false ->
            KVs
    end.


kvs_remove(KVs, K) ->
    lists:keydelete(K, 1, KVs).


kvs_hit(S, K) ->
    kvs_lookup(S, K).


key() -> any().
val() -> any().

