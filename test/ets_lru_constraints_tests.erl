-module(ets_lru_max_count_tests).
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


-record(st, {lru, keys}).


proper_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 25},
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
    #st{keys=[]}.


command(#st{lru=undefined}=S) ->
    MaxObjs = 1 + random:uniform(10),
    MaxSize = 512 + random:uniform(128),
    Opts = [{max_objects, MaxObjs}, {max_size, MaxSize}],
    {call, ets_lru, create, [proper_ets_lru_tab, Opts]};
command(S) ->
    Key = {call, ?MODULE, random_key, [S#st.keys]},
    frequency([
        {50, {call, ets_lru, insert, [S#st.lru, key(), val()]}},
        {1, {call, ets_lru, lookup, [S#st.lru, Key]}},
        {1, {call, ets_lru, remove, [S#st.lru, Key]}},
        {1, {call, ets_lru, hit, [S#st.lru, Key]}}
    ]).


precondition(_, _) ->
    true.


postcondition(_S, {call, _, create, [_, _]}, {ok, _}) ->
    true;
postcondition(S, _C, _V) ->
    check_constraints(S).


check_constraints(S) ->
    Count = ets:info(element(2, S#st.lru), size),
    MaxCount = element(5, S#st.lru),
    case Count > MaxCount of
        true ->
            io:format(standard_error, "Max count exceeded: ~p ~p~n",
                [Count, MaxCount]);
        false ->
            ok
    end,
    Size = ets:info(element(2, S#st.lru), memory),
    MaxSize = element(6, S#st.lru),
    case Size > MaxSize of
        true ->
            io:format(standard_error, "Max size exceeded: ~p ~p~n",
                [Size, MaxSize]);
        false ->
            ok
    end,
    Count =< MaxCount andalso Size =< MaxSize.


next_state(S, V, {call, _, create, [_, _]}) ->
    S#st{
        lru={call, erlang, element, [2, V]}
    };
next_state(S, _V, {call, _, insert, [_, Key, _Val]}) ->
    S#st{keys=[Key | S#st.keys]};
next_state(S, _V, _C) ->
    S.


cleanup(#st{lru=undefined}) ->
    ok;
cleanup(S) ->
    ets_lru:destroy(S#st.lru).


random_key(Keys0) ->
    Keys = [foo] ++ Keys0,
    NumKeys = erlang:length(Keys),
    KeyPos = random:uniform(NumKeys),
    lists:nth(KeyPos, Keys).


key() -> any().
val() -> any().
