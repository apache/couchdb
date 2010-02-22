%%% @author    Cliff Moon <cliff@powerset.com> []
%%% @copyright 2008 Cliff Moon

-module (vector_clock).
-export ([create/1, truncate/1, increment/2, compare/2, resolve/2, merge/2,
    equals/2]).

%% -ifdef(TEST).
%% -include("etest/vector_clock_test.erl").
%% -endif.

create(NodeName) -> [{NodeName, lib_misc:now_float()}].

truncate(Clock) when length(Clock) > 10 ->
  lists:nthtail(length(Clock) - 10, lists:keysort(2, Clock));

truncate(Clock) -> Clock.

increment(NodeName, [{NodeName, _Version}|Clocks]) ->
	[{NodeName, lib_misc:now_float()}|Clocks];

increment(NodeName, [NodeClock|Clocks]) ->
	[NodeClock|increment(NodeName, Clocks)];

increment(NodeName, []) ->
	[{NodeName, lib_misc:now_float()}].

resolve({ClockA, ValuesA}, {ClockB, ValuesB}) ->
  case compare(ClockA, ClockB) of
    less -> {ClockB, ValuesB};
    greater -> {ClockA, ValuesA};
    equal -> {ClockA, ValuesA};
    concurrent ->
          io:format("~nConcurrent Clocks~n"
                    "ClockA : ~p~nClockB : ~p~n"
                    "ValuesA: ~p~nValuesB: ~p~n"
                    , [ClockA, ClockB, ValuesA, ValuesB]),
          {merge(ClockA,ClockB), ValuesA ++ ValuesB}
  end;
resolve(not_found, {Clock, Values}) ->
    {Clock, Values};
resolve({Clock, Values}, not_found) ->
    {Clock, Values}.

merge(ClockA, ClockB) ->
  merge([], ClockA, ClockB).

merge(Merged, [], ClockB) -> lists:keysort(1, Merged ++ ClockB);

merge(Merged, ClockA, []) -> lists:keysort(1, Merged ++ ClockA);

merge(Merged, [{NodeA, VersionA}|ClockA], ClockB) ->
  case lists:keytake(NodeA, 1, ClockB) of
    {value, {NodeA, VersionB}, TrunkClockB} when VersionA > VersionB ->
      merge([{NodeA,VersionA}|Merged],ClockA,TrunkClockB);
    {value, {NodeA, VersionB}, TrunkClockB} ->
      merge([{NodeA,VersionB}|Merged],ClockA,TrunkClockB);
    false ->
      merge([{NodeA,VersionA}|Merged],ClockA,ClockB)
  end.

compare(ClockA, ClockB) ->
    AltB = less_than(ClockA, ClockB),
    if AltB -> less; true ->
        BltA = less_than(ClockB, ClockA),
        if BltA -> greater; true ->
            AeqB = equals(ClockA, ClockB),
            if AeqB -> equal; true -> concurrent end
        end
    end.

%% ClockA is less than ClockB if and only if ClockA[z] <= ClockB[z] for all
%% instances z and there exists an index z' such that ClockA[z'] < ClockB[z']
less_than(ClockA, ClockB) ->
  ForAll = lists:all(fun({Node, VersionA}) ->
    case lists:keysearch(Node, 1, ClockB) of
      {value, {_NodeB, VersionB}} -> VersionA =< VersionB;
      false -> false
    end
  end, ClockA),
  Exists = lists:any(fun({NodeA, VersionA}) ->
    case lists:keysearch(NodeA, 1, ClockB) of
      {value, {_NodeB, VersionB}} -> VersionA /= VersionB;
      false -> true
    end
  end, ClockA),
  %length takes care of the case when clockA is shorter than B
  ForAll and (Exists or (length(ClockA) < length(ClockB))).

equals(ClockA, ClockB) ->
  Equivalent = lists:all(fun({NodeA, VersionA}) ->
    lists:any(fun(NodeClockB) ->
      case NodeClockB of
        {NodeA, VersionA} -> true;
        _ -> false
      end
    end, ClockB)
  end, ClockA),
  Equivalent and (length(ClockA) == length(ClockB)).
