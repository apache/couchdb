-module(smoosh_priority_queue_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("couch/include/couch_eunit.hrl").

-define(PROP_PREFIX, "prop_").

-define(CAPACITY, 3).

-define(RANDOM_FILE, lists:flatten(io_lib:format("~p", [erlang:timestamp()]))).

setup() ->
    Ctx = test_util:start_couch(),
    Ctx.

teardown(Ctx) ->
    test_util:stop_couch(Ctx).

smoosh_priority_queue_test_() ->
    {
        "smoosh priority queue test",
        {
            setup,
            fun setup/0,
            fun teardown/1,
            [
                fun prop_inverse_test_/0,
                fun no_halt_on_corrupted_file_test/0,
                fun no_halt_on_missing_file_test/0
            ]
        }
    }.

%% ==========
%% Tests
%% ----------

%% define all tests to be able to run them individually
prop_inverse_test_() ->
    ?_test(begin
        test_property(prop_inverse)
    end).

no_halt_on_corrupted_file_test() ->
    ?_test(begin
        Name = ?RANDOM_FILE,
        Q0 = smoosh_priority_queue:new(Name),
        Q = smoosh_priority_queue:open(Q0),
        FilePath = smoosh_priority_queue:file_name(Q),
        ok = file:write_file(FilePath, <<"garbage">>),
        ?assertEqual(Q, smoosh_priority_queue:open(Q0)),
        ok
    end).

no_halt_on_missing_file_test() ->
    ?_test(begin
        Name = ?RANDOM_FILE,
        Q0 = smoosh_priority_queue:new(Name),
        Q = smoosh_priority_queue:open(Q0),
        FilePath = smoosh_priority_queue:file_name(Q),
        ok = file:delete(FilePath),
        ?assertEqual(Q, smoosh_priority_queue:open(Q0)),
        ok
    end).

%% ==========
%% Properties
%% ----------

prop_inverse() ->
    ?FORALL(
        Q,
        queue(),
        begin
            List = smoosh_priority_queue:to_list(Q),
            equal(Q, smoosh_priority_queue:from_list(List, Q))
        end
    ).

%% ==========
%% Generators
%% ----------

key() ->
    proper_types:oneof([proper_types:binary(), {proper_types:binary(), proper_types:binary()}]).
value() ->
    proper_types:oneof([proper_types:binary(), {proper_types:binary(), proper_types:binary()}]).
priority() -> integer().
item() -> {key(), value(), priority()}.

items_list() ->
    ?LET(L, list(item()), L).

simple_queue() ->
    ?LET(
        L,
        items_list(),
        from_list(L)
    ).

with_deleted() ->
    ?LET(
        Q,
        ?LET(
            {{K0, V0, P0}, Q0},
            {item(), simple_queue()},
            smoosh_priority_queue:in(K0, V0, P0, ?CAPACITY, Q0)
        ),
        frequency([
            {1, Q},
            {2, element(3, smoosh_priority_queue:out(Q))}
        ])
    ).

queue() ->
    with_deleted().

%% ==========================
%% Proper related boilerplate
%% --------------------------

test_property(Property) when is_atom(Property) ->
    test_property({atom_to_list(Property), Property});
test_property({Id, Property}) ->
    Name = string:sub_string(Id, length(?PROP_PREFIX) + 1),
    Opts = [long_result, {numtests, 1000}, {to_file, user}],
    {Name, {timeout, 60, fun() -> test_it(Property, Opts) end}}.

test_it(Property, Opts) ->
    case proper:quickcheck(?MODULE:Property(), Opts) of
        true ->
            true;
        Else ->
            erlang:error(
                {propertyFailed, [
                    {module, ?MODULE},
                    {property, Property},
                    {result, Else}
                ]}
            )
    end.

%% ================
%% Helper functions
%% ----------------

new() ->
    Q = smoosh_priority_queue:new("foo"),
    smoosh_priority_queue:open(Q).

from_list(List) ->
    lists:foldl(
        fun({Key, Value, Priority}, Queue) ->
            smoosh_priority_queue:in(Key, Value, Priority, ?CAPACITY, Queue)
        end,
        new(),
        List
    ).

equal(Q1, Q2) ->
    out_all(Q1) =:= out_all(Q2).

out_all(Q) ->
    out_all(Q, []).
out_all(Q0, Acc) ->
    case smoosh_priority_queue:out(Q0) of
        {K, V, Q1} -> out_all(Q1, [{K, V} | Acc]);
        false -> lists:reverse(Acc)
    end.
