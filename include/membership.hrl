
%% version 3 of membership state
-record(mem, {header=3,
              nodes=[],
              clock=[],
              args
             }).

%% types
-type join_type() :: init | join | replace | leave.
-type join_order() :: non_neg_integer().
-type options() :: list().
-type mem_node() :: {join_order(), node(), options()}.
-type mem_node_list() :: [mem_node()].
-type arg_options() :: {test, boolean()}.
-type args() :: [] | [arg_options()].
-type mem_state() :: #mem{}.
-type test() :: undefined | node().
-type epoch() :: float().
-type clock() :: {node(), epoch()}.
-type vector_clock() :: [clock()].
-type ping_node() :: node() | nil.

-type part() :: integer().
-type ref_node_part() :: {reference(), node(), part()}.
-type tref() :: reference().
-type np() :: {node(), part()}.
-type np_acc() :: [{np(), any()}].
