-define(MEMBERSHIP, true).

-ifndef(FABRIC).
-include("../../fabric/include/fabric.hrl").
-endif.

-ifndef(COUCH).
-include("../../couch/src/couch_db.hrl").
-endif.

-include_lib("eunit/include/eunit.hrl").

%% version 3 of membership state
-record(mem, {header=3,
              nodes=[],
              clock=[],
              args
             }).

%% partition record
-record(shard, {name, node, dbname, range, ref}).

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
-type gossip_fun() :: call | cast.

-type part() :: #shard{}.
-type fullmap() :: [part()].
-type ref_part_map() :: {reference(), part()}.
-type tref() :: reference().
-type np() :: {node(), part()}.
-type beg_acc() :: [integer()].
