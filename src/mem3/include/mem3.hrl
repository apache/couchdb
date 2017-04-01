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

% type specification hacked to suppress dialyzer warning re: match spec
-record(shard, {
    name :: binary() | '_',
    node :: node() | '_',
    dbname :: binary(),
    range :: [non_neg_integer() | '$1' | '$2'] | '_',
    ref :: reference() | 'undefined' | '_'
}).

%% Do not reference outside of mem3.
-record(ordered_shard, {
    name :: binary() | '_',
    node :: node() | '_',
    dbname :: binary(),
    range :: [non_neg_integer() | '$1' | '$2'] | '_',
    ref :: reference() | 'undefined' | '_',
    order :: non_neg_integer() | 'undefined' | '_'
}).

%% types
-type join_type() :: init | join | replace | leave.
-type join_order() :: non_neg_integer().
-type options() :: list().
-type mem_node() :: {join_order(), node(), options()}.
-type mem_node_list() :: [mem_node()].
-type arg_options() :: {test, boolean()}.
-type args() :: [] | [arg_options()].
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
