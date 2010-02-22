-module(test_suite).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
  [{module, mem_utils_test},
   {module, membership2_test},
   {module, partitions_test},
   {module, replication_test}
  ].
