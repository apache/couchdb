-module(test_suite).

-include_lib("eunit/include/eunit.hrl").

all_test_() ->
    [
        {module, mem3_test},
        {module, partitions_test}
    ].
