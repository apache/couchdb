-module (mem3_cache_bench).

-export ([doit/1]).

-include("../include/config.hrl").


init() ->
    Config = #config{n=3,r=2,w=2,q=3,directory="/srv/db",
                     storage_mod="dynomite_couch_storage"},
    {ok, _Pid} = mem3:start_link([{test,true}, {config, Config}]),
    mem3:join(first, [{1, a, []}, {2, b, []}]).


doit(Reps) ->
    init(),
    Begin = erlang:now(),
    process(Reps),
    % seconds to run the test
    Time = timer:now_diff(erlang:now(), Begin)/1000000,
    mem3:stop(),
    Time.


process(0) ->
    ok;
process(M) ->
    mem3:fullmap(),
    process(M-1).
