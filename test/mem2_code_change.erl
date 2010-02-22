-module(mem2_code_change).

-export([run/0]).

run() ->
    Pid = whereis(membership),
    OldVsn = "0.7.1-cloudant",
    Extra = "",
    
    sys:suspend(Pid),
    sys:change_code(Pid, membership2, OldVsn, Extra),
    sys:resume(Pid).
