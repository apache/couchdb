-module(a).

-compile(export_all).

-include_lib("b/include/b.hrl").

hello() ->
    io:format("~s\n", [?HELLO]).
