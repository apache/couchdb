-module(ddoc_cache_util).


-export([
    new_uuid/0
]).


new_uuid() ->
    to_hex(crypto:rand_bytes(16), []).


to_hex(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));
to_hex(<<C1:4, C2:4, Rest/binary>>, Acc) ->
    to_hex(Rest, [hexdig(C1), hexdig(C2) | Acc]).


hexdig(C) when C >= 0, C =< 9 ->
    C + $0;
hexdig(C) when C >= 10, C =< 15 ->
    C + $A - 10.
