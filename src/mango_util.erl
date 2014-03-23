-module(mango_util).


-export([
    to_lower/1
]).


to_lower(Key) when is_binary(Key) ->
    KStr = binary_to_list(Key),
    KLower = string:to_lower(KStr),
    list_to_binary(KLower).