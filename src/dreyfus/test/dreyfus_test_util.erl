-module(dreyfus_test_util).

-export([
    wait_config_change/2
]).

-include_lib("couch/include/couch_db.hrl").

wait_config_change(Key, Value) ->
    test_util:wait(fun() ->
        case dreyfus_config:get(Key) of
            Value -> ok;
            _ -> wait
        end
    end).
