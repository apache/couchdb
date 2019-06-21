-module(dreyfus_test_util).

-compile(export_all).

-include_lib("couch/include/couch_db.hrl").

wait_config_change(Key, Value) ->
    test_util:wait(fun() ->
        case dreyfus_config:get(Key) of
            Value -> ok;
            _ -> wait
        end
    end).
