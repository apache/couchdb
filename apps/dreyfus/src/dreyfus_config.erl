-module(dreyfus_config).

-export([data/0, get/1]).

data() ->
    try
        config:get("dreyfus_blacklist")
    catch
        error:badarg ->
            % lazy workaround to address issue with epi invocation on startup
            []
    end.

get(Key) ->
    Handle = couch_epi:get_handle({dreyfus, black_list}),
    couch_epi:get_value(Handle, dreyfus, Key).
