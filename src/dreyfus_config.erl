 -module(dreyfus_config).

 -export([data/0, get/1]).

data() ->
    config:get("dreyfus_blacklist").

get(Key) ->
    Handle = couch_epi:get_handle({dreyfus, black_list}),
    couch_epi:get_value(Handle, dreyfus, Key).
