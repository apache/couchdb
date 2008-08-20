% couch_config module test suote

% Set up test suite
% ?MODULE_test() returns a list of functions 
% that run the actual tests.
couch_config_test() ->
    [
        fun() -> store_tuples() end, 
        fun() -> store_strings() end,
        fun() -> store_numbers() end,
        fun() -> store_tuple_key() end
    ].


% test functions

% test storing different types and see if they come back
% the same way there put in.
store_tuples() ->
    store(key, value).
  
store_strings() ->
    store("key", "value").

store_numbers() ->
    store("number_key", 12345).

store_tuple_key() ->
    store({key, subkey}, value).

    
store(Key2, Value2) ->
    Key = binary_to_list(term_to_binary(Key2)),
    Value = binary_to_list(term_to_binary(Value2)),

    couch_config:start_link(["couch.ini"]),

    couch_config:store({"test_module", Key}, Value),
    Result = couch_config:get({"test_module", Key}),
    couch_config:unset(Key),

    couch_config:terminate(end_of_test, ok),
    Value = Result.