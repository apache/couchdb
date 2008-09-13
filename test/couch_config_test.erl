% couch_config module test suote

% Set up test suite
% ?MODULE_test() returns a list of functions 
% that run the actual tests.
couch_config_test() ->
    [
        fun() -> store_strings() end
    ].


% test functions
  
store_strings() ->
    Filename = "test.ini",
    file:write_file(Filename, ""),

    Key = "foo",
    Value = "bar",

    {ok, Proc} = couch_config:start_link([Filename]),

    couch_config:set("test_module", Key, Value),
    Result = couch_config:get("test_module", Key),
    couch_config:delete("test_module", Key),

    exit(Proc, kill),
    receive {'EXIT', Proc, _} -> ok end,
    
    % clean up
    file:delete(Filename),

    Value = Result.