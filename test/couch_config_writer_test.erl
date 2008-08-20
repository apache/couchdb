% couch_config_writer module test suote

% Set up test suite
% ?MODULE_test() returns a list of functions 
% that run the actual tests.
couch_config_writer_test() ->
    [
        fun() -> replace_existing_variable() end,
        fun() -> append_new_variable() end,
        fun() -> append_new_module() end
    ].


% test functions
replace_existing_variable() ->
    % create file
    Contents = "; etc/couchdb/couch.ini.tpl.  Generated from couch.ini.tpl.in by configure.

[CouchDB]
RootDirectory=/Users/jan/Work/runcouch/conf9/var/lib/couchdb
UtilDriverDir=/Users/jan/Work/runcouch/conf9/lib/couchdb/erlang/lib/couch-0.7.3a663206/priv/lib
MaximumDocumentSize=4294967296 ; 4 GB

[HTTPd]
Port=5984
BindAddress=127.0.0.1
DocumentRoot=/Users/jan/Work/runcouch/conf9/share/couchdb/www

[Log]
File=/Users/jan/Work/runcouch/conf9/var/log/couchdb/couch.log
Level=info

[CouchDB Query Servers]
javascript=/Users/jan/Work/runcouch/conf9/bin/couchjs /Users/jan/Work/runcouch/conf9/share/couchdb/server/main.js

[CouchDB Query Server Options]
QueryTimeout=5000 ; 5 seconds
",

    Expect = "; etc/couchdb/couch.ini.tpl.  Generated from couch.ini.tpl.in by configure.

[CouchDB]
RootDirectory=/Users/jan/Work/runcouch/conf9/var/lib/couchdb
UtilDriverDir=/Users/jan/Work/runcouch/conf9/lib/couchdb/erlang/lib/couch-0.7.3a663206/priv/lib
MaximumDocumentSize=4294967296 ; 4 GB

[HTTPd]
Port=5985
BindAddress=127.0.0.1
DocumentRoot=/Users/jan/Work/runcouch/conf9/share/couchdb/www

[Log]
File=/Users/jan/Work/runcouch/conf9/var/log/couchdb/couch.log
Level=info

[CouchDB Query Servers]
javascript=/Users/jan/Work/runcouch/conf9/bin/couchjs /Users/jan/Work/runcouch/conf9/share/couchdb/server/main.js

[CouchDB Query Server Options]
QueryTimeout=5000 ; 5 seconds
",
    run_operation_and_compare_results(Contents, Expect, {{"HTTPd", "Port"}, "5985"}).


append_new_variable() ->
    % create file
    Contents = "; etc/couchdb/couch.ini.tpl.  Generated from couch.ini.tpl.in by configure.

[CouchDB]
RootDirectory=/Users/jan/Work/runcouch/conf9/var/lib/couchdb
UtilDriverDir=/Users/jan/Work/runcouch/conf9/lib/couchdb/erlang/lib/couch-0.7.3a663206/priv/lib
MaximumDocumentSize=4294967296 ; 4 GB

[HTTPd]
Port=5984
BindAddress=127.0.0.1
DocumentRoot=/Users/jan/Work/runcouch/conf9/share/couchdb/www

[Log]
File=/Users/jan/Work/runcouch/conf9/var/log/couchdb/couch.log
Level=info

[CouchDB Query Servers]
javascript=/Users/jan/Work/runcouch/conf9/bin/couchjs /Users/jan/Work/runcouch/conf9/share/couchdb/server/main.js

[CouchDB Query Server Options]
QueryTimeout=5000 ; 5 seconds
",

    Expect = "; etc/couchdb/couch.ini.tpl.  Generated from couch.ini.tpl.in by configure.

[CouchDB]
RootDirectory=/Users/jan/Work/runcouch/conf9/var/lib/couchdb
UtilDriverDir=/Users/jan/Work/runcouch/conf9/lib/couchdb/erlang/lib/couch-0.7.3a663206/priv/lib
MaximumDocumentSize=4294967296 ; 4 GB

[HTTPd]
Port=5984
BindAddress=127.0.0.1
DocumentRoot=/Users/jan/Work/runcouch/conf9/share/couchdb/www

FantasyConfiguration=Citation Needed
[Log]
File=/Users/jan/Work/runcouch/conf9/var/log/couchdb/couch.log
Level=info

[CouchDB Query Servers]
javascript=/Users/jan/Work/runcouch/conf9/bin/couchjs /Users/jan/Work/runcouch/conf9/share/couchdb/server/main.js

[CouchDB Query Server Options]
QueryTimeout=5000 ; 5 seconds
",
    run_operation_and_compare_results(Contents, Expect, {{"HTTPd", "FantasyConfiguration"}, "Citation Needed"}).


append_new_module() ->
    % create file
    Contents = "; etc/couchdb/couch.ini.tpl.  Generated from couch.ini.tpl.in by configure.

[CouchDB]
RootDirectory=/Users/jan/Work/runcouch/conf9/var/lib/couchdb
UtilDriverDir=/Users/jan/Work/runcouch/conf9/lib/couchdb/erlang/lib/couch-0.7.3a663206/priv/lib
MaximumDocumentSize=4294967296 ; 4 GB

[HTTPd]
Port=5984
BindAddress=127.0.0.1
DocumentRoot=/Users/jan/Work/runcouch/conf9/share/couchdb/www

[Log]
File=/Users/jan/Work/runcouch/conf9/var/log/couchdb/couch.log
Level=info

[CouchDB Query Servers]
javascript=/Users/jan/Work/runcouch/conf9/bin/couchjs /Users/jan/Work/runcouch/conf9/share/couchdb/server/main.js

[CouchDB Query Server Options]
QueryTimeout=5000 ; 5 seconds",

    Expect = "; etc/couchdb/couch.ini.tpl.  Generated from couch.ini.tpl.in by configure.

[CouchDB]
RootDirectory=/Users/jan/Work/runcouch/conf9/var/lib/couchdb
UtilDriverDir=/Users/jan/Work/runcouch/conf9/lib/couchdb/erlang/lib/couch-0.7.3a663206/priv/lib
MaximumDocumentSize=4294967296 ; 4 GB

[HTTPd]
Port=5984
BindAddress=127.0.0.1
DocumentRoot=/Users/jan/Work/runcouch/conf9/share/couchdb/www

[Log]
File=/Users/jan/Work/runcouch/conf9/var/log/couchdb/couch.log
Level=info

[CouchDB Query Servers]
javascript=/Users/jan/Work/runcouch/conf9/bin/couchjs /Users/jan/Work/runcouch/conf9/share/couchdb/server/main.js

[CouchDB Query Server Options]
QueryTimeout=5000 ; 5 seconds

[Erlang]
Option=Value
",
    run_operation_and_compare_results(Contents, Expect, {{"Erlang", "Option"}, "Value"}).
  
run_operation_and_compare_results(Contents, Expect, Config) ->
    Filename = "couch.ini",
    file:write_file(Filename, Contents),

    % call replace function
    couch_config_writer:save_to_file(Config, Filename),
    
    % compare new file with expected file
    {ok, Result_} = file:read_file(Filename),
    Result = binary_to_list(Result_),

    % clean up
    % file:delete(Filename),
    
    Result = Expect.
