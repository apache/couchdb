-module(bad_plugin).
-compile(export_all).

%% this plugin contains numerous DELIBERATE syntax errors

fwibble(Config, _) >
    file:delete("fwibble.test")
