# How to add a check

A new check can be added by creating a new file in the src/ directory named
`weatherreport_check_NAME.erl` where `NAME` is a short descriptive check name
(e.g. memory_use).

The file must be an erlang module which implements the `weatherreport_check`
behaviour. This requires the following four functions to be implemented (see
the documentation of the `weatherreport_check` module for more details):

 - `description/0` Return a short description of what the check does. This will
   be printed to the console when `weatherreport` is run with the `-l` option.

 - `valid/0` Check that running the diagnostic check is valid. Any preconditions
   required by the check (e.g. cluster connectivity) should be carried out here.
   If a check has no prerequisites then this function can just return `true`.

 - `check/0` The function that actually performs the check. Typically this will
   involve either calls to the local OS (via `weatherreport_util:run_command/1`,
   calls to the local cluster node (via `weatherreport_node:local_command/3`) or
   calls to the cluster (via `weatherreport_node:cluster_command/3`). This
   command should return a list of tuples of the form `{LogLevel, Message}`
   where `LogLevel` is an atom that specifies a supported log level (e.g.
   `warning` or `info`) and `Message` is any erlang term that is matched by the
   `format/1` function.

 - `format/1` This function is used to format the messages returned by `check/0`
   and its clauses must match all possible messages returnable by `check/0`. It
   should return a tuple of the form `{String, Args}` where `String` is the
   format string `Args` is the list of formatting arguments. The format string
   should be a human-readable description of the message.

## Annotated example

The following annotated example is based on `weatherreport_check_memory_use.erl`
and the file header and licence is omitted.

```erlang
%% @doc Diagnostic that checks the current memory usage. If memory
%% usage is high, a warning message will be sent, otherwise only
%% informational messages.
```

The module begins with an edoc declaration which provides af full description of
the check. Any relevant details which cannot be communicated in the one-line
string returned by `description/0` function should be included here.

```erlang
-module(weatherreport_check_memory_use).
-behaviour(weatherreport_check).

-export([description/0,
         valid/0,
         check/0,
         format/1]).
```

The module name is specified, the `weatherreport_check` behaviour is set and the
functions required by that behaviour are exported.

```erlang
-spec description() -> string().
description() ->
    "Measure memory usage".
```

Define `description/0` which returns a concise description for inclusion in
command line output.

```erlang
-spec valid() -> boolean().
valid() ->
    weatherreport_node:can_connect().
```

Define `valid/0` which is used to check that we can connect to the local cluster
node. Connectivity to the local node is required in this check so that the OS
process ID can be obtained.

```erlang
-spec check() -> [{atom(), term()}].
check() ->
    Pid = weatherreport_node:pid(),
    Output = weatherreport_util:run_command("ps -o pmem,rss -p " ++ Pid),
    [_,_,Percent, RealSize| _] = string:tokens(Output, "/n \n"),
    Messages = [{info, {process_usage, Percent, RealSize}}],
    case weatherreport_util:binary_to_float(list_to_binary(Percent)) >= 90 of
        false ->
            Messages;
        true ->
            [{critical, {high_memory, Percent}} | Messages]
    end.
```

The actual code that carries out the check. Note that an `info` message is
always returned and a `critical` message is appended to the `Messages` list
only if memory usage exceeds a hard-coded threshold. Note also that there are
two message forms: `{process_usage, Percent RealSize}` and
`{high_memory, Percent}`. When `format/1` is defined it must match both of
these message forms.

```erlang
-spec format(term()) -> {io:format(), [term()]}.
format({high_memory, Percent}) ->
    {"Memory usage is HIGH: ~s% of available RAM", [Percent]};
format({process_usage, Percent, Real}) ->
    {"Process is using ~s% of available RAM, totalling ~s KB of real memory.", [Percent, Real]}.
```

Finally `format/1` is defined. There are two function clauses, one to match each
of the message forms that can be returned by check. The tuple returned by this
function will eventually be used to generate the text displayed in the console
output.
