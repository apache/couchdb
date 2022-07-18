% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_event_listener).

-export([
    start/3,
    start/4,
    start_link/3,
    start_link/4,
    enter_loop/3,
    cast/2
]).

-export([
    do_init/3,
    loop/2
]).

-record(st, {
    module,
    state
}).

-callback init(Arg :: term()) ->
    term().

-callback terminate(Reason :: term(), State :: term()) ->
    term().

-callback handle_cast(Message :: term(), State :: term()) ->
    term().

-callback handle_event(DbName :: term(), Event :: term(), State :: term()) ->
    term().

-callback handle_info(Message :: term(), State :: term()) ->
    term().

start(Mod, Arg, Options) ->
    Pid = erlang:spawn(?MODULE, do_init, [Mod, Arg, Options]),
    {ok, Pid}.

start(Name, Mod, Arg, Options) ->
    case where(Name) of
        undefined ->
            start(Mod, Arg, [{name, Name} | Options]);
        Pid ->
            {error, {already_started, Pid}}
    end.

start_link(Mod, Arg, Options) ->
    Pid = erlang:spawn_link(?MODULE, do_init, [Mod, Arg, Options]),
    {ok, Pid}.

start_link(Name, Mod, Arg, Options) ->
    case where(Name) of
        undefined ->
            start_link(Mod, Arg, [{name, Name} | Options]);
        Pid ->
            {error, {already_started, Pid}}
    end.

enter_loop(Module, State, Options) ->
    ok = register_listeners(Options),
    ?MODULE:loop(#st{module = Module, state = State}, infinity).

cast(Pid, Message) ->
    Pid ! {'$couch_event_cast', Message},
    ok.

do_init(Module, Arg, Options) ->
    ok = maybe_name_process(Options),
    ok = register_listeners(Options),
    case (catch Module:init(Arg)) of
        {ok, State} ->
            ?MODULE:loop(#st{module = Module, state = State}, infinity);
        {ok, State, Timeout} when is_integer(Timeout), Timeout >= 0 ->
            ?MODULE:loop(#st{module = Module, state = State}, Timeout);
        Else ->
            erlang:exit(Else)
    end.

loop(St, Timeout) ->
    receive
        {'$couch_event', DbName, Event} ->
            do_event(St, DbName, Event);
        {'$couch_event_cast', Message} ->
            do_cast(St, Message);
        Else ->
            do_info(St, Else)
    after Timeout ->
        do_info(St, timeout)
    end.

maybe_name_process(Options) ->
    case proplists:lookup(name, Options) of
        {name, Name} ->
            case name_register(Name) of
                true ->
                    ok;
                {false, Pid} ->
                    erlang:error({already_started, Pid})
            end;
        none ->
            ok
    end.

register_listeners(Options) ->
    case get_all_dbnames(Options) of
        all_dbs ->
            couch_event:register_all(self());
        DbNames ->
            couch_event:register_many(self(), DbNames)
    end,
    ok.

do_event(#st{module = Module, state = State} = St, DbName, Event) ->
    case (catch Module:handle_event(DbName, Event, State)) of
        {ok, NewState} ->
            ?MODULE:loop(St#st{state = NewState}, infinity);
        {ok, NewState, Timeout} when is_integer(Timeout), Timeout >= 0 ->
            ?MODULE:loop(St#st{state = NewState}, Timeout);
        {stop, Reason, NewState} ->
            do_terminate(Reason, St#st{state = NewState});
        Else ->
            erlang:error(Else)
    end.

do_cast(#st{module = Module, state = State} = St, Message) ->
    case (catch Module:handle_cast(Message, State)) of
        {ok, NewState} ->
            ?MODULE:loop(St#st{state = NewState}, infinity);
        {ok, NewState, Timeout} when is_integer(Timeout), Timeout >= 0 ->
            ?MODULE:loop(St#st{state = NewState}, Timeout);
        {stop, Reason, NewState} ->
            do_terminate(Reason, St#st{state = NewState});
        Else ->
            erlang:error(Else)
    end.

do_info(#st{module = Module, state = State} = St, Message) ->
    case (catch Module:handle_info(Message, State)) of
        {ok, NewState} ->
            ?MODULE:loop(St#st{state = NewState}, infinity);
        {ok, NewState, Timeout} when is_integer(Timeout), Timeout >= 0 ->
            ?MODULE:loop(St#st{state = NewState}, Timeout);
        {stop, Reason, NewState} ->
            do_terminate(Reason, St#st{state = NewState});
        Else ->
            erlang:error(Else)
    end.

do_terminate(Reason, #st{module = Module, state = State}) ->
    % Order matters. We want to make sure Module:terminate/1
    % is called even if couch_event:unregister/1 hangs
    % indefinitely.
    catch Module:terminate(Reason, State),
    catch couch_event:unregister(self()),
    Status =
        case Reason of
            normal -> normal;
            shutdown -> normal;
            ignore -> normal;
            Else -> Else
        end,
    erlang:exit(Status).

where({global, Name}) -> global:whereis_name(Name);
where({local, Name}) -> whereis(Name).

name_register({global, Name} = GN) ->
    case global:register_name(Name, self()) of
        yes -> true;
        no -> {false, where(GN)}
    end;
name_register({local, Name} = LN) ->
    try register(Name, self()) of
        true -> true
    catch
        error:_ ->
            {false, where(LN)}
    end.

get_all_dbnames(Options) ->
    case proplists:get_value(all_dbs, Options) of
        true -> all_dbs;
        _ -> get_all_dbnames(Options, [])
    end.

get_all_dbnames([], []) ->
    erlang:error(no_dbnames_provided);
get_all_dbnames([], Acc) ->
    lists:usort(convert_dbname_list(Acc));
get_all_dbnames([{dbname, DbName} | Rest], Acc) ->
    get_all_dbnames(Rest, [DbName | Acc]);
get_all_dbnames([{dbnames, DbNames} | Rest], Acc) when is_list(DbNames) ->
    get_all_dbnames(Rest, DbNames ++ Acc);
get_all_dbnames([_Ignored | Rest], Acc) ->
    get_all_dbnames(Rest, Acc).

convert_dbname_list([]) ->
    [];
convert_dbname_list([DbName | Rest]) when is_binary(DbName) ->
    [DbName | convert_dbname_list(Rest)];
convert_dbname_list([DbName | Rest]) when is_list(DbName) ->
    [list_to_binary(DbName) | convert_dbname_list(Rest)];
convert_dbname_list([DbName | _]) ->
    erlang:error({invalid_dbname, DbName}).
