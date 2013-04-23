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
    behaviour_info/1
]).

-export([
    do_init/3,
    loop/2
]).


-record(st, {
    module,
    state
}).


behaviour_info(callbacks) ->
    [
        {init,1},
        {terminate/2},
        {handle_cast/2},
        {handle_event/2},
        {handle_info/2}
    ];
behaviour_info(_) ->
    undefined.


start(Mod, Arg, Options) ->
    erlang:spawn(?MODULE, do_init, [Mod, Arg, Options]).


start(Name, Mod, Arg, Options) ->
    case where(Name) of
        undefined ->
            start(Mod, Arg, [{name, Name} | Options]);
        Pid ->
            {error, {already_started, Pid}}
    end.


start_link(Mod, Arg, Options) ->
    erlang:spawn_link(?MODULE, do_init, [Mod, Arg, Options]).


start_link(Name, Mod, Arg, Options) ->
    case where(Name) of
        undefined ->
            start_link(Mod, Arg, [{name, Name} | Options]);
        Pid ->
            {error, {already_started, Pid}}
    end.


enter_loop(Module, State, Options) ->
    ok = register_listeners(Options),
    ?MODULE:loop(#st{module=Module, state=State}, infinity).


cast(Pid, Message) ->
    Pid ! {'$couch_event_cast', Message},
    ok.


do_init(Module, Arg, Options) ->
    ok = maybe_name_process(Options),
    ok = register_listeners(Options),
    case (catch Module:init(Arg)) of
        {ok, State} ->
            ?MODULE:loop(#st{module=Module, state=State}, infinity);
        {ok, State, Timeout} when is_integer(Timeout), Timeout >= 0 ->
            ?MODULE:loop(#st{module=Module, state=State}, Timeout);
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
        undefined ->
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


do_event(#st{module=Module, state=State}=St, DbName, Event) ->
    case (catch Module:handle_event(DbName, Event, State)) of
        {ok, NewState} ->
            ?MODULE:loop(St#st{state=NewState}, infinity);
        {ok, NewState, Timeout} when is_integer(Timeout), Timeout >= 0 ->
            ?MODULE:loop(St#st{state=NewState}, Timeout);
        {stop, Reason, NewState} ->
            do_terminate(Reason, St#st{state=NewState});
        Else ->
            erlang:error(Else)
    end.


do_cast(#st{module=Module, state=State}=St, Message) ->
    case (catch Module:handle_cast(Message, State)) of
        {ok, NewState} ->
            ?MODULE:loop(St#st{state=NewState}, infinity);
        {ok, NewState, Timeout} when is_integer(Timeout), Timeout >= 0 ->
            ?MODULE:loop(St#st{state=NewState}, Timeout);
        {stop, Reason, NewState} ->
            do_terminate(Reason, St#st{state=NewState});
        Else ->
            erlang:error(Else)
    end.


do_info(#st{module=Module, state=State}=St, Message) ->
    case (catch Module:handle_info(Message, State)) of
        {ok, NewState} ->
            ?MODULE:loop(St#st{state=NewState}, infinity);
        {ok, NewState, Timeout} when is_integer(Timeout), Timeout >= 0 ->
            ?MODULE:loop(St#st{state=NewState}, Timeout);
        {stop, Reason, NewState} ->
            do_terminate(Reason, St#st{state=NewState});
        Else ->
            erlang:error(Else)
    end.


do_terminate(Reason, #st{module=Module, state=State}) ->
    % Order matters. We want to make sure Module:terminate/1
    % is called even if couch_event:unregister_all/1 hangs
    % indefinitely.
    catch Module:terminate(Reason, State),
    catch couch_event:unregister_all(self()),
    case Reason of
        normal -> ok;
        shutdown -> ok;
        ignore -> ok;
        Else -> erlang:error(Else)
    end.


where({global, Name}) -> global:safe_whereis_name(Name);
where({local, Name}) -> whereis(Name).


name_register({global, Name}=GN) ->
    case global:register_name(Name, self()) of
        yes -> true;
        no -> {false, where(GN)}
    end;
name_register({local, Name}=LN) ->
    try register(Name, self()) of
        true -> true
    catch error:_ ->
        {false, where(LN)}
    end.


get_all_dbnames(Options) ->
    case proplists:get_value(all_dbs, Options) of
        true -> all_dbs;
        false -> get_all_dbnames(Options, [])
    end.


get_all_dbnames([], []) ->
    erlang:error(no_dbnames_provided);
get_all_dbnames([], Acc) ->
    lists:usort(Acc);
get_all_dbnames([{dbname, DbName} | Rest], Acc) when is_binary(DbName) ->
    get_all_dbnames(Rest, [DbName | Acc]);
get_all_dbnames([{dbnames, DbNames} | Rest], Acc) when is_list(DbNames) ->
    BinDbNames = [DbName || DbName <- DbNames, is_binary(DbName)],
    get_all_dbnames(Rest, BinDbNames ++ Acc);
get_all_dbnames([_Ignored | Rest], Acc) ->
    get_all_dbnames(Rest, Acc).
