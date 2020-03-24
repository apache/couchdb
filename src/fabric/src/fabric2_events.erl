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

-module(fabric2_events).


-export([
    link_listener/4,
    stop_listener/1
]).

-export([
    init/2,
    poll/1
]).


-include_lib("couch/include/couch_db.hrl").


link_listener(Mod, Fun, Acc, Options) ->
    State = #{
        dbname => fabric2_util:get_value(dbname, Options),
        uuid => fabric2_util:get_value(uuid, Options, undefined),
        timeout => fabric2_util:get_value(timeout, Options, 1000),
        mod => Mod,
        callback => Fun,
        acc => Acc
    },
    Pid = spawn_link(?MODULE, init, [self(), State]),
    receive
        {Pid, initialized} -> ok
    end,
    {ok, Pid}.


stop_listener(Pid) ->
    Pid ! stop_listening.


init(Parent, #{dbname := DbName} = State) ->
    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    Since = fabric2_db:get_update_seq(Db),
    erlang:monitor(process, Parent),
    Parent ! {self(), initialized},
    poll(State#{since => Since}).


poll(#{} = State) ->
    #{
        dbname := DbName,
        uuid := DbUUID,
        timeout := Timeout,
        since := Since,
        mod := Mod,
        callback := Fun,
        acc := Acc
    } = State,
    {Resp, NewSince} = try
        Opts = [?ADMIN_CTX, {uuid, DbUUID}],
        case fabric2_db:open(DbName, Opts) of
            {ok, Db} ->
                case fabric2_db:get_update_seq(Db) of
                    Since ->
                        {{ok, Acc}, Since};
                    Other ->
                        {Mod:Fun(DbName, updated, Acc), Other}
                end;
            Error ->
                exit(Error)
        end
    catch error:database_does_not_exist ->
        Mod:Fun(DbName, deleted, Acc),
        {{stop, ok}, Since}
    end,
    receive
        stop_listening ->
            ok;
        {'DOWN', _, _, _, _} ->
            ok
    after 0 ->
        case Resp of
            {ok, NewAcc} ->
                timer:sleep(Timeout),
                NewState = State#{
                    since := NewSince,
                    acc := NewAcc
                },
                ?MODULE:poll(NewState);
            {stop, _} ->
                ok
        end
    end.
