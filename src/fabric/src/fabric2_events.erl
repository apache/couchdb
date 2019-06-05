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
    init/5,
    poll/5
]).


-include_lib("couch/include/couch_db.hrl").


link_listener(Mod, Fun, St, Options) ->
    DbName = fabric2_util:get_value(dbname, Options),
    Pid = spawn_link(?MODULE, init, [self(), DbName, Mod, Fun, St]),
    receive
        {Pid, initialized} -> ok
    end,
    {ok, Pid}.


stop_listener(Pid) ->
    Pid ! stop_listening.


init(Parent, DbName, Mod, Fun, St) ->
    {ok, Db} = fabric2_db:open(DbName, [?ADMIN_CTX]),
    Since = fabric2_db:get_update_seq(Db),
    couch_log:error("XKCD: START LISTENER: ~s : ~p for ~p", [DbName, Since, Parent]),
    erlang:monitor(process, Parent),
    Parent ! {self(), initialized},
    poll(DbName, Since, Mod, Fun, St),
    couch_log:error("XKCD: STOP LISTENER for ~p", [Parent]).


poll(DbName, Since, Mod, Fun, St) ->
    {Resp, NewSince} = try
        case fabric2_db:open(DbName, [?ADMIN_CTX]) of
            {ok, Db} ->
                case fabric2_db:get_update_seq(Db) of
                    Since ->
                        couch_log:error("XKCD: NO UPDATE: ~s :: ~p", [DbName, Since]),
                        {{ok, St}, Since};
                    Other ->
                        couch_log:error("XKCD: UPDATED: ~s :: ~p -> ~p", [DbName, Since, Other]),
                        {Mod:Fun(DbName, updated, St), Other}
                end;
            Error ->
                exit(Error)
        end
    catch error:database_does_not_exist ->
        Mod:Fun(DbName, deleted, St)
    end,
    receive
        stop_listening ->
            ok;
        {'DOWN', _, _, _, _} ->
            ok
    after 0 ->
        case Resp of
            {ok, NewSt} ->
                timer:sleep(1000),
                ?MODULE:poll(DbName, NewSince, Mod, Fun, NewSt);
            {stop, _} ->
                ok
        end
    end.
