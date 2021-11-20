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

-module(couch_sup).
-behaviour(supervisor).
-vsn(1).
-behaviour(config_listener).

-export([
    start_link/0,
    init/1,
    handle_config_change/5,
    handle_config_terminate/3
]).

-include_lib("couch/include/couch_db.hrl").

start_link() ->
    assert_admins(),
    maybe_launch_admin_annoyance_reporter(),
    write_pidfile(),
    notify_starting(),

    case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
        {ok, _} = Resp ->
            notify_started(),
            Resp;
        Else ->
            notify_error(Else),
            Else
    end.

init(_Args) ->
    couch_log:info("Starting ~s", [?MODULE]),
    {ok,
        {{one_for_one, 10, 60}, [
            {
                config_listener_mon,
                {config_listener_mon, start_link, [?MODULE, nil]},
                permanent,
                5000,
                worker,
                [config_listener_mon]
            },
            {
                couch_primary_services,
                {couch_primary_sup, start_link, []},
                permanent,
                infinity,
                supervisor,
                [couch_primary_sup]
            },
            {
                couch_secondary_services,
                {couch_secondary_sup, start_link, []},
                permanent,
                infinity,
                supervisor,
                [couch_secondary_sup]
            }
        ]}}.

handle_config_change("daemons", _, _, _, _) ->
    exit(whereis(?MODULE), shutdown),
    remove_handler;
handle_config_change("couchdb", "util_driver_dir", _, _, _) ->
    [Pid] = [
        P
     || {collation_driver, P, _, _} <-
            supervisor:which_children(couch_primary_services)
    ],
    Pid ! reload_driver,
    {ok, nil};
handle_config_change(_, _, _, _, _) ->
    {ok, nil}.

handle_config_terminate(_Server, _Reason, _State) ->
    ok.

assert_admins() ->
    couch_log:info("Preflight check: Asserting Admin Account~n", []),
    case {config:get("admins"), os:getenv("COUCHDB_TEST_ADMIN_PARTY_OVERRIDE")} of
        {[], false} ->
            couch_log:info(
                "~n%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n" ++
                    "  No Admin Account Found, aborting startup.                  ~n" ++
                    "  Please configure an admin account in your local.ini file.  ~n" ++
                    "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%~n",
                []
            ),
            % Wait a second so the log message can make it to the log
            timer:sleep(500),
            erlang:halt(1);
        _ ->
            ok
    end.

send_no_admin_account_error_message() ->
    couch_log:error(
        "No Admin Account configured." ++
            " Please configure an Admin Account in your local.ini file and restart CouchDB.~n",
        []
    ),
    FiveMinutes = 5 * 1000 * 60,
    timer:sleep(FiveMinutes),
    send_no_admin_account_error_message().

maybe_launch_admin_annoyance_reporter() ->
    case os:getenv("COUCHDB_TEST_ADMIN_PARTY_OVERRIDE") of
        false -> ok;
        _ -> spawn_link(fun send_no_admin_account_error_message/0)
    end.

notify_starting() ->
    couch_log:info("Apache CouchDB ~s is starting.~n", [
        couch_server:get_version()
    ]).

notify_started() ->
    couch_log:info("Apache CouchDB has started. Time to relax.~n", []).

notify_error(Error) ->
    couch_log:error("Error starting Apache CouchDB:~n~n    ~p~n~n", [Error]).

write_pidfile() ->
    case init:get_argument(pidfile) of
        {ok, [PidFile]} ->
            write_file(PidFile, os:getpid());
        _ ->
            ok
    end.

write_file(FileName, Contents) ->
    case file:write_file(FileName, Contents) of
        ok ->
            ok;
        {error, Reason} ->
            Args = [FileName, file:format_error(Reason)],
            couch_log:error("Failed ot write ~s :: ~s", Args),
            throw({error, Reason})
    end.
