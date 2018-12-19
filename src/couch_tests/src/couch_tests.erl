% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(couch_tests).

-export([
    new/4,
    setup/1,
    setup/3,
    teardown/1
]).

-export([
    start_applications/2,
    stop_applications/2
]).

-export([
    get/2,
    get_state/2,
    set_state/3
]).

-export([
    validate/1,
    validate_and_report/1
]).

-export([
    validate_fixture/1,
    validate_fixture/3
]).

-include_lib("couch_tests/include/couch_tests.hrl").

%% ------------------------------------------------------------------
%% API functions definitions
%% ------------------------------------------------------------------

new(Module, FixtureId, Setup, Teardown) ->
    #couch_tests_fixture{
        module = Module,
        id = FixtureId,
        setup = Setup,
        teardown = Teardown
    }.

setup(Chain) ->
    setup(Chain, [], []).

setup(Chain, Args, Opts) ->
    Ctx = #couch_tests_ctx{chain = Chain, args = Args, opts = Opts},
    do_setup(Chain, Ctx, []).

teardown(#couch_tests_ctx{chain = Chain} = Ctx0) ->
    Ctx1 = lists:foldl(fun do_teardown/2, Ctx0, lists:reverse(Chain)),
    ToStop = lists:reverse(Ctx1#couch_tests_ctx.started_apps),
    stop_applications(ToStop, Ctx1).

start_applications(Apps, Ctx) when is_list(Apps) ->
    #couch_tests_ctx{
        started_apps = Running
    } = Ctx,
    Started = start_applications(Apps),
    Ctx#couch_tests_ctx{started_apps = Running ++ Started}.

stop_applications(Apps, Ctx) when is_list(Apps) ->
    #couch_tests_ctx{
        started_apps = Started,
        stopped_apps = Stopped
    } = Ctx,
    JustStopped = stop_applications(Apps -- Stopped),
    Ctx#couch_tests_ctx{
        started_apps = Started -- JustStopped,
        stopped_apps = remove_duplicates(Stopped ++ JustStopped)
    }.

get_state(#couch_tests_fixture{module = Module, id = Id}, Ctx) ->
    dict:fetch({Module, Id}, Ctx#couch_tests_ctx.dict).

set_state(Fixture, Ctx, State) ->
    #couch_tests_fixture{
        module = Module,
        id = Id
    } = Fixture,
    Dict = dict:store({Module, Id}, State, Ctx#couch_tests_ctx.dict),
    Ctx#couch_tests_ctx{dict = Dict}.

get(started_apps, #couch_tests_ctx{started_apps = Started}) ->
    Started;
get(stopped_apps, #couch_tests_ctx{stopped_apps = Stopped}) ->
    Stopped.

validate_fixture(#couch_tests_fixture{} = Fixture) ->
    validate_fixture(Fixture, [], []).

validate_fixture(#couch_tests_fixture{} = Fixture0, Args, Opts) ->
    AppsBefore = applications(),
    #couch_tests_ctx{chain = [Fixture1]} = Ctx0 = setup([Fixture0], Args, Opts),
    AppsWhile = applications(),
    Ctx1 = teardown(Ctx0),
    AppsAfter = applications(),
    AppsStarted = lists:usort(AppsWhile -- AppsBefore),
    FixtureApps = lists:usort(Fixture1#couch_tests_fixture.apps),
    StartedAppsBeforeTeardown = lists:usort(Ctx0#couch_tests_ctx.started_apps),
    StoppedAppsAfterTeardown = lists:usort(Ctx1#couch_tests_ctx.stopped_apps),
    StartedAppsAfterTeardown = Ctx1#couch_tests_ctx.started_apps,

    validate_and_report([
         {equal, "Expected applications before calling fixture (~p) "
            "to be equal to applications after its calling",
            AppsBefore, AppsAfter},
         {equal, "Expected list of started applications (~p) "
            "to be equal to #couch_tests_fixture.apps (~p)",
            AppsStarted, FixtureApps},
         {equal, "Expected list of started applications (~p) "
            "to be equal to #couch_tests_ctx.started_apps (~p)",
            AppsStarted, StartedAppsBeforeTeardown},
         {equal, "Expected list of stopped applications (~p) "
            "to be equal to #couch_tests_ctx.stopped_apps (~p)",
            AppsStarted, StoppedAppsAfterTeardown},
         {equal, "Expected empty list ~i of #couch_tests_ctx.started_apps (~p) "
            "after teardown", [], StartedAppsAfterTeardown}
    ]).

validate(Sheet) ->
    case lists:foldl(fun do_validate/2, [], Sheet) of
        [] -> true;
        Errors -> Errors
    end.

validate_and_report(Sheet) ->
    case validate(Sheet) of
        true ->
            true;
        Errors ->
            [io:format(user, "    ~s~n", [Err]) || Err <- Errors],
            false
    end.

%% ------------------------------------------------------------------
%% Helper functions definitions
%% ------------------------------------------------------------------


do_setup([#couch_tests_fixture{setup = Setup} = Fixture | Rest], Ctx0, Acc) ->
    Ctx1 = Ctx0#couch_tests_ctx{started_apps = []},
    #couch_tests_ctx{started_apps = Apps} = Ctx2 = Setup(Fixture, Ctx1),
    Ctx3 = Ctx2#couch_tests_ctx{started_apps = []},
    do_setup(Rest, Ctx3, [Fixture#couch_tests_fixture{apps = Apps} | Acc]);
do_setup([], Ctx, Acc) ->
    Apps = lists:foldl(fun(#couch_tests_fixture{apps = A}, AppsAcc) ->
        A ++ AppsAcc
    end, [], Acc),
    Ctx#couch_tests_ctx{chain = lists:reverse(Acc), started_apps = Apps}.

do_teardown(Fixture, Ctx0) ->
    #couch_tests_fixture{teardown = Teardown, apps = Apps} = Fixture,
    #couch_tests_ctx{} = Ctx1 = Teardown(Fixture, Ctx0),
    stop_applications(lists:reverse(Apps), Ctx1).

start_applications(Apps) ->
    do_start_applications(Apps, []).

do_start_applications([], Acc) ->
    lists:reverse(Acc);
do_start_applications([App | Apps], Acc) ->
    case application:start(App) of
    {error, {already_started, _}} ->
        do_start_applications(Apps, Acc);
    {error, {not_started, Dep}} ->
        do_start_applications([Dep, App | Apps], Acc);
    {error, {not_running, Dep}} ->
        do_start_applications([Dep, App | Apps], Acc);
    ok ->
        do_start_applications(Apps, [App | Acc])
    end.

stop_applications(Apps) ->
    do_stop_applications(Apps, []).

do_stop_applications([], Acc) ->
    lists:reverse(Acc);
do_stop_applications([App | Apps], Acc) ->
    case application:stop(App) of
    {error, _} ->
        do_stop_applications(Apps, Acc);
    ok ->
        do_stop_applications(Apps, [App | Acc])
    end.

remove_duplicates([])    ->
    [];
remove_duplicates([H | T]) ->
    [H | [X || X <- remove_duplicates(T), X /= H]].

applications() ->
    lists:usort([App || {App, _, _} <-application:which_applications()]).

do_validate({equal, _Message, Arg, Arg}, Acc) ->
    Acc;
do_validate({equal, Message, Arg1, Arg2}, Acc) ->
    [io_lib:format(Message, [Arg1, Arg2]) | Acc].


%% ------------------------------------------------------------------
%% Tests
%% ------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

validate_test() ->
    ?assertMatch("1 == 2", lists:flatten(validate([{equal, "~w == ~w", 1, 2}]))),
    ?assertMatch("2", lists:flatten(validate([{equal, "~i~w", 1, 2}]))),
    ?assert(validate([{equal, "~w == ~w", 1, 1}])),
    ok.

-endif.
