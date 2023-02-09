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

-module(dreyfus_blacklist_await_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("dreyfus/include/dreyfus.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(DDOC_ID, <<"_design/black_list_doc">>).
-define(INDEX_NAME, <<"my_index">>).
-define(DBNAME, <<"mydb">>).
-define(TIMEOUT, 1000).

start() ->
    test_util:start_couch([dreyfus]).

stop(_) ->
    test_util:stop_couch([dreyfus]).

setup() ->
    ok = meck:new(couch_log),
    ok = meck:expect(couch_log, notice, fun(_Fmt, _Args) ->
        ?debugFmt(_Fmt, _Args)
    end).

teardown(_) ->
    ok = meck:unload(couch_log).

dreyfus_blacklist_await_test_() ->
    {
        "dreyfus black_list_doc await tests",
        {
            setup,
            fun start/0,
            fun stop/1,
            {
                foreach,
                fun setup/0,
                fun teardown/1,
                [
                    fun do_not_await_1/0
                ]
            }
        }
    }.

do_not_await_1() ->
    ok = meck:new(dreyfus_index, [passthrough]),
    Denied = lists:flatten([
        ?b2l(?DBNAME),
        ".",
        "black_list_doc",
        ".",
        "my_index"
    ]),
    config:set("dreyfus_blacklist", Denied, "true"),
    dreyfus_test_util:wait_config_change(Denied, "true"),
    Index = #index{dbname = ?DBNAME, name = ?INDEX_NAME, ddoc_id = ?DDOC_ID},
    State = create_state(?DBNAME, Index, nil, nil, []),
    Msg = "Index Blocked from Updating - db: ~p, ddocid: ~p name: ~p",
    Return = wait_log_message(Msg, fun() ->
        {noreply, _NewState} = dreyfus_index:handle_call(
            {await, 1},
            self(),
            State
        )
    end),
    ?assertEqual(Return, ok).

wait_log_message(Fmt, Fun) ->
    ok = meck:reset(couch_log),
    Fun(),
    ok = meck:wait(couch_log, '_', [Fmt, '_'], 5000).

create_state(DbName, Index, UPid, IPid, WList) ->
    {state, DbName, Index, UPid, IPid, WList}.
