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

-module(couch_mrview_index_info_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

-define(TIMEOUT, 1000).


-ifdef(run_broken_tests).

setup() ->
    {ok, Db} = couch_mrview_test_util:init_db(?tempdb(), map),
    couch_mrview:query_view(Db, <<"_design/bar">>, <<"baz">>),
    {ok, Info} = couch_mrview:get_info(Db, <<"_design/bar">>),
    {Db, Info}.

teardown({Db, _}) ->
    couch_db:close(Db),
    couch_server:delete(couch_db:name(Db), [?ADMIN_CTX]),
    ok.


view_info_test_() ->
    {
        "Views index tests",
        {
            setup,
            fun test_util:start_couch/0, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun should_get_property/1
                ]
            }
        }
    }.


should_get_property({_, Info}) ->
    InfoProps = [
        {signature, <<"276df562b152b3c4e5d34024f62672ed">>},
        {language, <<"javascript">>},
        {disk_size, 314},
        {data_size, 263},
        {update_seq, 11},
        {purge_seq, 0},
        {updater_running, false},
        {compact_running, false},
        {waiting_clients, 0}
    ],
    [
        {atom_to_list(Key), ?_assertEqual(Val, getval(Key, Info))}
        || {Key, Val} <- InfoProps
    ].


getval(Key, PL) ->
    {value, {Key, Val}} = lists:keysearch(Key, 1, PL),
    Val.


-endif.
