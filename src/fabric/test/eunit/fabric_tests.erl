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

-module(fabric_tests).

-include_lib("couch/include/couch_eunit.hrl").

cleanup_index_files_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        fun(Ctx) ->
            [
                t_cleanup_index_files(),
                t_cleanup_index_files_with_existing_db(Ctx),
                t_cleanup_index_files_with_deleted_db(Ctx)
            ]
        end
    }.

setup() ->
    Ctx = test_util:start_couch([fabric]),
    % TempDb is deleted in the test "t_cleanup_index_files_with_deleted_db".
    TempDb = ?tempdb(),
    fabric:create_db(TempDb),
    {Ctx, TempDb}.

teardown({Ctx, _TempDb}) ->
    test_util:stop_couch(Ctx).

t_cleanup_index_files() ->
    ?_assert(
        lists:all(fun(Res) -> Res =:= ok end, fabric:cleanup_index_files())
    ).

t_cleanup_index_files_with_existing_db({_Ctx, TempDb}) ->
    ?_assertEqual(ok, fabric:cleanup_index_files(TempDb)).

t_cleanup_index_files_with_deleted_db({_Ctx, TempDb}) ->
    ?_test(
        begin
            fabric:delete_db(TempDb, []),
            ?assertError(
                database_does_not_exist,
                fabric:inactive_index_files(TempDb)
            ),
            ?assertEqual(ok, fabric:cleanup_index_files(TempDb))
        end
    ).
