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

-include_lib("eunit/include/eunit.hrl").

-define(ADMIN_ROLE, #user_ctx{roles=[<<"_admin">>]}).
-define(ADMIN_USER, {user_ctx, ?ADMIN_ROLE}).

-define(BUILDDIR,
    fun() ->
        case os:getenv("BUILDDIR") of
            false ->
                throw("BUILDDIR environment variable must be set");
            Dir ->
                Dir
        end
    end).
-define(CONFIG_CHAIN, [
    filename:join([?BUILDDIR(), "etc", "couchdb", "default_dev.ini"]),
    filename:join([?BUILDDIR(), "etc", "couchdb", "local_dev.ini"]),
    filename:join([?BUILDDIR(), "etc", "couchdb", "eunit.ini"])]).
-define(FIXTURESDIR,
    filename:join([?BUILDDIR(), "src", "couch", "test", "fixtures"])).
-define(TEMPDIR,
    filename:join([?BUILDDIR(), "test", "temp"])).

-define(tempfile,
    fun() ->
        {A, B, C} = erlang:now(),
        N = node(),
        FileName = lists:flatten(io_lib:format("~p-~p.~p.~p", [N, A, B, C])),
        filename:join([?TEMPDIR, FileName])
    end).
-define(tempdb,
    fun() ->
            Nums = tuple_to_list(erlang:now()),
            Prefix = "eunit-test-db",
            Suffix = lists:concat([integer_to_list(Num) || Num <- Nums]),
            list_to_binary(Prefix ++ "-" ++ Suffix)
    end).
-define(docid,
    fun() ->
        {A, B, C} = erlang:now(),
        lists:flatten(io_lib:format("~p~p~p", [A, B, C]))
    end).
