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

-module(aegis_key_manager_test).


-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_eunit.hrl").


-define(ROOT_KEY, <<7:256>>).
-define(CB_REPLY, {ok, <<5:256>>, {my_wrapped_key, <<5:320>>}}).



basic_test_() ->
    {
        setup,
        fun setup/0,
        fun teardown/1,
        [
            {"provide state and pass through normal reply for generate_key",
            fun generate_key/0},
            {"provide state and pass through disable reply for generate_key",
            fun generate_key_encryption_disabled/0},
            {"provide state and pass through normal reply for unwrap_key",
            fun unwrap_key/0},
            {"provide state and pass through error for unwrap_key",
            fun unwrap_key_error/0}
        ]
    }.


setup() ->
    %% mock callback first, so aegis_key_manager store expected state
    meck:new([?AEGIS_KEY_MANAGER], [passthrough]),
    ok = meck:expect(?AEGIS_KEY_MANAGER, init, 0, ?ROOT_KEY),
    test_util:start_couch([fabric]).


teardown(Ctx) ->
    test_util:stop_couch(Ctx),
    meck:unload().


generate_key() ->
    ok = meck:expect(?AEGIS_KEY_MANAGER, generate_key, fun(St, _, _) ->
        ?assertEqual(?ROOT_KEY, St),
        ?CB_REPLY
    end),
    ?assertEqual(?CB_REPLY, aegis_key_manager:generate_key(#{}, [])).


generate_key_encryption_disabled() ->
    ok = meck:expect(?AEGIS_KEY_MANAGER, generate_key, fun(St, _, _) ->
        ?assertEqual(?ROOT_KEY, St),
        {ok, false}
    end),
    ?assertEqual({ok, false}, aegis_key_manager:generate_key(#{}, [])).


unwrap_key() ->
    ok = meck:expect(?AEGIS_KEY_MANAGER, unwrap_key, fun(St, _, _) ->
        ?assertEqual(?ROOT_KEY, St),
        ?CB_REPLY
    end),
    ?assertEqual(?CB_REPLY, aegis_key_manager:unwrap_key(#{}, [])).


unwrap_key_error() ->
    ok = meck:expect(?AEGIS_KEY_MANAGER, unwrap_key, fun(St, _, _) ->
        ?assertEqual(?ROOT_KEY, St),
        erlang:error(unwrap_failed)
    end),
    ?assertEqual({error,unwrap_failed}, aegis_key_manager:unwrap_key(#{}, [])).
