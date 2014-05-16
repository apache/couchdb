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

-module(couch_util_tests).

-include("couch_eunit.hrl").


setup() ->
    %% We cannot start driver from here since it becomes bounded to eunit
    %% master process and the next couch_server_sup:start_link call will
    %% fail because server couldn't load driver since it already is.
    %%
    %% On other hand, we cannot unload driver here due to
    %% {error, not_loaded_by_this_process} while it is. Any ideas is welcome.
    %%
    couch_server_sup:start_link(?CONFIG_CHAIN),
    %% couch_config:start_link(?CONFIG_CHAIN),
    %% {ok, _} = couch_drv:start_link(),
    ok.

teardown(_) ->
    couch_server_sup:stop(),
    %% couch_config:stop(),
    %% erl_ddll:unload_driver(couch_icu_driver),
    ok.


collation_test_() ->
    {
        "Collation tests",
        [
            {
                setup,
                fun setup/0, fun teardown/1,
                [
                    should_collate_ascii(),
                    should_collate_non_ascii()
                ]
            }
        ]
    }.

should_collate_ascii() ->
    ?_assertEqual(1, couch_util:collate(<<"foo">>, <<"bar">>)).

should_collate_non_ascii() ->
    ?_assertEqual(-1, couch_util:collate(<<"A">>, <<"aa">>)).
