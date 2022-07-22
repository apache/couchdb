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

-module(ddoc_cache_no_cache_test).

-include_lib("couch/include/couch_db.hrl").
-include_lib("eunit/include/eunit.hrl").

ddoc(DDocId) ->
    {ok, #doc{
        id = DDocId,
        revs = {1, [<<"deadbeefdeadbeef">>]},
        body =
            {[
                {<<"ohai">>, null}
            ]}
    }}.

not_found(_DDocId) ->
    {not_found, missing}.

return_error(_DDocId) ->
    {error, timeout}.

no_cache_test_() ->
    {
        "ddoc_cache no cache test",
        {
            setup,
            fun setup_all/0,
            fun teardown_all/1,
            {
                foreachx,
                fun setup/1,
                fun teardown/2,
                [
                    {fun ddoc/1, fun no_cache_open_ok_test/2},
                    {fun not_found/1, fun no_cache_open_not_found_test/2},
                    {fun return_error/1, fun no_cache_open_error_test/2}
                ]
            }
        }
    }.

setup_all() ->
    Ctx = ddoc_cache_tutil:start_couch(),
    meck:new(fabric),
    Ctx.

teardown_all(Ctx) ->
    meck:unload(),
    ddoc_cache_tutil:stop_couch(Ctx).

setup(Resp) ->
    meck:expect(fabric, open_doc, fun(_, DDocId, _) ->
        Resp(DDocId)
    end).

teardown(_, _) ->
    meck:unload().

no_cache_open_ok_test(_, _) ->
    Resp = ddoc_cache:open_doc(<<"foo">>, <<"bar">>),
    ?_assertEqual(ddoc(<<"bar">>), Resp).

no_cache_open_not_found_test(_, _) ->
    Resp = ddoc_cache:open_doc(<<"foo">>, <<"baz">>),
    ?_assertEqual(not_found(<<"baz">>), Resp).

no_cache_open_error_test(_, _) ->
    Resp = ddoc_cache:open_doc(<<"foo">>, <<"bif">>),
    ?_assertEqual(return_error(<<"bif">>), Resp).
