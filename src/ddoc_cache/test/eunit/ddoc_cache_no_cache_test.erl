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
        body = {[
            {<<"ohai">>, null}
        ]}
    }}.


not_found(_DDocId) ->
    {not_found, missing}.


return_error(_DDocId) ->
    {error, timeout}.


start(Resp) ->
    Ctx = ddoc_cache_tutil:start_couch(),
    meck:new(fabric),
    meck:expect(fabric, open_doc, fun(_, DDocId, _) ->
        Resp(DDocId)
    end),
    Ctx.


stop(Ctx) ->
    meck:unload(),
    ddoc_cache_tutil:stop_couch(Ctx).


no_cache_open_ok_test() ->
    Ctx = start(fun ddoc/1),
    try
        Resp = ddoc_cache:open_doc(<<"foo">>, <<"bar">>),
        ?assertEqual(ddoc(<<"bar">>), Resp)
    after
        stop(Ctx)
    end.


no_cache_open_not_found_test() ->
    Ctx = start(fun not_found/1),
    try
        Resp = ddoc_cache:open_doc(<<"foo">>, <<"bar">>),
        ?assertEqual(not_found(<<"bar">>), Resp)
    after
        stop(Ctx)
    end.


no_cache_open_error_test() ->
    Ctx = start(fun return_error/1),
    try
        Resp = ddoc_cache:open_doc(<<"foo">>, <<"bar">>),
        ?assertEqual(return_error(<<"bar">>), Resp)
    after
        stop(Ctx)
    end.
