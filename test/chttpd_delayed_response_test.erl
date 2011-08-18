%% Copyright 2011 Cloudant
%%
%% Licensed under the Apache License, Version 2.0 (the "License"); you may not
%% use this file except in compliance with the License. You may obtain a copy of
%% the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
%% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
%% License for the specific language governing permissions and limitations under
%% the License.

-module(chttpd_delayed_response_test).
-include_lib("eunit/include/eunit.hrl").
-include_lib("couch/include/couch_db.hrl").

all_test_() ->
    {foreach,
     fun() -> application:load(couch) end,
     fun(_) -> application:unload(couch) end,
     [
      fun delayed_chunked_response/1,
      fun delayed_chunked_response_after_error/1
     ]}.

delayed_chunked_response(_) ->
    {"sending an error first should be ok",
    fun() ->
        Req = #httpd{mochi_req=mock_request:new(nil, get, "/", {1, 1}, [])},
        {ok, Resp} = chttpd:start_delayed_chunked_response(Req, 200, []),
        ?assertMatch({ok, _}, chttpd:send_delayed_error(Resp, bad_request))
    end}.

delayed_chunked_response_after_error(_) ->
    {"sending an error midstream should throw http_abort",
    fun() ->
        Req = #httpd{mochi_req=mock_request:new(nil, get, "/", {1, 1}, [])},
        {ok, Resp} = chttpd:start_delayed_chunked_response(Req, 200, []),
        {ok, Resp1} = chttpd:send_delayed_chunk(Resp, <<>>),
        ?assertThrow({http_abort, _, _}, chttpd:send_delayed_error(Resp1, bad_request))
    end}.
