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

-module(couch_doc_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch/include/couch_db.hrl").


-define(REQUEST_FIXTURE,
    filename:join([?FIXTURESDIR, "multipart.http"])).

parse_rev_test() ->
    ?assertEqual({1, <<"123">>}, couch_doc:parse_rev("1-123")),
    ?assertEqual({1, <<"123">>}, couch_doc:parse_rev(<<"1-123">>)),
    ?assertException(throw, {bad_request, _}, couch_doc:parse_rev("1f-123")),
    ?assertException(throw, {bad_request, _}, couch_doc:parse_rev("bar")).

doc_from_multi_part_stream_test() ->
    ContentType = "multipart/related;boundary=multipart_related_boundary~~~~~~~~~~~~~~~~~~~~",
    DataFun = fun() -> request(start) end,

    {ok, #doc{id = <<"doc0">>, atts = [_]} = Doc, _Fun, _Parser} =
        couch_doc:doc_from_multi_part_stream(ContentType, DataFun),
    ok.

request(start) ->
    {ok, Doc} = file:read_file(?REQUEST_FIXTURE),
    {Doc, fun() -> request(stop) end};
request(stop) ->
    {"", fun() -> request(stop) end}.
