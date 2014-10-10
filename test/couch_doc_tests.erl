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


parse_rev_test() ->
    ?assertEqual({1, <<"123">>}, couch_doc:parse_rev("1-123")),
    ?assertEqual({1, <<"123">>}, couch_doc:parse_rev(<<"1-123">>)),
    ?assertException(throw, {bad_request, _}, couch_doc:parse_rev("1f-123")),
    ?assertException(throw, {bad_request, _}, couch_doc:parse_rev("bar")).
