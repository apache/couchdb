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

-module(couch_etag_tests).

-include_lib("eunit/include/eunit.hrl").

local_with_empty_body_test() ->
    Etag = couch_httpd:doc_etag(<<"_local/local-and-empty">>, {[]}, {0, <<"1">>}),
    ?assertEqual(Etag, <<"\"5ZVXQYO7VLEOU0TL9VXDNP5PV\"">>).

local_with_body_test() ->
    DocBody = {[{<<"hello">>, <<"world">>}, {<<"relax">>, true}]},
    Etag = couch_httpd:doc_etag(<<"_local/local-with-body">>, DocBody, {0, <<"1">>}),
    ?assertEqual(Etag, <<"\"CEFXP6WH8OKYIWO1GLGBHKCCA\"">>).

normal_doc_uses_rev_test() ->
    DocBody = {[{<<"hello">>, <<"world">>}, {<<"relax">>, true}]},
    Etag = couch_httpd:doc_etag(
        <<"nomal-doc">>, DocBody, {1, <<"efda11e34e88ebe31a2f83e84a0435b6">>}
    ),
    ?assertEqual(Etag, <<"\"1-efda11e34e88ebe31a2f83e84a0435b6\"">>).
