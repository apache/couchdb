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

-module(couch_base32_tests).

-include_lib("eunit/include/eunit.hrl").

base32_test() ->
    roundtrip(<<"">>, <<"">>),
    roundtrip(<<"f">>, <<"MY======">>),
    roundtrip(<<"fo">>, <<"MZXQ====">>),
    roundtrip(<<"foo">>, <<"MZXW6===">>),
    roundtrip(<<"foob">>, <<"MZXW6YQ=">>),
    roundtrip(<<"fooba">>, <<"MZXW6YTB">>),
    roundtrip(<<"foobar">>, <<"MZXW6YTBOI======">>).

roundtrip(Plain, Encoded) ->
    ?assertEqual(Plain, couch_base32:decode(Encoded)),
    ?assertEqual(Encoded, couch_base32:encode(Plain)).
