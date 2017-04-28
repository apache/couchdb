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

-module(couch_replicator_proxy_tests).

-include_lib("couch/include/couch_eunit.hrl").
-include_lib("couch_replicator/src/couch_replicator.hrl").
-include_lib("couch_replicator/src/couch_replicator_api_wrap.hrl").


setup() ->
    ok.


teardown(_) ->
    ok.


replicator_proxy_test_() ->
    {
        "replicator proxy tests",
        {
            setup,
            fun() -> test_util:start_couch([couch_replicator]) end, fun test_util:stop_couch/1,
            {
                foreach,
                fun setup/0, fun teardown/1,
                [
                    fun parse_rep_doc_without_proxy/1,
                    fun parse_rep_doc_with_proxy/1
                ]
            }
        }
    }.


parse_rep_doc_without_proxy(_) ->
    ?_test(begin
        NoProxyDoc = {[
            {<<"source">>, <<"http://unproxied.com">>},
            {<<"target">>, <<"http://otherunproxied.com">>}
        ]},
        Rep = couch_replicator_docs:parse_rep_doc(NoProxyDoc),
        ?assertEqual((Rep#rep.source)#httpdb.proxy_url, undefined),
        ?assertEqual((Rep#rep.target)#httpdb.proxy_url, undefined)
    end).


parse_rep_doc_with_proxy(_) ->
    ?_test(begin
        ProxyURL = <<"http://myproxy.com">>,
        ProxyDoc = {[
            {<<"source">>, <<"http://unproxied.com">>},
            {<<"target">>, <<"http://otherunproxied.com">>},
            {<<"proxy">>, ProxyURL}
        ]},
        Rep = couch_replicator_docs:parse_rep_doc(ProxyDoc),
        ?assertEqual((Rep#rep.source)#httpdb.proxy_url, binary_to_list(ProxyURL)),
        ?assertEqual((Rep#rep.target)#httpdb.proxy_url, binary_to_list(ProxyURL))
    end).
