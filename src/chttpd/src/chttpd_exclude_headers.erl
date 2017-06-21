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

-module(chttpd_exclude_headers).



-export([maybe_exclude_headers/2]).



-include_lib("couch/include/couch_db.hrl").

maybe_exclude_headers(#httpd{mochi_req = MochiReq}, Headers) ->
    case MochiReq:get_header_value("X-Couch-Exclude-Headers") of
        "All" -> 
            filter_headers(Headers, get_header_list("all"));
        "Minimal"  ->
            filter_headers(Headers, get_header_list("minimal"));
        _ -> 
            Headers
    end.



filter_headers(Headers, IncludeList) ->
    lists:filter(fun({HeaderName, _}) -> 
        lists:member(HeaderName, IncludeList)
    end, Headers).



get_header_list(Section) ->
    SectionStr = config:get("exclude_headers", Section, []),
    split_list(SectionStr).



split_list(S) ->
    re:split(S, "\\s*,\\s*", [trim, {return, list}]). 
