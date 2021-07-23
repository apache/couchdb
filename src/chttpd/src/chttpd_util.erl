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

-module(chttpd_util).


-export([
    maybe_add_csp_header/3
]).


maybe_add_csp_header(Component, OriginalHeaders, DefaultHeaderValue) ->
    Default = case Component of
        "utils" -> true;
         _Other -> false
    end,
    Enabled = config:get_boolean("csp", Component ++ "_enable", Default),
    case Enabled of
        true ->
            HeaderValue = config:get("csp", Component ++ "_header_value", DefaultHeaderValue),
            % As per https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Content-Security-Policy#multiple_content_security_policies
            % The top most CSP header defines the most open policy,
            % subsequent CSP headers set by show/list functions can
            % only further restrict the policy.
            %
            % Ours goes on top and we don’t have to worry about additional
            % headers set by users.
            [{"Content-Security-Policy", HeaderValue} | OriginalHeaders];
        false ->
            % Fallback for old config vars
            case Component of
                "utils" ->
                    handle_legacy_config(OriginalHeaders, DefaultHeaderValue);
                _ ->
                    OriginalHeaders
            end
    end.

handle_legacy_config(OriginalHeaders, DefaultHeaderValue) ->
    LegacyUtilsEnabled = config:get_boolean("csp", "enable", true),
    case LegacyUtilsEnabled of
        true ->
            LegacyUtilsHeaderValue = config:get("csp", "header_value", DefaultHeaderValue),
            [{"Content-Security-Policy", LegacyUtilsHeaderValue} | OriginalHeaders];
        false ->
            OriginalHeaders
    end.
