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

-module(couch_replicator_dns).

-export([
    resolve_host/1
]).

-ifdef(TEST).
-export([
    parse_config/1,
    match_pattern/2,
    get_overrides/0
]).
-endif.

-type dns_override() :: {binary(), binary()}.

-spec resolve_host(string()) -> {string(), string() | undefined}.
resolve_host(Host) ->
    case find_override(list_to_binary(Host), get_overrides()) of
        {ok, Target} ->
            {binary_to_list(Target), Host};
        not_found ->
            {Host, undefined}
    end.

-spec get_overrides() -> [dns_override()].
get_overrides() ->
    case config:get("replicator", "dns_overrides", undefined) of
        undefined ->
            [];
        ConfigStr ->
            parse_config(ConfigStr)
    end.

-spec parse_config(string()) -> [dns_override()].
parse_config(ConfigStr) ->
    ConfigBin = list_to_binary(ConfigStr),
    Entries = binary:split(ConfigBin, <<",">>, [global, trim]),
    lists:filtermap(fun parse_entry/1, Entries).

% Note: IPv6 addresses in targets must be enclosed in brackets.
% Format: pattern:target
% Valid:   *.example.com:[2001:db8::1]
% Invalid: [2001:db8::1]:proxy.internal (IPv6 as pattern not supported)
parse_entry(<<>>) ->
    false;
parse_entry(Entry0) ->
    Entry = string:trim(Entry0),
    case binary:split(Entry, <<":">>) of
        [Pattern0, Target0] ->
            Pattern = string:trim(Pattern0),
            Target = string:trim(Target0),
            case {Pattern, Target} of
                {<<>>, _} ->
                    invalid_entry(Entry);
                {_, <<>>} ->
                    invalid_entry(Entry);
                % Reject IPv6 addresses as patterns (they start with '[')
                {<<"[", _/binary>>, _} ->
                    invalid_entry_reason(Entry, "IPv6 addresses cannot be used as patterns");
                _ ->
                    {true, {Pattern, Target}}
            end;
        _ ->
            invalid_entry(Entry)
    end.

invalid_entry(Entry) ->
    couch_log:warning("Invalid dns_override entry: ~ts", [Entry]),
    false.

invalid_entry_reason(Entry, Reason) ->
    couch_log:warning("Invalid dns_override entry: ~ts (~s)", [Entry, Reason]),
    false.

find_override(_Host, []) ->
    not_found;
find_override(Host, [{Pattern, Target} | Rest]) ->
    case match_pattern(Host, Pattern) of
        true ->
            {ok, Target};
        false ->
            find_override(Host, Rest)
    end.

% DNS Override Pattern Matching
%
% Supports leading wildcard patterns only:
%   - *.example.com matches any.subdomain.example.com
%   - *.example.com does NOT match example.com (requires at least one subdomain)
%
% Not supported:
%   - middle wildcards: sub.*.example.com
%   - trailing wildcards: example.*
%   - multiple wildcards: *.*.example.com
-spec match_pattern(binary(), binary()) -> boolean().
match_pattern(Host, Pattern) when is_binary(Host), is_binary(Pattern) ->
    % DNS names are case-insensitive
    HostLower = string:lowercase(Host),
    PatternLower = string:lowercase(Pattern),
    match_pattern_impl(HostLower, PatternLower).

match_pattern_impl(Host, <<"*", Suffix/binary>>) ->
    % wildcard match: extract last N bytes from Host and compare to Suffix
    HostSize = byte_size(Host),
    SuffixSize = byte_size(Suffix),
    % ensure we have enough bytes before extracting suffix
    case HostSize >= SuffixSize of
        true ->
            Pos = HostSize - SuffixSize,
            binary:part(Host, Pos, SuffixSize) =:= Suffix;
        false ->
            false
    end;
match_pattern_impl(Host, Pattern) ->
    Host =:= Pattern.
