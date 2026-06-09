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

% Thin wrapper for Erlang's zstd (available in OTP 28+)

-module(couch_zstd).

-export([
    available/0,
    compress/2,
    decompress/1,
    uncompressed_size/1
]).

-if(?OTP_RELEASE >= 28).

available() ->
    true.

compress(Term, Level) when is_integer(Level) ->
    % Since compress/2 can take an iovec and, term_to_iovec/2 exists, plug into
    % the other for a potential performance win
    Iovec = term_to_iovec(Term, [{minor_version, 1}]),
    iolist_to_binary(zstd:compress(Iovec, #{compressionLevel => Level})).

decompress(Bin) when is_binary(Bin) ->
    binary_to_term(iolist_to_binary(zstd:decompress(Bin))).

uncompressed_size(Bin) when is_binary(Bin) ->
    {ok, #{frameContentSize := Size}} = zstd:get_frame_header(Bin),
    Size.

-else.

available() ->
    false.

compress(_Bin, _Level) ->
    erlang:error(zstd_unavailable).

decompress(_Bin) ->
    erlang:error(zstd_unavailable).

uncompressed_size(_Bin) ->
    erlang:error(zstd_unavailable).

-endif.
