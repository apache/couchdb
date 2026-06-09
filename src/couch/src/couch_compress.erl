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

-module(couch_compress).

-export([compress/2, decompress/1, is_compressed/2]).
-export([get_compression_method/0]).
-export([uncompressed_size/1]).

-include_lib("couch/include/couch_db.hrl").

% binaries compressed with snappy have their first byte set to this value
-define(SNAPPY_PREFIX, 1).
% Term prefixes documented at:
%      http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
-define(TERM_PREFIX, 131).
-define(COMPRESSED_TERM_PREFIX, 131, 80).
% Zstandard frame magic number = 0xFD2FB528 in little endian(!)
-define(ZSTD_MAGIC, 16#28, 16#B5, 16#2F, 16#FD).
-define(ZSTD_DEFAULT_LEVEL, 3).

get_compression_method() ->
    case config:get("couchdb", "file_compression") of
        undefined ->
            ?DEFAULT_COMPRESSION;
        Method1 ->
            Method =
                case string:tokens(Method1, "_") of
                    [M] ->
                        list_to_existing_atom(M);
                    [M, Level] ->
                        {list_to_existing_atom(M), list_to_integer(Level)}
                end,
            maybe_zstd(Method)
    end.

maybe_zstd(zstd) ->
    maybe_zstd({zstd, ?ZSTD_DEFAULT_LEVEL});
maybe_zstd({zstd, _} = Method) ->
    % zstd needs OTP 28+. On older releases fall back to deflate at a
    % comparable level (zstd's default is also 3) so writes still compress.
    case couch_zstd:available() of
        true -> Method;
        false -> {deflate, 3}
    end;
maybe_zstd(Method) ->
    Method.

compress(<<?SNAPPY_PREFIX, _/binary>> = Bin, snappy) ->
    Bin;
compress(<<?SNAPPY_PREFIX, _/binary>> = Bin, Method) ->
    compress(decompress(Bin), Method);
compress(<<?COMPRESSED_TERM_PREFIX, _/binary>> = Bin, {deflate, _Level}) ->
    Bin;
compress(<<?TERM_PREFIX, _/binary>> = Bin, Method) ->
    compress(decompress(Bin), Method);
compress(<<?ZSTD_MAGIC, _/binary>> = Bin, {zstd, _Level}) ->
    Bin;
compress(<<?ZSTD_MAGIC, _/binary>> = Bin, Method) ->
    compress(decompress(Bin), Method);
compress(Term, none) ->
    ?term_to_bin(Term);
compress(Term, {deflate, Level}) ->
    term_to_binary(Term, [{minor_version, 1}, {compressed, Level}]);
compress(Term, {zstd, Level}) ->
    couch_zstd:compress(Term, Level);
compress(Term, snappy) ->
    Bin = ?term_to_bin(Term),
    try
        {ok, CompressedBin} = snappy:compress(Bin),
        <<?SNAPPY_PREFIX, CompressedBin/binary>>
    catch
        exit:snappy_nif_not_loaded ->
            Bin
    end.

decompress(<<?SNAPPY_PREFIX, Rest/binary>>) ->
    {ok, TermBin} = snappy:decompress(Rest),
    binary_to_term(TermBin);
decompress(<<?ZSTD_MAGIC, _/binary>> = Bin) ->
    couch_zstd:decompress(Bin);
decompress(<<?TERM_PREFIX, _/binary>> = Bin) ->
    binary_to_term(Bin);
decompress(_) ->
    error(invalid_compression).

is_compressed(<<?SNAPPY_PREFIX, _/binary>>, Method) ->
    Method =:= snappy;
is_compressed(<<?COMPRESSED_TERM_PREFIX, _/binary>>, {deflate, _Level}) ->
    true;
is_compressed(<<?COMPRESSED_TERM_PREFIX, _/binary>>, _Method) ->
    false;
is_compressed(<<?ZSTD_MAGIC, _/binary>>, {zstd, _Level}) ->
    true;
is_compressed(<<?ZSTD_MAGIC, _/binary>>, _Method) ->
    false;
is_compressed(<<?TERM_PREFIX, _/binary>>, Method) ->
    Method =:= none;
is_compressed(Term, _Method) when not is_binary(Term) ->
    false;
is_compressed(_, _) ->
    error(invalid_compression).

uncompressed_size(<<?SNAPPY_PREFIX, Rest/binary>>) ->
    {ok, Size} = snappy:uncompressed_length(Rest),
    Size;
uncompressed_size(<<?ZSTD_MAGIC, _/binary>> = Bin) ->
    couch_zstd:uncompressed_size(Bin);
uncompressed_size(<<?COMPRESSED_TERM_PREFIX, Size:32, _/binary>> = _Bin) ->
    % See http://erlang.org/doc/apps/erts/erl_ext_dist.html
    % The uncompressed binary would be encoded with <<131, Rest/binary>>
    % so need to add 1 for 131
    Size + 1;
uncompressed_size(<<?TERM_PREFIX, _/binary>> = Bin) ->
    byte_size(Bin);
uncompressed_size(_) ->
    error(invalid_compression).
