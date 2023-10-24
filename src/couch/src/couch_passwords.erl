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

-module(couch_passwords).

-export([simple/2, pbkdf2/3, pbkdf2/4, verify/2]).
-export([hash_admin_password/1, get_unhashed_admins/0]).

-include_lib("couch/include/couch_db.hrl").

-define(MAX_DERIVED_KEY_LENGTH, (1 bsl 32 - 1)).
-define(SHA1_OUTPUT_LENGTH, 20).

%% legacy scheme, not used for new passwords.
-spec simple(binary(), binary()) -> binary().
simple(Password, Salt) when is_binary(Password), is_binary(Salt) ->
    couch_util:to_hex_bin(crypto:hash(sha, <<Password/binary, Salt/binary>>));
simple(Password, Salt) when is_binary(Salt) ->
    Msg = io_lib:format("Password value of '~p' is invalid.", [Password]),
    throw({forbidden, Msg});
simple(Password, Salt) when is_binary(Password) ->
    Msg = io_lib:format("Salt value of '~p' is invalid.", [Salt]),
    throw({forbidden, Msg}).

%% CouchDB utility functions
-spec hash_admin_password(binary() | list()) -> binary().
hash_admin_password(ClearPassword) when is_list(ClearPassword) ->
    hash_admin_password(?l2b(ClearPassword));
hash_admin_password(ClearPassword) when is_binary(ClearPassword) ->
    %% Support both schemes to smooth migration from legacy scheme
    Scheme = chttpd_util:get_chttpd_auth_config("password_scheme", "pbkdf2"),
    hash_admin_password(Scheme, ClearPassword).

% deprecated
hash_admin_password("simple", ClearPassword) ->
    Salt = couch_uuids:random(),
    Hash = crypto:hash(sha, <<ClearPassword/binary, Salt/binary>>),
    ?l2b("-hashed-" ++ couch_util:to_hex(Hash) ++ "," ++ ?b2l(Salt));
hash_admin_password("pbkdf2", ClearPassword) ->
    Iterations = chttpd_util:get_chttpd_auth_config("iterations", "10"),
    Salt = couch_uuids:random(),
    DerivedKey = couch_passwords:pbkdf2(
        couch_util:to_binary(ClearPassword),
        Salt,
        list_to_integer(Iterations)
    ),
    ?l2b(
        "-pbkdf2-" ++ ?b2l(DerivedKey) ++ "," ++
            ?b2l(Salt) ++ "," ++
            Iterations
    ).

-spec get_unhashed_admins() -> list().
get_unhashed_admins() ->
    lists:filter(
        fun
            ({_User, "-hashed-" ++ _}) ->
                % already hashed
                false;
            ({_User, "-pbkdf2-" ++ _}) ->
                % already hashed
                false;
            ({_User, _ClearPassword}) ->
                true
        end,
        config:get("admins")
    ).

%% Current scheme, much stronger.
-spec pbkdf2(binary(), binary(), integer()) -> binary().
pbkdf2(Password, Salt, Iterations) when
    is_binary(Password),
    is_binary(Salt),
    is_integer(Iterations),
    Iterations > 0
->
    {ok, Result} = pbkdf2(Password, Salt, Iterations, ?SHA1_OUTPUT_LENGTH),
    Result;
pbkdf2(Password, Salt, Iterations) when
    is_binary(Salt),
    is_integer(Iterations),
    Iterations > 0
->
    Msg = io_lib:format("Password value of '~p' is invalid.", [Password]),
    throw({forbidden, Msg});
pbkdf2(Password, Salt, Iterations) when
    is_binary(Password),
    is_integer(Iterations),
    Iterations > 0
->
    Msg = io_lib:format("Salt value of '~p' is invalid.", [Salt]),
    throw({forbidden, Msg}).

-spec pbkdf2(binary(), binary(), integer(), integer()) ->
    {ok, binary()} | {error, derived_key_too_long}.
pbkdf2(_Password, _Salt, _Iterations, DerivedLength) when
    DerivedLength > ?MAX_DERIVED_KEY_LENGTH
->
    {error, derived_key_too_long};
pbkdf2(Password, Salt, Iterations, DerivedLength) when
    is_binary(Password),
    is_binary(Salt),
    is_integer(Iterations),
    Iterations > 0,
    is_integer(DerivedLength)
->
    DerivedKey = fast_pbkdf2:pbkdf2(sha, Password, Salt, Iterations, DerivedLength),
    {ok, couch_util:to_hex_bin(DerivedKey)}.

%% verify two lists for equality without short-circuits to avoid timing attacks.
-if((?OTP_RELEASE) >= 25).
verify(ListA, ListB) when is_list(ListA), is_list(ListB) ->
    verify(?l2b(ListA), ?l2b(ListB));
verify(BinA, BinB) when is_binary(BinA), is_binary(BinB), byte_size(BinA) == byte_size(BinB) ->
    crypto:hash_equals(BinA, BinB);
verify(BinA, BinB) when is_binary(BinA), is_binary(BinB) ->
    false.
-else.
-spec verify(string(), string(), integer()) -> boolean().
verify([X | RestX], [Y | RestY], Result) ->
    verify(RestX, RestY, (X bxor Y) bor Result);
verify([], [], Result) ->
    Result == 0.

-spec verify
    (binary(), binary()) -> boolean();
    (list(), list()) -> boolean().
verify(<<X/binary>>, <<Y/binary>>) ->
    verify(?b2l(X), ?b2l(Y));
verify(X, Y) when is_list(X) and is_list(Y) ->
    case length(X) == length(Y) of
        true ->
            verify(X, Y, 0);
        false ->
            false
    end;
verify(_X, _Y) ->
    false.
-endif.
