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

-export([simple/2, pbkdf2/3, pbkdf2/4, bcrypt/2, verify/2]).
-export([hash_admin_password/1, get_unhashed_admins/0]).

-include_lib("couch/include/couch_db.hrl").

-define(MAX_DERIVED_KEY_LENGTH, (1 bsl 32 - 1)).
-define(SHA1_OUTPUT_LENGTH, 20).

%% legacy scheme, not used for new passwords.
-spec simple(binary(), binary()) -> binary().
simple(Password, Salt) when is_binary(Password), is_binary(Salt) ->
    ?l2b(couch_util:to_hex(crypto:hash(sha, <<Password/binary, Salt/binary>>)));
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
    Scheme = config:get("couch_httpd_auth", "password_scheme", "pbkdf2"),
    hash_admin_password(Scheme, ClearPassword).

hash_admin_password("simple", ClearPassword) -> % deprecated
    Salt = couch_uuids:random(),
    Hash = crypto:hash(sha, <<ClearPassword/binary, Salt/binary>>),
    ?l2b("-hashed-" ++ couch_util:to_hex(Hash) ++ "," ++ ?b2l(Salt));
hash_admin_password("pbkdf2", ClearPassword) ->
    Iterations = config:get("couch_httpd_auth", "iterations", "10000"),
    Salt = couch_uuids:random(),
    DerivedKey = couch_passwords:pbkdf2(couch_util:to_binary(ClearPassword),
                                        Salt ,list_to_integer(Iterations)),
    ?l2b("-pbkdf2-" ++ ?b2l(DerivedKey) ++ ","
        ++ ?b2l(Salt) ++ ","
        ++ Iterations);
hash_admin_password("bcrypt", ClearPassword) ->
    LogRounds = list_to_integer(config:get("couch_httpd_auth", "log_rounds", "10")),
    ?l2b("-bcrypt-" ++ couch_passwords:bcrypt(couch_util:to_binary(ClearPassword), LogRounds)).

-spec get_unhashed_admins() -> list().
get_unhashed_admins() ->
    lists:filter(
        fun({_User, "-hashed-" ++ _}) ->
            false; % already hashed
        ({_User, "-pbkdf2-" ++ _}) ->
            false; % already hashed
        ({_User, "-bcrypt-" ++ _}) ->
            false; % already hashed
        ({_User, _ClearPassword}) ->
            true
        end,
    config:get("admins")).

%% Current scheme, much stronger.
-spec pbkdf2(binary(), binary(), integer()) -> binary().
pbkdf2(Password, Salt, Iterations) when is_binary(Password),
                                        is_binary(Salt),
                                        is_integer(Iterations),
                                        Iterations > 0 ->
    {ok, Result} = pbkdf2(Password, Salt, Iterations, ?SHA1_OUTPUT_LENGTH),
    Result;
pbkdf2(Password, Salt, Iterations) when is_binary(Salt),
                                        is_integer(Iterations),
                                        Iterations > 0 ->
    Msg = io_lib:format("Password value of '~p' is invalid.", [Password]),
    throw({forbidden, Msg});
pbkdf2(Password, Salt, Iterations) when is_binary(Password),
                                        is_integer(Iterations),
                                        Iterations > 0 ->
    Msg = io_lib:format("Salt value of '~p' is invalid.", [Salt]),
    throw({forbidden, Msg}).

-spec pbkdf2(binary(), binary(), integer(), integer())
    -> {ok, binary()} | {error, derived_key_too_long}.
pbkdf2(_Password, _Salt, _Iterations, DerivedLength)
    when DerivedLength > ?MAX_DERIVED_KEY_LENGTH ->
    {error, derived_key_too_long};
pbkdf2(Password, Salt, Iterations, DerivedLength) when is_binary(Password),
                                                       is_binary(Salt),
                                                       is_integer(Iterations),
                                                       Iterations > 0,
                                                       is_integer(DerivedLength) ->
    L = ceiling(DerivedLength / ?SHA1_OUTPUT_LENGTH),
    <<Bin:DerivedLength/binary,_/binary>> =
        iolist_to_binary(pbkdf2(Password, Salt, Iterations, L, 1, [])),
    {ok, ?l2b(couch_util:to_hex(Bin))}.

-spec pbkdf2(binary(), binary(), integer(), integer(), integer(), iolist())
    -> iolist().
pbkdf2(_Password, _Salt, _Iterations, BlockCount, BlockIndex, Acc)
    when BlockIndex > BlockCount ->
    lists:reverse(Acc);
pbkdf2(Password, Salt, Iterations, BlockCount, BlockIndex, Acc) ->
    Block = pbkdf2(Password, Salt, Iterations, BlockIndex, 1, <<>>, <<>>),
    pbkdf2(Password, Salt, Iterations, BlockCount, BlockIndex + 1, [Block|Acc]).

-spec pbkdf2(binary(), binary(), integer(), integer(), integer(),
    binary(), binary()) -> binary().
pbkdf2(_Password, _Salt, Iterations, _BlockIndex, Iteration, _Prev, Acc)
    when Iteration > Iterations ->
    Acc;
pbkdf2(Password, Salt, Iterations, BlockIndex, 1, _Prev, _Acc) ->
    InitialBlock = crypto:hmac(sha, Password,
        <<Salt/binary,BlockIndex:32/integer>>),
    pbkdf2(Password, Salt, Iterations, BlockIndex, 2,
        InitialBlock, InitialBlock);
pbkdf2(Password, Salt, Iterations, BlockIndex, Iteration, Prev, Acc) ->
    Next = crypto:hmac(sha, Password, Prev),
    pbkdf2(Password, Salt, Iterations, BlockIndex, Iteration + 1,
                   Next, crypto:exor(Next, Acc)).

%% Define the bcrypt functions to hash a password
-spec bcrypt(binary(), binary()) -> binary();
    (binary(), integer()) -> binary().
bcrypt(Password, Salt) when is_binary(Salt) ->
    {ok, Hash} = bcrypt:hashpw(Password, Salt),
    list_to_binary(Hash);
bcrypt(Password, LogRounds) when is_integer(LogRounds) ->
    {ok, Salt} = bcrypt:gen_salt(LogRounds),
    bcrypt(Password, list_to_binary(Salt)).

%% verify two lists for equality without short-circuits to avoid timing attacks.
-spec verify(string(), string(), integer()) -> boolean().
verify([X|RestX], [Y|RestY], Result) ->
    verify(RestX, RestY, (X bxor Y) bor Result);
verify([], [], Result) ->
    Result == 0.

-spec verify(binary(), binary()) -> boolean();
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
verify(_X, _Y) -> false.

-spec ceiling(number()) -> integer().
ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.
