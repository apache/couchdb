-module(erl_pbkdf2).

-export([pbkdf2_oneblock/4]).

-type sha_type() :: crypto:sha1() | crypto:sha2().

-spec pbkdf2_oneblock(sha_type(), binary(), binary(), non_neg_integer()) -> binary().
pbkdf2_oneblock(Sha, Password, Salt, 1) ->
    crypto_hmac(Sha, Password, <<Salt/binary, 0, 0, 0, 1>>);
pbkdf2_oneblock(Sha, Password, Salt, IterationCount) when
    is_integer(IterationCount), IterationCount > 1
->
    U1 = crypto_hmac(Sha, Password, <<Salt/binary, 0, 0, 0, 1>>),
    mask(U1, iteration(Sha, Password, U1, IterationCount - 1)).

-spec iteration(sha_type(), binary(), binary(), non_neg_integer()) -> binary().
iteration(Sha, Password, UPrev, 1) ->
    crypto_hmac(Sha, Password, UPrev);
iteration(Sha, Password, UPrev, IterationCount) ->
    U = crypto_hmac(Sha, Password, UPrev),
    mask(U, iteration(Sha, Password, U, IterationCount - 1)).

-spec mask(binary(), binary()) -> binary().
mask(Key, Data) ->
    KeySize = size(Key) * 8,
    <<A:KeySize>> = Key,
    <<B:KeySize>> = Data,
    C = A bxor B,
    <<C:KeySize>>.

-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 23).
crypto_hmac(Sha, Bin1, Bin2) ->
    crypto:mac(hmac, Sha, Bin1, Bin2).
-else.
crypto_hmac(Sha, Bin1, Bin2) ->
    crypto:hmac(Sha, Bin1, Bin2).
-endif.
-else.
crypto_hmac(Sha, Bin1, Bin2) ->
    crypto:hmac(Sha, Bin1, Bin2).
-endif.
