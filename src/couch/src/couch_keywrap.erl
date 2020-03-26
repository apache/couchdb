-module(couch_keywrap).

%% Implementation of NIST Special Publication 800-38F
%% For wrapping and unwrapping keys with AES.

-export([key_wrap/2, key_unwrap/2]).

-define(ICV1, 16#A6A6A6A6A6A6A6A6).

%% Assume old crypto api
-define(aes_ecb_encrypt(Key, Data),
        crypto:block_encrypt(aes_ecb, Key, Data)).
-define(aes_ecb_decrypt(Key, Data),
        crypto:block_decrypt(aes_ecb, Key, Data)).

%% Replace macros if new crypto api is available
-ifdef(OTP_RELEASE).
-if(?OTP_RELEASE >= 22).
-define(key_alg(Key), case bit_size(Key) of 128 -> aes_128_ecb; 192 -> aes_192_ecb; 256 -> aes_256_ecb end).
-undef(aes_ecb_encrypt).
-define(aes_ecb_encrypt(Key, Data),
        crypto:crypto_one_time(?key_alg(Key), Key, Data, true)).
-undef(aes_ecb_decrypt).
-define(aes_ecb_decrypt(Key, Data),
        crypto:crypto_one_time(?key_alg(Key), Key, Data, false)).
-endif.
-endif.

-spec key_wrap(WrappingKey :: binary(), KeyToWrap :: binary()) -> binary().
key_wrap(WrappingKey, KeyToWrap)
  when is_binary(WrappingKey), bit_size(KeyToWrap) rem 64 == 0 ->
    N = bit_size(KeyToWrap) div 64,
    wrap(WrappingKey, <<?ICV1:64>>, KeyToWrap, 1, 6 * N).

wrap(_WrappingKey, A, R, T, End) when T > End ->
    <<A/binary, R/binary>>;
wrap(WrappingKey, A, R, T, End) ->
    <<R1:64, Rest/binary>> = R,
    <<MSB_B:64, LSB_B:64>> = ?aes_ecb_encrypt(WrappingKey, <<A/binary, R1:64>>),
    wrap(WrappingKey, <<(MSB_B bxor T):64>>, <<Rest/binary, LSB_B:64>>, T + 1, End).


-spec key_unwrap(WrappingKey :: binary(), KeyToUnwrap :: binary()) -> binary() | fail.
key_unwrap(WrappingKey, KeyToUnwrap)
  when is_binary(WrappingKey), bit_size(KeyToUnwrap) rem 64 == 0 ->
    N = (bit_size(KeyToUnwrap) div 64),
    <<A:64, R/binary>> = KeyToUnwrap,
    case unwrap(WrappingKey, <<A:64>>, R, 6 * (N - 1)) of
        <<?ICV1:64, UnwrappedKey/binary>> ->
            UnwrappedKey;
        _ ->
            fail
    end.

unwrap(_WrappingKey, A, R, 0) ->
    <<A/binary, R/binary>>;
unwrap(WrappingKey, <<A:64>>, R, T) ->
    RestSize = bit_size(R) - 64,
    <<Rest:RestSize, R2: 64>> = R,
    <<MSB_B:64, LSB_B:64>> = ?aes_ecb_decrypt(WrappingKey, <<(A bxor T):64, R2:64>>),
    unwrap(WrappingKey, <<MSB_B:64>>, <<LSB_B:64, Rest:RestSize>>, T - 1).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

wrap_test_() ->
    [
     %% 128 KEK / 128 DATA
     test_wrap_unwrap(<<16#000102030405060708090A0B0C0D0E0F:128>>,
                      <<16#00112233445566778899AABBCCDDEEFF:128>>,
                      <<16#1FA68B0A8112B447AEF34BD8FB5A7B829D3E862371D2CFE5:192>>),
     %% 192 KEK / 128 DATA
     test_wrap_unwrap(<<16#000102030405060708090A0B0C0D0E0F1011121314151617:192>>,
                      <<16#00112233445566778899AABBCCDDEEFF:128>>,
                      <<16#96778B25AE6CA435F92B5B97C050AED2468AB8A17AD84E5D:192>>),
     %% 256 KEK / 128 DATA
     test_wrap_unwrap(<<16#000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F:256>>,
                      <<16#00112233445566778899AABBCCDDEEFF:128>>,
                      <<16#64E8C3F9CE0F5BA263E9777905818A2A93C8191E7D6E8AE7:192>>),
     %% 192 KEK / 192 DATA
     test_wrap_unwrap(<<16#000102030405060708090A0B0C0D0E0F1011121314151617:192>>,
                      <<16#00112233445566778899AABBCCDDEEFF0001020304050607:192>>,
                      <<16#031D33264E15D33268F24EC260743EDCE1C6C7DDEE725A936BA814915C6762D2:256>>),
     %% 256 KEK / 192 DATA
     test_wrap_unwrap(<<16#000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F:256>>,
                      <<16#00112233445566778899AABBCCDDEEFF0001020304050607:192>>,
                      <<16#A8F9BC1612C68B3FF6E6F4FBE30E71E4769C8B80A32CB8958CD5D17D6B254DA1:256>>),
     %% 256 KEK / 256 DATA
     test_wrap_unwrap(<<16#000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F:256>>,
                      <<16#00112233445566778899AABBCCDDEEFF000102030405060708090A0B0C0D0E0F:256>>,
                      <<16#28C9F404C4B810F4CBCCB35CFB87F8263F5786E2D80ED326CBC7F0E71A99F43BFB988B9B7A02DD21:320>>)].

test_wrap_unwrap(WrappingKey, KeyToWrap, ExpectedWrappedKey) ->
    [?_assertEqual(ExpectedWrappedKey, key_wrap(WrappingKey, KeyToWrap)),
     ?_assertEqual(KeyToWrap, key_unwrap(WrappingKey, key_wrap(WrappingKey, KeyToWrap)))].

fail_test() ->
    KEK = <<16#000102030405060708090A0B0C0D0E0F101112131415161718191A1B1C1D1E1F:256>>,
    CipherText = <<16#28C9F404C4B810F4CBCCB35CFB87F8263F5786E2D80ED326CBC7F0E71A99F43BFB988B9B7A02DD20:320>>,
    ?assertEqual(fail, key_unwrap(KEK, CipherText)).

-endif.
