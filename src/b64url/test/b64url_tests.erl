-module(b64url_tests).

-include_lib("eunit/include/eunit.hrl").

-define(MAX_SIZE, 6401).
-define(NUM_TESTS, 500).

table_test() ->
    ?assertEqual(ok, b64url:check_tables()).

encode_binary_test() ->
    lists:foreach(
        fun(_) ->
            Bin = gen_binary(),
            A = couch_encode_base64url(Bin),
            B = b64url:encode(Bin),
            ?assertEqual(A, B)
        end,
        lists:seq(1, ?NUM_TESTS)
    ).

encode_iolist_test() ->
    lists:foreach(
        fun(_) ->
            IoList = shallow_iolist(),
            A = couch_encode_base64url(iolist_to_binary(IoList)),
            B = b64url:encode(IoList),
            ?assertEqual(A, B)
        end,
        lists:seq(1, ?NUM_TESTS)
    ).

decode_binary_test() ->
    lists:foreach(
        fun(_) ->
            Bin = gen_binary(),
            B64UrlBin = couch_encode_base64url(Bin),
            Dec = b64url:decode(B64UrlBin),
            ?assertEqual(Bin, Dec)
        end,
        lists:seq(1, ?NUM_TESTS)
    ).

decode_iolist_test() ->
    lists:foreach(
        fun(_) ->
            IoList = shallow_b64_iolist(),
            A = couch_decode_base64url(iolist_to_binary(IoList)),
            B = b64url:decode(IoList),
            ?assertEqual(A, B)
        end,
        lists:seq(1, ?NUM_TESTS)
    ).

decode_binary_error_test() ->
    lists:foreach(
        fun(_) ->
            {ErrBin, BlockPos} = bad_binary(),
            Dec = b64url:decode(ErrBin),
            ?assertEqual({error, {bad_block, BlockPos}}, Dec)
        end,
        lists:seq(1, ?NUM_TESTS)
    ).

decode_bad_length_test() ->
    lists:foreach(
        fun(_) ->
            Bin = bad_len_binary(),
            ?assertError(badarg, b64url:decode(Bin))
        end,
        lists:seq(1, ?NUM_TESTS)
    ).

gen_binary() ->
    % -1 to allow for 0 length
    Length = rand:uniform(?MAX_SIZE) - 1,
    crypto:strong_rand_bytes(Length).

shallow_iolist() ->
    to_iolist(gen_binary()).

shallow_b64_iolist() ->
    to_iolist(couch_encode_base64url(gen_binary())).

bad_binary() ->
    insert_error(gen_binary()).

bad_len_binary() ->
    make_bad_len(gen_binary()).

to_iolist(<<>>) ->
    case rand:uniform(2) of
        1 -> <<>>;
        2 -> [<<>>]
    end;
to_iolist(B) when is_binary(B), size(B) > 0 ->
    S = rand:uniform(size(B)),
    <<First:S/binary, Second/binary>> = B,
    case rand:uniform(3) of
        1 ->
            [to_iolist(First), Second];
        2 ->
            [First, to_iolist(Second)];
        3 ->
            [First, Second]
    end.

insert_error(B) when is_binary(B), size(B) < 2 ->
    case rand:uniform(2) of
        1 -> {<<122, 255>>, 0};
        2 -> {<<122, 122, 255>>, 0}
    end;
insert_error(B) when is_binary(B) ->
    B64 = couch_encode_base64url(B),
    S = rand:uniform(size(B64) - 1),
    <<First:S/binary, _:1/binary, Second/binary>> = B64,
    {<<First:S/binary, 255, Second/binary>>, 4 * (S div 4)}.

make_bad_len(Bin) when size(Bin) rem 4 == 1 ->
    Bin;
make_bad_len(Bin) when size(Bin) rem 4 == 2 ->
    <<"AAA", Bin/binary>>;
make_bad_len(Bin) when size(Bin) rem 4 == 3 ->
    <<"AA", Bin/binary>>;
make_bad_len(Bin) when size(Bin) rem 4 == 0 ->
    <<"A", Bin/binary>>.

% These functions are copy/pasted from couch_util to avoid
% the direct dependency. The goal of this project is to replace
% these in couch_util anyway so when that happens they'll only
% exist here for these tests.

couch_encode_base64url(Url) ->
    Url1 = iolist_to_binary(re:replace(base64:encode(Url), "=+$", "")),
    Url2 = iolist_to_binary(re:replace(Url1, "/", "_", [global])),
    iolist_to_binary(re:replace(Url2, "\\+", "-", [global])).

couch_decode_base64url(Url64) ->
    Url1 = re:replace(iolist_to_binary(Url64), "-", "+", [global]),
    Url2 = iolist_to_binary(
        re:replace(iolist_to_binary(Url1), "_", "/", [global])
    ),
    Padding = list_to_binary(lists:duplicate((4 - size(Url2) rem 4) rem 4, $=)),
    base64:decode(<<Url2/binary, Padding/binary>>).
