-module(oauth).

-export([get/3, get/5, get/6, post/3, post/5, post/6, put/6, put/7, uri/2, header/1,
  sign/6, params_decode/1, token/1, token_secret/1, verify/6]).

-export([plaintext_signature/2, hmac_sha1_signature/5,
  hmac_sha1_signature/3, rsa_sha1_signature/4, rsa_sha1_signature/2,
  signature_base_string/3, params_encode/1]).

-export([plaintext_verify/3, hmac_sha1_verify/6, hmac_sha1_verify/4,
  rsa_sha1_verify/5, rsa_sha1_verify/3]).

-export([header_params_encode/1, header_params_decode/1,
  uri_params_encode/1, uri_params_decode/1]).

-include_lib("public_key/include/public_key.hrl").

get(URL, ExtraParams, Consumer) ->
  get(URL, ExtraParams, Consumer, "", "").

get(URL, ExtraParams, Consumer, Token, TokenSecret) ->
  get(URL, ExtraParams, Consumer, Token, TokenSecret, []).

get(URL, ExtraParams, Consumer, Token, TokenSecret, HttpcOptions) ->
  SignedParams = sign("GET", URL, ExtraParams, Consumer, Token, TokenSecret),
  http_request(get, {uri(URL, SignedParams), []}, HttpcOptions).

post(URL, ExtraParams, Consumer) ->
  post(URL, ExtraParams, Consumer, "", "").

post(URL, ExtraParams, Consumer, Token, TokenSecret) ->
  post(URL, ExtraParams, Consumer, Token, TokenSecret, []).

post(URL, ExtraParams, Consumer, Token, TokenSecret, HttpcOptions) ->
  SignedParams = sign("POST", URL, ExtraParams, Consumer, Token, TokenSecret),
  http_request(post, {URL, [], "application/x-www-form-urlencoded", uri_params_encode(SignedParams)}, HttpcOptions).

put(URL, ExtraParams, {ContentType, Body}, Consumer, Token, TokenSecret) ->
  put(URL, ExtraParams, {ContentType, Body}, Consumer, Token, TokenSecret, []).

put(URL, ExtraParams, {ContentType, Body}, Consumer, Token, TokenSecret, HttpcOptions) ->
  SignedParams = sign("PUT", URL, ExtraParams, Consumer, Token, TokenSecret),
  http_request(put, {uri(URL, SignedParams), [], ContentType, Body}, HttpcOptions).

uri(Base, []) ->
  Base;
uri(Base, Params) ->
  lists:concat([Base, "?", uri_params_encode(Params)]).

header(Params) ->
  {"Authorization", "OAuth " ++ header_params_encode(Params)}.

token(Params) ->
  proplists:get_value("oauth_token", Params).

token_secret(Params) ->
  proplists:get_value("oauth_token_secret", Params).

consumer_key(_Consumer={Key, _, _}) ->
  Key.

consumer_secret(_Consumer={_, Secret, _}) ->
  Secret.

signature_method(_Consumer={_, _, Method}) ->
  Method.

sign(HttpMethod, URL, Params, Consumer, Token, TokenSecret) ->
  SignatureParams = signature_params(Consumer, Params, Token),
  Signature = signature(HttpMethod, URL, SignatureParams, Consumer, TokenSecret),
  [{"oauth_signature", Signature} | SignatureParams].

signature_params(Consumer, Params, "") ->
  signature_params(Consumer, Params);
signature_params(Consumer, Params, Token) ->
  signature_params(Consumer, [{"oauth_token", Token} | Params]).

signature_params(Consumer, Params) ->
  Timestamp = unix_timestamp(),
  Nonce = base64:encode_to_string(crypto:rand_bytes(32)), % cf. ruby-oauth
  [ {"oauth_version", "1.0"}
  , {"oauth_nonce", Nonce}
  , {"oauth_timestamp", integer_to_list(Timestamp)}
  , {"oauth_signature_method", signature_method_string(Consumer)}
  , {"oauth_consumer_key", consumer_key(Consumer)}
  | Params
  ].

verify(Signature, HttpMethod, URL, Params, Consumer, TokenSecret) ->
  case signature_method(Consumer) of
    plaintext ->
      plaintext_verify(Signature, Consumer, TokenSecret);
    hmac_sha1 ->
      hmac_sha1_verify(Signature, HttpMethod, URL, Params, Consumer, TokenSecret);
    rsa_sha1 ->
      rsa_sha1_verify(Signature, HttpMethod, URL, Params, Consumer)
  end.

signature(HttpMethod, URL, Params, Consumer, TokenSecret) ->
  case signature_method(Consumer) of
    plaintext ->
      plaintext_signature(Consumer, TokenSecret);
    hmac_sha1 ->
      hmac_sha1_signature(HttpMethod, URL, Params, Consumer, TokenSecret);
    rsa_sha1 ->
      rsa_sha1_signature(HttpMethod, URL, Params, Consumer)
  end.

signature_method_string(Consumer) ->
  case signature_method(Consumer) of
    plaintext ->
      "PLAINTEXT";
    hmac_sha1 ->
      "HMAC-SHA1";
    rsa_sha1 ->
      "RSA-SHA1"
  end.

plaintext_signature(Consumer, TokenSecret) ->
  uri_join([consumer_secret(Consumer), TokenSecret]).

plaintext_verify(Signature, Consumer, TokenSecret) ->
  verify_in_constant_time(Signature, plaintext_signature(Consumer, TokenSecret)).

hmac_sha1_signature(HttpMethod, URL, Params, Consumer, TokenSecret) ->
  BaseString = signature_base_string(HttpMethod, URL, Params),
  hmac_sha1_signature(BaseString, Consumer, TokenSecret).

hmac_sha1_signature(BaseString, Consumer, TokenSecret) ->
  Key = uri_join([consumer_secret(Consumer), TokenSecret]),
  base64:encode_to_string(crypto:sha_mac(Key, BaseString)).

hmac_sha1_verify(Signature, HttpMethod, URL, Params, Consumer, TokenSecret) ->
  verify_in_constant_time(Signature, hmac_sha1_signature(HttpMethod, URL, Params, Consumer, TokenSecret)).

hmac_sha1_verify(Signature, BaseString, Consumer, TokenSecret) ->
  verify_in_constant_time(Signature, hmac_sha1_signature(BaseString, Consumer, TokenSecret)).

rsa_sha1_signature(HttpMethod, URL, Params, Consumer) ->
  BaseString = signature_base_string(HttpMethod, URL, Params),
  rsa_sha1_signature(BaseString, Consumer).

rsa_sha1_signature(BaseString, Consumer) ->
  Key = read_private_key(consumer_secret(Consumer)),
  base64:encode_to_string(public_key:sign(list_to_binary(BaseString), sha, Key)).

rsa_sha1_verify(Signature, HttpMethod, URL, Params, Consumer) ->
  BaseString = signature_base_string(HttpMethod, URL, Params),
  rsa_sha1_verify(Signature, BaseString, Consumer).

rsa_sha1_verify(Signature, BaseString, Consumer) ->
  Key = read_cert_key(consumer_secret(Consumer)),
  public_key:verify(to_binary(BaseString), sha, base64:decode(Signature), Key).

verify_in_constant_time(<<X/binary>>, <<Y/binary>>) ->
  verify_in_constant_time(binary_to_list(X), binary_to_list(Y));
verify_in_constant_time(X, Y) when is_list(X) and is_list(Y) ->
  case length(X) == length(Y) of
    true ->
      verify_in_constant_time(X, Y, 0);
    false ->
      false
  end.

verify_in_constant_time([X | RestX], [Y | RestY], Result) ->
  verify_in_constant_time(RestX, RestY, (X bxor Y) bor Result);
verify_in_constant_time([], [], Result) ->
  Result == 0.

signature_base_string(HttpMethod, URL, Params) ->
  uri_join([HttpMethod, uri_normalize(URL), params_encode(Params)]).

params_encode(Params) ->
  % cf. http://tools.ietf.org/html/rfc5849#section-3.4.1.3.2
  Encoded = [{uri_encode(K), uri_encode(V)} || {K, V} <- Params],
  Sorted = lists:sort(Encoded),
  Concatenated = [lists:concat([K, "=", V]) || {K, V} <- Sorted],
  string:join(Concatenated, "&").

params_decode(_Response={{_, _, _}, _, Body}) ->
  uri_params_decode(Body).

http_request(Method, Request, Options) ->
  httpc:request(Method, Request, [{autoredirect, false}], Options).

unix_timestamp() ->
  unix_timestamp(calendar:universal_time()).

unix_timestamp(DateTime) ->
  unix_seconds(DateTime) - unix_epoch().

unix_epoch() ->
  unix_seconds({{1970,1,1},{00,00,00}}).

unix_seconds(DateTime) ->
  calendar:datetime_to_gregorian_seconds(DateTime).

read_cert_key(Path) when is_list(Path) ->
  {ok, Contents} = file:read_file(Path),
  [{'Certificate', DerCert, not_encrypted}] = public_key:pem_decode(Contents),
  read_cert_key(public_key:pkix_decode_cert(DerCert, otp));
read_cert_key(#'OTPCertificate'{tbsCertificate=Cert}) ->
  read_cert_key(Cert);
read_cert_key(#'OTPTBSCertificate'{subjectPublicKeyInfo=Info}) ->
  read_cert_key(Info);
read_cert_key(#'OTPSubjectPublicKeyInfo'{subjectPublicKey=Key}) ->
  Key.

read_private_key(Path) ->
  {ok, Contents} = file:read_file(Path),
  [Info] = public_key:pem_decode(Contents),
  public_key:pem_entry_decode(Info).

to_binary(Term) when is_list(Term) ->
  list_to_binary(Term);
to_binary(Term) when is_binary(Term) ->
  Term.

header_params_encode(Params) ->
  intercalate(", ", [lists:concat([uri_encode(K), "=\"", uri_encode(V), "\""]) || {K, V} <- Params]).

header_params_decode(String) ->
  [header_param_decode(Param) || Param <- re:split(String, ",\\s*", [{return, list}]), Param =/= ""].

header_param_decode(Param) ->
  [Key, QuotedValue] = string:tokens(Param, "="),
  Value = string:substr(QuotedValue, 2, length(QuotedValue) - 2),
  {uri_decode(Key), uri_decode(Value)}.

uri_normalize(URI) ->
  case http_uri:parse(URI) of
    {ok, {Scheme, UserInfo, Host, Port, Path, _Query}} -> % R15B
      uri_normalize(Scheme, UserInfo, string:to_lower(Host), Port, [Path]);
    {Scheme, UserInfo, Host, Port, Path, _Query} ->
      uri_normalize(Scheme, UserInfo, string:to_lower(Host), Port, [Path]);
    Else ->
      Else
  end.

uri_normalize(http, UserInfo, Host, 80, Acc) ->
  uri_normalize(http, UserInfo, [Host|Acc]);
uri_normalize(https, UserInfo, Host, 443, Acc) ->
  uri_normalize(https, UserInfo, [Host|Acc]);
uri_normalize(Scheme, UserInfo, Host, Port, Acc) ->
  uri_normalize(Scheme, UserInfo, [Host, ":", Port|Acc]).

uri_normalize(Scheme, [], Acc) ->
  lists:concat([Scheme, "://" | Acc]);
uri_normalize(Scheme, UserInfo, Acc) ->
  lists:concat([Scheme, "://", UserInfo, "@" | Acc]).

uri_params_encode(Params) ->
  intercalate("&", [uri_join([K, V], "=") || {K, V} <- Params]).

uri_params_decode(String) ->
  [uri_param_decode(Substring) || Substring <- string:tokens(String, "&")].

uri_param_decode(String) ->
  [Key, Value] = string:tokens(String, "="),
  {uri_decode(Key), uri_decode(Value)}.

uri_join(Values) ->
  uri_join(Values, "&").

uri_join(Values, Separator) ->
  string:join([uri_encode(Value) || Value <- Values], Separator).

intercalate(Sep, Xs) ->
  lists:concat(intersperse(Sep, Xs)).

intersperse(_, []) ->
  [];
intersperse(_, [X]) ->
  [X];
intersperse(Sep, [X | Xs]) ->
  [X, Sep | intersperse(Sep, Xs)].

uri_encode(Term) when is_integer(Term) ->
  integer_to_list(Term);
uri_encode(Term) when is_atom(Term) ->
  uri_encode(atom_to_list(Term));
uri_encode(Term) when is_list(Term) ->
  uri_encode(lists:reverse(Term, []), []).

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

uri_encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
  uri_encode(T, [X | Acc]);
uri_encode([X | T], Acc) ->
  NewAcc = [$%, dec2hex(X bsr 4), dec2hex(X band 16#0f) | Acc],
  uri_encode(T, NewAcc);
uri_encode([], Acc) ->
  Acc.

uri_decode(Str) when is_list(Str) ->
  uri_decode(Str, []).

uri_decode([$%, A, B | T], Acc) ->
  uri_decode(T, [(hex2dec(A) bsl 4) + hex2dec(B) | Acc]);
uri_decode([X | T], Acc) ->
  uri_decode(T, [X | Acc]);
uri_decode([], Acc) ->
  lists:reverse(Acc, []).

-compile({inline, [{dec2hex, 1}, {hex2dec, 1}]}).

dec2hex(N) when N >= 10 andalso N =< 15 ->
  N + $A - 10;
dec2hex(N) when N >= 0 andalso N =< 9 ->
  N + $0.

hex2dec(C) when C >= $A andalso C =< $F ->
  C - $A + 10;
hex2dec(C) when C >= $0 andalso C =< $9 ->
  C - $0.
