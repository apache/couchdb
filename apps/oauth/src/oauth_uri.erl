-module(oauth_uri).

-export([normalize/1, calate/2, encode/1]).
-export([params_from_string/1, params_to_string/1,
  params_from_header_string/1, params_to_header_string/1]).

-import(lists, [concat/1]).

-define(is_uppercase_alpha(C), C >= $A, C =< $Z).
-define(is_lowercase_alpha(C), C >= $a, C =< $z).
-define(is_alpha(C), ?is_uppercase_alpha(C); ?is_lowercase_alpha(C)).
-define(is_digit(C), C >= $0, C =< $9).
-define(is_alphanumeric(C), ?is_alpha(C); ?is_digit(C)).
-define(is_unreserved(C), ?is_alphanumeric(C); C =:= $-; C =:= $_; C =:= $.; C =:= $~).
-define(is_hex(C), ?is_digit(C); C >= $A, C =< $F).


normalize(URI) ->
  case http_uri:parse(URI) of
    {Scheme, UserInfo, Host, Port, Path, _Query} ->
      normalize(Scheme, UserInfo, string:to_lower(Host), Port, [Path]);
    Else ->
      Else
  end.

normalize(http, UserInfo, Host, 80, Acc) ->
  normalize(http, UserInfo, [Host|Acc]);
normalize(https, UserInfo, Host, 443, Acc) ->
  normalize(https, UserInfo, [Host|Acc]);
normalize(Scheme, UserInfo, Host, Port, Acc) ->
  normalize(Scheme, UserInfo, [Host, ":", Port|Acc]).

normalize(Scheme, [], Acc) ->
  concat([Scheme, "://"|Acc]);
normalize(Scheme, UserInfo, Acc) ->
  concat([Scheme, "://", UserInfo, "@"|Acc]).

params_to_header_string(Params) ->
  intercalate(", ", [concat([encode(K), "=\"", encode(V), "\""]) || {K, V} <- Params]).

params_from_header_string(String) ->
  [param_from_header_string(Param) || Param <- re:split(String, ",\\s*", [{return, list}]), Param =/= ""].

param_from_header_string(Param) ->
  [Key, QuotedValue] = string:tokens(Param, "="),
  Value = string:substr(QuotedValue, 2, length(QuotedValue) - 2),
  {decode(Key), decode(Value)}.

params_from_string(Params) ->
  [param_from_string(Param) || Param <- string:tokens(Params, "&")].

param_from_string(Param) ->
  list_to_tuple([decode(Value) || Value <- string:tokens(Param, "=")]).

params_to_string(Params) ->
  intercalate("&", [calate("=", [K, V]) || {K, V} <- Params]).

calate(Sep, Xs) ->
  intercalate(Sep, [encode(X) || X <- Xs]).

intercalate(Sep, Xs) ->
  concat(intersperse(Sep, Xs)).

intersperse(_, []) -> [];
intersperse(_, [X]) -> [X];
intersperse(Sep, [X|Xs]) ->
  [X, Sep|intersperse(Sep, Xs)].

decode(Chars) ->
  decode(Chars, []).

decode([], Decoded) ->
  lists:reverse(Decoded);
decode([$%,A,B|Etc], Decoded) when ?is_hex(A), ?is_hex(B) ->
  decode(Etc, [erlang:list_to_integer([A,B], 16)|Decoded]);
decode([C|Etc], Decoded) when ?is_unreserved(C) ->
  decode(Etc, [C|Decoded]).

encode(Chars) ->
  encode(Chars, []).

encode([], Encoded) ->
  lists:flatten(lists:reverse(Encoded));
encode([C|Etc], Encoded) when ?is_unreserved(C) ->
  encode(Etc, [C|Encoded]);
encode([C|Etc], Encoded) ->
  Value = io_lib:format("%~2.1.0s", [erlang:integer_to_list(C, 16)]),
  encode(Etc, [Value|Encoded]).
