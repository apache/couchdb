-module(oauth_uri).

-export([normalize/1, calate/2, encode/1]).
-export([params_from_string/1, params_to_string/1,
  params_from_header_string/1, params_to_header_string/1]).

-import(lists, [concat/1]).


normalize(URI) ->
  case http_uri:parse(URI) of
    {ok, {Scheme, UserInfo, Host, Port, Path, _Query}} -> % R15B
        normalize(Scheme, UserInfo, string:to_lower(Host), Port, [Path]);
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

-define(is_alphanum(C), C >= $A, C =< $Z; C >= $a, C =< $z; C >= $0, C =< $9).

encode(Term) when is_integer(Term) ->
  integer_to_list(Term);
encode(Term) when is_atom(Term) ->
  encode(atom_to_list(Term));
encode(Term) when is_list(Term) ->
  encode(lists:reverse(Term, []), []).

encode([X | T], Acc) when ?is_alphanum(X); X =:= $-; X =:= $_; X =:= $.; X =:= $~ ->
  encode(T, [X | Acc]);
encode([X | T], Acc) ->
  NewAcc = [$%, dec2hex(X bsr 4), dec2hex(X band 16#0f) | Acc],
  encode(T, NewAcc);
encode([], Acc) ->
  Acc.

decode(Str) when is_list(Str) ->
  decode(Str, []).

decode([$%, A, B | T], Acc) ->
  decode(T, [(hex2dec(A) bsl 4) + hex2dec(B) | Acc]);
decode([X | T], Acc) ->
  decode(T, [X | Acc]);
decode([], Acc) ->
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
