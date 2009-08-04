-module(oauth_plaintext).

-export([signature/2, verify/3]).


signature(CS, TS) ->
  oauth_uri:calate("&", [CS, TS]).

verify(Signature, CS, TS) ->
  Signature =:= signature(CS, TS).
