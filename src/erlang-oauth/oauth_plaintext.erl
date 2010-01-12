-module(oauth_plaintext).

-export([signature/2, verify/3]).


signature(CS, TS) ->
  oauth_uri:calate("&", [CS, TS]).

verify(Signature, CS, TS) ->
  couch_util:verify(signature(CS, TS), Signature).
