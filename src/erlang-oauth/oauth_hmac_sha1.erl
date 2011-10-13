-module(oauth_hmac_sha1).

-export([signature/3, verify/4]).


signature(BaseString, CS, TS) ->
  Key = oauth_uri:calate("&", [CS, TS]),
  base64:encode_to_string(crypto:sha_mac(Key, BaseString)).

verify(Signature, BaseString, CS, TS) ->
  couch_util:verify(Signature, signature(BaseString, CS, TS)).
