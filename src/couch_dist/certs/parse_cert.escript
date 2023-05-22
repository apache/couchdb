#!/usr/bin/env escript
-mode(compile).

main(File) ->
    {ok, PemBin} = file:read_file(File),
    [{_, DerCert, _}] = public_key:pem_decode(PemBin),
    OTPCert = public_key:pkix_decode_cert(DerCert, otp),
    io:format("~p~n", [inet_tls_dist:cert_nodes(OTPCert)]).
