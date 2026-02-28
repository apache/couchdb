#!/bin/bash

# Expects erlperf to be installed
#
# $ git clone https://github.com/max-au/erlperf.git
# $ cd erlperf
# $ rebar3 as prod escriptize
# $ cd ..

for i in 50 100 150 200 500 1000 5000 10000 50000 1000000 10000000; do
 echo ""
 echo "--- bytes: ${i} -----"
 ERL_LIBS="." erlperf/erlperf -w 2 \
  'runner(Bin) -> b64url:encode(Bin).' --label "encode_nif_${i}" \
  'runner(Bin) -> base64:encode(Bin, #{mode => urlsafe, padding => false}).' --label "encode_otp_${i}" \
   --init_runner_all "rand:seed(default,{1,2,3}), rand:bytes(${i})."

 ERL_LIBS="." erlperf/erlperf -w 2 \
  'runner(Enc) -> b64url:decode(Enc).' --label "decode_nif_${i}" \
  'runner(Enc) -> base64:decode(Enc, #{mode => urlsafe, padding => false}).' --label "decode_otp_${i}" \
   --init_runner_all "rand:seed(default,{1,2,3}), b64url:encode(rand:bytes(round(${i} * (3/4))))."
done
