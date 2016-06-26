#!/bin/bash
for t in test/javascript/tests/*.js
do
#  echo $t
  rm -rf dev/lib
  dev/run -n 1 -q --with-admin-party-please test/javascript/run $t
done

