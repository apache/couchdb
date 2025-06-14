#!/bin/bash

set -e
set -o pipefail

DIR=$(dirname -- "$(readlink -f -- "${BASH_SOURCE[0]}")")

echo "+++++++++++++++++++++ Set up test262 repo +++++++++++++++++++++++"
make -C ${DIR}/quickjs test2-bootstrap
echo
echo "++++++++++++++++++++++ Build test runner ++++++++++++++++++++++++"
make -C ${DIR}/quickjs run-test262
echo
echo "++++++++++++++++++++++ Run test262 tests ++++++++++++++++++++++++"
SHELL=/bin/bash make -C ${DIR}/quickjs test2
echo
